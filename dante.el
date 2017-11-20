;;; dante.el --- Development mode for Haskell -*- lexical-binding: t -*-


;; Copyright (c) 2016 Jean-Philippe Bernardy
;; Copyright (c) 2016 Chris Done
;; Copyright (c) 2015 Athur Fayzrakhmanov
;; Copyright (c) 2013 Herbert Valerio Riedel
;; Copyright (c) 2007 Stefan Monnier

;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/dante
;; Created: October 2016
;; Keywords: haskell, tools
;; Package-Requires: ((dash "2.13.0") (emacs "25.1") (f "0.19.0") (flycheck "0.30") (haskell-mode "13.14") (s "1.11.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; DANTE: Do Not Aim To Expand.

;; This is a mode for GHCi advanced "IDE" features.  The mode depends
;; on GHCi only, keeping the logic simple.  Additionally it aims to be
;; minimal as far as possible.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'flycheck)
(require 'haskell-mode)
(require 's)
(require 'xref)

(defmacro dante-cps-bind (vars expr &rest body)
  "Bind VARS in a continuation passed to EXPR with contents BODY.
So (dante-cps-bind x (fun arg) body) expands to (fun arg (λ (x) body))"
  (declare (indent 2))
  (if (listp vars)
      `(,@expr (lambda ,vars ,@body))
  `(,@expr (lambda (,vars) ,@body))))

(defmacro dante-cps-let (bindings &rest body)
"Expand multiple BINDINGS and call BODY as a continuation.
Example: (dante-cps-let ((x (fun1 arg1)) (y z (fun2 arg2))) body)
expands to: (fun1 arg1 (λ (x) (fun2 arg2 (λ (x y) body))))."
  (declare (indent 1))
  (pcase bindings
    (`((,vars ,expr)) `(dante-cps-bind ,vars ,expr ,@body))
    (`((,vars ,expr) . ,rest) `(dante-cps-bind ,vars ,expr (dante-cps-let ,rest ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup dante nil
  "Interactive development mode for Haskell"
  :group 'haskell)

(defcustom dante-debug nil
  "Show debug output."
  :group 'dante
  :type '(set (const inputs) (const outputs) (const responses) (const command-line)))

(defcustom dante-repl-command-line nil
  "Command line to start GHCi, as a list: the executable and its arguments.
When nil, dante will guess the value depending on
`dante-project-root' contents.  This should usually be customized
as a file or directory variable.  Each element of the list is a
sexp which is evaluated to a string before being passed to the
shell."
  :group 'dante
  :type '(repeat sexp))

(defcustom dante-project-root nil
  "The project root, as a string or nil.
When nil, dante will guess the value by looking for a cabal file.
Customize as a file or directory variable."
  :group 'dante
  :type '(choice (const nil) string))

(defcustom dante-target nil
  "The target to demand from cabal repl, as a string or nil.
Customize as a file or directory variable.  Different targets
will be in different GHCi sessions."
  :group 'dante
  :type '(choice (const nil) string))

(defun dante-project-root ()
  "Get the root directory for the project.
If `dante-project-root' is set as a variable, return that,
otherwise look for a .cabal file, or use the current dir."
  (file-name-as-directory
   (or dante-project-root
       (set (make-local-variable 'dante-project-root)
            (file-name-directory (or (dante-cabal-find-file) (dante-buffer-file-name)))))))

(defun dante-repl-by-file (root files cmdline)
  "Return if ROOT / file exists for any file in FILES, return CMDLINE."
  (when (-any? (lambda (file) (file-exists-p (concat root file))) files) cmdline))

(defcustom dante-repl-command-line-methods-alist
  `((styx  . ,(lambda (root) (dante-repl-by-file root '("styx.yaml") '("styx" "repl" dante-target))))
    (nix   . ,(lambda (root) (dante-repl-by-file root '("shell.nix" "default.nix")
                                                      '("nix-shell" "--run" (if dante-target (concat "cabal repl " dante-target) "cabal repl")))))
    (stack . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target))))
    (mafia . ,(lambda (root) (dante-repl-by-file root '("mafia") '("mafia" "repl" dante-target))))
    (new-build . ,(lambda (root)
                    (when (or (directory-files root nil ".*\\.cabal$")
                              (file-exists-p "cabal.project"))
                      '("cabal" "new-repl" dante-target))))
    (bare  . ,(lambda (_) '("cabal" "repl" dante-target))))
"GHCi launch command lines.
This is an alist from method name to a function taking the root
directory and returning either a command line or nil if the
method should not apply.  The first non-nil result will be used as
a command line.  Customize this if you do not want certain methods
to be used by default by dante.  If you want a specific
configuration for your project, customize
`dante-repl-command-line' directly, f as a directory-local
variable."
  :type '(alist :key-type symbol :value-type function))

(defvar dante-command-line)

(defun dante-repl-command-line ()
  "Return the command line for running GHCi.
If the custom variable `dante-repl-command-line' is non-nil, it
will be returned.  Otherwise, use
`dante-repl-command-line-methods-alist'."
  (or dante-repl-command-line
      (let ((root (dante-project-root)))
            (--first it (--map (funcall (cdr it) root)
                               dante-repl-command-line-methods-alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar dante-mode-map (make-sparse-keymap) "Dante minor mode's map.")

(defun dante-status ()
  "Return dante's status for the current source buffer."
  (pcase (dante-get-var 'dante-state)
    ('started (if (dante-get-var 'dante-callback) (format "busy(%s)" (1+ (length (dante-get-var 'dante-queue))))
                (dante-get-var 'dante-loaded-modules)))
    (state (symbol-name state))))

;;;###autoload
(define-minor-mode dante-mode
  "Minor mode for Dante.

`dante-mode' takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles dante.
A prefix argument enables dante if the argument is positive,
and disables it otherwise.

When called from Lisp, the `dante-mode' toggles dante if the
argument is `toggle', disables dante if the argument is a
non-positive integer, and enables dante otherwise (including
if the argument is omitted or nil or a positive integer).

\\{dante-mode-map}"
  :lighter (:eval (concat " Danté:" (dante-status)))
  :keymap dante-mode-map
  :group dante
  (if dante-mode
      (progn (flycheck-select-checker 'haskell-dante))
      (progn (flycheck-select-checker nil))))

(define-key dante-mode-map (kbd "C-c .") 'dante-type-at)
(define-key dante-mode-map (kbd "C-c ,") 'dante-info)
(define-key dante-mode-map (kbd "C-c /") 'dante-auto-fix)
(define-key dante-mode-map (kbd "C-c \"") 'dante-eval-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local dante-loaded-modules "" "Loaded modules as a string, reported by GHCi")
(defvar-local dante-queue nil "List of ready GHCi queries.")
(defvar-local dante-callback nil "Callback waiting for output.")
(defvar-local dante-package-name nil "The package name associated with the current buffer.")
(defvar-local dante-state nil
  "nil: initial state
- starting: GHCi starting
- started: GHCi started
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own. Do not try restarting
automatically. The user will have to manually run `dante-restart'
to destroy the buffer and create a fresh one without this variable enabled.")

(defun dante-get-var (symbol)
  "Return the value of SYMBOL in the GHCi process buffer."
  (let ((bp (dante-buffer-p)))
    (when bp (buffer-local-value symbol bp))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive utils

(defun dante-list-buffers ()
  "List hidden process buffers created by dante.
You can use this to kill them or look inside."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (buffer)
                    (string-match " dante:" (buffer-name buffer)))
                  (buffer-list))))
    (if buffers
        (display-buffer
         (list-buffers-noselect
          nil
          buffers))
      (error "There are no Dante process buffers"))))

(defun dante-fontify-expression (expression)
  "Return a haskell-fontified version of EXPRESSION.
If `haskell-mode' is loaded, just return EXPRESSION."
  (if (fboundp 'haskell-mode)
      (with-temp-buffer
        (let ((haskell-mode-hook nil)) ;; to keep switching mode cheap
          (haskell-mode)
          (insert expression)
          (font-lock-ensure)
          (buffer-string)))
    expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type and info at point

(defun dante-type-at (insert)
  "Get the type of the thing or selection at point.
When the universal argument INSERT is non-nil, insert the type in the buffer."
  (interactive "P")
  (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
    (dante-cps-let (((done _load-messages) (dante-async-load-current-buffer nil))
                    (ty (dante-async-call (concat ":type-at " tap))))
      (funcall done)
      (if insert (save-excursion (goto-char (line-beginning-position))
                                 (insert (dante-fontify-expression ty) "\n"))
                 (message "%s" (dante-fontify-expression ty))))))

(defun dante-info (ident)
  "Get the info about the IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (let ((package (dante-package-name))
        (help-xref-following nil)
        (origin (buffer-name)))
    (dante-cps-let (((done _load-message) (dante-async-load-current-buffer t))
                    (info (dante-async-call (format ":i %s" ident))))
      (funcall done)
      (help-setup-xref (list #'dante-call-in-buffer (current-buffer) #'dante-info ident)
                       (called-interactively-p 'interactive))
      (save-excursion
        (let ((help-xref-following nil))
          (with-help-window (help-buffer)
            (with-current-buffer (help-buffer)
              (insert
               (dante-fontify-expression ident)
               " in `" origin "'" " (" package ")"
               "\n\n"
               (dante-fontify-expression info))
              (goto-char (point-min)))))))))

;;;;;;;;;;;;;;;;;;;;;
;; Flycheck checker

(defvar-local dante-loaded-file "<DANTE:NO-FILE-LOADED>")
(defvar-local dante-loaded-interpreted nil)

(defun dante-async-load-current-buffer (interpret cont)
  "Load and maybe INTERPRET the temp file for current buffer and run CONT in a session.
The continuation must call its first argument; see `dante-session'."
  (let ((fname (dante-local-name (dante-temp-file))))
    (dante-cps-let (((buffer done) (dante-session))
                    (_ (dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code")))
                    (load-message
                     (dante-async-call
                      (if (string-equal (buffer-local-value 'dante-loaded-file buffer) fname)
                          ":r" (concat ":l *" fname)))))
      (with-current-buffer buffer (setq dante-loaded-interpreted interpret))
      (funcall cont done load-message))))

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-get-var 'dante-state) 'dead)
      (run-with-timer 0 nil cont 'interrupted)
    (dante-cps-let (((done string) (dante-async-load-current-buffer nil)))
      (funcall done)
      (let ((msgs (dante-parse-errors-warnings-splices
                   checker
                   (current-buffer)
                   string)))
        (funcall cont
                 'finished
                 (cl-remove-if (lambda (msg)
                                 (eq 'splice (flycheck-error-level msg)))
                               msgs))))))

(flycheck-define-generic-checker 'haskell-dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :modes '(haskell-mode literate-haskell-mode))

;;;###autoload
(defun flycheck-dante-setup ()
  "Setup Flycheck Dante.

Add `haskell-dante' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'haskell-dante))

(defun dante-parse-errors-warnings-splices (checker buffer string)
  "Parse flycheck errors and warnings.
CHECKER and BUFFER are added to each item parsed from STRING."
  (let ((messages (list))
        (temp-file (dante-local-name (dante-temp-file-name buffer))))
    (while (string-match
            (concat "[\n]\\([A-Z]?:?[^ \n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                    "[ \n]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
            string)
      (let* ((file (dante-canonicalize-path (match-string 1 string)))
             (location-raw (match-string 2 string))
             (msg (match-string 3 string)) ;; Replace gross bullet points.
             (s (substring string (1+ (match-end 0))))
             (type (cond ((string-match "^Warning:" msg)
                          (setq msg (replace-regexp-in-string "^Warning: *" "" msg))
                          (if (string-match "^\\[-Wdeferred-type-errors\\]" msg)
                              'error
                            'warning))
                         ((string-match "^Splicing " msg) 'splice)
                         (t                               'error)))
             (location (dante-parse-error
                        (concat file ":" location-raw ": x")))
             (line (plist-get location :line))
             (column (plist-get location :col)))
        (setq string s)
        (push (flycheck-error-new-at line column type msg
                                     :checker checker
                                     :buffer (when (string= temp-file file) buffer)
                                     ;; TODO: report external errors somehow.
                                     :filename (dante-buffer-file-name buffer))
              messages)))
    messages))

(defconst dante-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6: Warning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.")

(defun dante-parse-error (string)
  "Parse the line number from the error in STRING."
  (let ((span nil))
    (cl-loop for regex
             in dante-error-regexp-alist
             do (when (string-match (car regex) string)
                  (setq span
                        (list :file (match-string 1 string)
                              :line (string-to-number (match-string 2 string))
                              :col (string-to-number (match-string 4 string))
                              :line2 (when (match-string 3 string)
                                       (string-to-number (match-string 3 string)))
                              :col2 (when (match-string 5 string)
                                      (string-to-number (match-string 5 string)))))))
    span))

(defun dante-call-in-buffer (buffer func &rest args)
  "In BUFFER, call FUNC with ARGS."
  (with-current-buffer buffer (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(defun dante-company (command &optional _arg &rest _ignored)
  "Company backend for dante.
See ``company-backends'' for the meaning of COMMAND and _ARGS."
  (let ((prefix (when (and dante-mode (dante-ident-pos-at-point))
                  (let* ((id-start (car (dante-ident-pos-at-point)))
                         (import-found (save-excursion (re-search-backward "import[\t ]*" (line-beginning-position) t)))
                         (import-end (match-end 0))
                         (import-start (match-beginning 0))
                         (is-import (eq import-end id-start)))
                    ;; (message "found %s end %s start %s id-start %s" import-found import-end import-start id-start)
                    (buffer-substring-no-properties (if is-import import-start id-start) (point)))))) ;; todo: pref len
    (cl-case command
      (interactive (company-begin-backend 'dante-company))
      (sorted t)
      (prefix prefix)
      (candidates
       (unless (eq (dante-get-var 'dante-state) 'dead)
         (cons :async
               (lambda (ret)
                 (dante-cps-let (((done _load-messages) (dante-async-load-current-buffer nil))
                                 (reply (dante-async-call (format ":complete repl %S" prefix))))
                   (funcall done)
                   (let* ((lines (s-lines reply))
                          (common (nth 2 (read (concat "(" (car lines) ")")))))
                     (funcall ret (--map (replace-regexp-in-string "\\\"" "" (concat common it)) (cdr lines))))))))))))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'dante-company))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source buffer operations

(defun dante-thing-at-point ()
  "Return (list START END) the indent at point, or the region if it is active."
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (dante-ident-pos-at-point)))

(defun dante-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (dante-ident-pos-at-point)))
    (when reg
      (apply #'buffer-substring-no-properties reg))))

(defun dante-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    (let ((case-fold-search nil))
      (cl-multiple-value-bind (start end)
          (list
           (progn (skip-syntax-backward "w_") (point))
           (progn (skip-syntax-forward "w_") (point)))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (unless (= start end)
          (list start end))))))

(defun dante-buffer-file-name (&optional buffer)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (let ((name (buffer-file-name buffer)))
    (when name
      (dante-canonicalize-path (substring-no-properties name)))))

(defvar-local dante-temp-file-name nil
  "The name of a temporary file to which the current buffer's content is copied.")

(defun dante-tramp-make-tramp-temp-file (buffer)
  "Create a temporary file for BUFFER, perhaps on a remote host."
  (let* ((fname (buffer-file-name buffer))
         (suffix (file-name-extension fname t)))
    (if (file-remote-p fname)
        (with-parsed-tramp-file-name (buffer-file-name buffer) vec
          (let ((prefix (concat
                         (expand-file-name
                          tramp-temp-name-prefix (tramp-get-remote-tmpdir vec))
                         "dante"))
                result)
            (while (not result)
              ;; `make-temp-file' would be the natural choice for
              ;; implementation.  But it calls `write-region' internally,
              ;; which also needs a temporary file - we would end in an
              ;; infinite loop.
              (setq result (concat (make-temp-name prefix) suffix))
              (if (file-exists-p result)
                  (setq result nil)
                ;; This creates the file by side effect.
                (set-file-times result)
                (set-file-modes result (tramp-compat-octal-to-decimal "0700"))))
            result))
      (make-temp-file "dante" nil suffix))))

(defun dante-local-name (fname)
  "Local name of FNAME on the remote host."
  (string-remove-prefix (or (file-remote-p fname) "") fname))

(defun dante-temp-file-name (buffer)
  "Return a (possibly remote) filename suitable to store BUFFER's contents."
  (with-current-buffer buffer
    (or dante-temp-file-name (setq dante-temp-file-name (dante-tramp-make-tramp-temp-file buffer)))))

(defvar-local dante-temp-epoch -1
  "The value of `buffer-modified-tick' when the contents were
  last written to `dante-temp-file-name'.")

(defun dante-temp-file (&optional buffer)
  "Create a temp file with an up-to-date copy of BUFFER's contents and return its name."
  (with-current-buffer (or buffer (current-buffer))
    (let ((epoch (buffer-modified-tick)))
      (unless (equal epoch dante-temp-epoch) ;; so ghci's :r may be noop
        (setq dante-temp-epoch epoch)
        (write-region nil nil (dante-temp-file-name (current-buffer)) nil 0)))
    (dante-temp-file-name (current-buffer))))

(defun dante-canonicalize-path (path)
  "Return a standardized version of PATH.
Path names are standardized and drive names are
capitalized (relevant on Windows)."
  (dante-capitalize-drive-letter (convert-standard-filename path)))

(defun dante-capitalize-drive-letter (path)
  "Ensures the drive letter is capitalized in PATH.
This applies to paths of the form
x:\\foo\\bar (i.e., Windows)."
  (save-match-data
    (let ((drive-path (split-string path ":\\\\")))
      (if (or (null (car drive-path)) (null (cdr drive-path)))
          path
        (concat (upcase (car drive-path)) ":\\" (cadr drive-path))))))

;;;;;;;;;;;;;;;;;;;;
;; GHCi formatting

(defun dante--ghc-column-number-at-pos (pos)
  "Format the point POS as a column number as expected by GHCi."
  (1+ (save-excursion (goto-char pos) (current-column))))

(defun dante--ghc-subexp (reg)
  "Format the subexpression denoted by REG for GHCi commands."
  (pcase reg (`(,beg . (,end . nil))
              (format "%S %d %d %d %d %s"
                      (dante-temp-file-name (current-buffer))
                      (line-number-at-pos beg)
                      (dante--ghc-column-number-at-pos beg)
                      (line-number-at-pos end)
                      (dante--ghc-column-number-at-pos end)
                      (buffer-substring-no-properties beg end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GHCi process communication

(defun dante-destroy ()
  "Stop GHCi and kill its associated process buffer."
  (interactive)
  (when (dante-buffer-p)
    (dante-set-state 'deleting)
    (with-current-buffer (dante-buffer-p)
      (when (get-buffer-process (current-buffer))
        (kill-process (get-buffer-process (current-buffer)))
        (delete-process (get-buffer-process (current-buffer))))
      (kill-buffer (current-buffer)))))

(defun blocking-call (cont)
  "Call CONT as (CONT K) and block until (K res) is called, then return res."
  (let ((result nil))
    (funcall cont (lambda (reply) (setq result (list reply))))
    ;; use a list so that even 'nil' will be detected as a result.
    (while (not result) (sleep-for 0.01))
    (car result)))

(defun dante-restart ()
  "Restart GHCi with the same configuration as before."
  (interactive)
  (when (dante-buffer-p)
    (dante-destroy)
    (dante-cps-let (((_buffer done) (dante-session))) (funcall done))))

(defun dante-session (cont)
  "Run the CONT in a valid GHCi session for the current (source) buffer.
CONT is called as (CONT process-buffer done).  CONT must call
done when it is done sending commands.  (Only by calling done can
other sub-sessions start running.)"
  (let ((source-buffer (current-buffer))
        (buffer (or (dante-buffer-p) (dante-start))))
    (with-current-buffer buffer (push (list :func cont :source-buffer source-buffer) dante-queue))
    ;; TODO: test process-live-p
    (dante-schedule-next buffer)))

(defun dante-start ()
  "Start a GHCi worker and return its buffer."
  (let* ((args (-non-nil (-map #'eval (dante-repl-command-line))))
         (buffer (dante-buffer-create))
         (process (with-current-buffer buffer
                    (when (memq 'command-line dante-debug)
                      (message "GHCi command line: %s" (combine-and-quote-strings args)))
                    (message "Dante: Starting GHCi ...")
                    (apply #'start-file-process "dante" buffer args))))
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer buffer
        (erase-buffer)
        (setq-local dante-command-line (process-command process)))
      (dante-set-state 'starting)
      (dante-cps-let
          ((_start-messages (dante-async-call
                             (concat ":set -Wall\n" ;; TODO: configure
                                     ":set +c\n" ;; collect type info
                                     ":set prompt \"\\4%s|\""))))
        (dante-set-state 'started)
        (message "GHCi started!"))
      (set-process-filter
       process
       (lambda (process string)
         (when (memq 'inputs dante-debug)
           (message "[Dante] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string)
             (dante-report-ghci-progress)
             (dante-read-buffer)))))
      (set-process-sentinel process 'dante-sentinel)
      buffer))

(defun dante-report-ghci-progress ()
  "In the dante buffer, look for GHCi process and inform the user."
  (goto-char (point-max))
  (when (search-backward "\n" nil t 2)
    (forward-char)
    (let ((message
          (cond ((search-forward-regexp "\\[\\([0-9]*\\) of \\([0-9]*\\)\\] Compiling \\([^ ]*\\)" nil t)
                 (format "%s/%s(%s)" (match-string 1) (match-string 2) (match-string 3))))))
    (when message (message "GHCi: %s" message)))))

(defun dante-async (cont)
  "Install CONT as a callback for GHCi output.
Called in process buffer."
    (when dante-callback
      (error "Try to set a callback (%s), but one exists already! (%s)" cont dante-callback))
    (setq dante-callback cont))

(defun dante-async-call (cmd cont)
  "Send GHCi the command string CMD.
The result is passed to CONT as (CONT REPLY).  Can only be called
from a valid session."
  (when (memq 'outputs dante-debug) (message "[Dante] -> %s" cmd))
  (let ((source-marker (point-marker)))
    (with-current-buffer (dante-buffer-p)
      (process-send-string (get-buffer-process (current-buffer)) (concat cmd "\n"))
      (dante-async (apply-partially #'dante-wait-for-prompt source-marker cmd "" cont)))))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (let ((buffer (process-buffer process)))
    (when (and (buffer-live-p buffer) (not (process-live-p process)))
      (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
          (message "GHCi process deleted.")
        (with-current-buffer buffer (setq dante-state 'dead))
        (dante-show-process-problem process change)))))

(defun dante-debug-info (buffer)
  "Show debug info for dante buffer BUFFER."
  (if buffer
      (with-current-buffer buffer
        (format "Directory:%s\n Command line:%s\n Loaded: %s\n State: %s\n Queue: %s\n Callback: %s\n"
                default-directory dante-command-line dante-loaded-modules dante-state dante-queue dante-callback))
    (format "Dante not started in %s" buffer)))

(defun dante-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Problem with GHCi process!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat "This is where GHCi output is bufferized. This buffer is normally hidden,
but a problem occcured.

EXTRA TROUBLESHOOTING INFO

The GHCi process ended. Here is the reason that Emacs gives us: " change "
" (dante-debug-info (current-buffer)) "

WHAT TO DO NEXT

Try to customize (probably file-locally or directory-locally)
`dante-project-root' and/or `dante-repl-command-line'.  If you
fixed the problem, just kill this buffer, Dante will make a fresh
one and attempt to restart GHCi automatically.

If you are unable to fix the problem, just leave this buffer
around and Dante will not attempt to restart GHCi.

You can always run M-x dante-restart to make it try again.
")
    'face 'compilation-error)))

(defun dante-wait-for-prompt (source-marker cmd acc cont s-in)
  "Loop waiting for a GHCi prompt.
Text from S-IN is ACC umulated.  CONT eventually is called with
all concatenated text, after jumping back to SOURCE-MARKER. (CMD
is remembered for debug.)"
  (let ((s (concat acc s-in)))
    (if (string-match "\4\\(.*\\)|" s)
        (progn
          (setq dante-loaded-modules (match-string 1 s))
          (let ((string (dante--kill-last-newline (substring s 0 (1- (match-beginning 1))))))
            (when (memq 'responses dante-debug) (message "GHCi <= %s\n     => %s" cmd string))
            (with-current-buffer (marker-buffer source-marker) (save-excursion (goto-char source-marker) (funcall cont string)))))
      (dante-async (apply-partially #'dante-wait-for-prompt source-marker cmd s cont)))))


(defun dante-read-buffer ()
  "Process GHCi output."
  (let ((callback dante-callback)
        (string (dante--strip-carriage-returns (buffer-string))))
    (unless dante-callback (error "Received output in %s (%s) but no callback is installed" (current-buffer) string))
    (delete-region (point-min) (point-max))
    (setq dante-callback nil)
    (funcall callback string)))

(defun dante-schedule-next (buffer)
  "If GHCi is idle, run the next queued GHCi sub-session for BUFFER, if any.
Note that sub-sessions are not interleaved."
  (with-current-buffer buffer
    (unless dante-callback
      (let ((req (pop dante-queue)))
        (force-mode-line-update)
        (when req
          (with-current-buffer (plist-get req :source-buffer)
            (funcall (plist-get req :func) buffer
                     (apply-partially #'dante-schedule-next buffer))))))))

(defun dante--strip-carriage-returns (string)
  "Return the STRING stripped of its \\r occurences."
  (replace-regexp-in-string "\r" "" string))

(defun dante--kill-last-newline (string)
  "Strip the last newline character in STRING."
  (replace-regexp-in-string "\n$" "" string))

(defun dante-buffer-name ()
  "Create a dante process buffer name."
  (let* ((root (dante-project-root))
         (package-name (dante-package-name)))
    (concat " dante:" package-name ":" dante-target ":" root)))

(defun dante-buffer-create ()
  "Create the buffer for GHCi."
  (let* ((root (dante-project-root)))
    (with-current-buffer (get-buffer-create (dante-buffer-name))
      (cd root)
      (fundamental-mode) ;; note: this has several effects, including resetting the local variables
      (current-buffer))))

(defun dante-set-state (state)
  "Set the dante-state to STATE and redisplay the modeline."
  (with-current-buffer (dante-buffer-p) (setq-local dante-state state))
  (force-mode-line-update))

(defun dante-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (dante-buffer-name)))

(defun dante-package-name (&optional cabal-file)
  "Get the current package name from a nearby .cabal file.
If there is none, return an empty string.  If specified, use
CABAL-FILE rather than trying to locate one."
  (or dante-package-name
      (setq dante-package-name
            (let ((cabal-file (or cabal-file
                                  (dante-cabal-find-file))))
              (if cabal-file
                  (replace-regexp-in-string
                   ".cabal$" ""
                   (file-name-nondirectory cabal-file))
                "")))))

(defun dante-cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses `dante-cabal-find-pkg-desc' internally."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((cabal-file (dante-cabal-find-pkg-desc root)))
              (when cabal-file
                (throw 'found cabal-file)))
            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun dante-cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref support

(defun dante--xref-backend () "Dante xref backend." (when dante-mode 'dante))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dante)))
  (dante--ghc-subexp (dante-ident-pos-at-point)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dante)))
  nil)

(defun dante-expand-filename (filename)
  "Prepend FILENAME with the dante running directory."
  (concat (with-current-buffer (dante-buffer-p) default-directory) filename))

(defun dante--match-src-span (string)
  "Extract a location from a ghc span STRING."
  ;; On external symbols, GHC may return a location such as integer-gmp-1.0.0.1:integer-gmp-1.0.0.1:GHC.Integer.Type
  (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$" string)
    (let ((file (match-string 1 string))
          (line (string-to-number (match-string 2 string)))
          (col (string-to-number (match-string 3 string))))
      (xref-make-file-location
       (if (string= file (dante-temp-file-name (current-buffer)))
           (buffer-file-name)
         (expand-file-name file dante-project-root))
       line (1- col)))))

(defun dante--summarize-src-spans (spans file)
  "Add summary strings to a list of source SPANS in FILE."
  (if (not (and file (file-readable-p file)))
      (--map (xref-make "<unreadable>" it) spans)
    (let* ((lines (s-lines (f-read file)))
           (wanted (--map (1- (oref it line)) spans))
           (lines (-select-by-indices wanted lines)))
      (-zip-with #'xref-make lines spans))))

(defun dante--make-xrefs (string)
  "Make xref objects for the source spans in STRING."
  (--mapcat (funcall #'dante--summarize-src-spans (cdr it) (car it))
            (--group-by (oref it file) (-non-nil (-map #'dante--match-src-span
                                                       (s-lines string))))))

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (dante-cps-let ((ret (blocking-call))
                  ((done _load-messages) (dante-async-load-current-buffer nil))
                  (target (dante-async-call (concat ":loc-at " symbol))))
    (funcall done)
    (let ((xrefs (dante--make-xrefs target)))
      (funcall ret xrefs))))

(cl-defmethod xref-backend-references ((_backend (eql dante)) symbol)
  (dante-cps-let ((ret (blocking-call))
                  ((done _load-messages) (dante-async-load-current-buffer nil))
                  (result (dante-async-call (concat ":uses " symbol))))
    (funcall done)
    (let ((xrefs (dante--make-xrefs result)))
      (funcall ret xrefs))))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-fix

(defcustom dante-suggestible-extensions
  '("AllowAmbiguousTypes" "BangPatterns" "ConstraintKinds" "DataKinds" "DeriveFoldable" "DeriveFunctor" "DeriveGeneric" "DeriveTraversable" "EmptyCase" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GADTs" "GeneralizedNewtypeDeriving" "InstanceSigs" "KindSignatures" "MultiParamTypeClasses" "PartialTypeSignatures" "PatternSynonyms" "PolyKinds" "RankNTypes" "RecordWildCards" "ScopedTypeVariables" "StandaloneDeriving" "TupleSections" "TypeApplications" "TypeFamilies" "TypeInType" "TypeOperators" "TypeSynonymInstances" "UndecidableSuperClasses" "UndecidableInstances" "ViewPatterns")
  "Language extensions that Dante can use to fix errors."
  :group 'dante
  :type '(repeat string))

(defun dante-auto-fix (pos)
  "Attempt to fix the flycheck error or warning at POS."
  (interactive "d")
  (let ((messages (delq nil (mapcar #'flycheck-error-message
                                    (flycheck-overlay-errors-at pos)))))
    (when messages
      (let ((msg (car messages)))
        (save-excursion
          (cond ;; use (set-selective-display 12) to see an outline of all possible matches
           ((string-match "Redundant constraints?: (?\\([^,)\n]*\\)" msg)
            (let ((constraint (match-string 1 msg)))
              (search-forward constraint) ; find type sig
              (delete-region (match-beginning 0) (match-end 0))
              (when (looking-at "[ \t]*,")
                (delete-region (point) (search-forward ",")))
              (when (looking-at "[ \t]*=>")
                (delete-region (point) (search-forward "=>")))))
           ((string-match "The type signature for ‘\\(.*\\)’[ \t\n]*lacks an accompanying binding" msg)
            (beginning-of-line)
            (forward-line)
            (insert (concat (match-string 1 msg) " = _\n")))
           ((string-match "add (\\(.*\\)) to the context of[\n ]*the type signature for:[ \n]*\\([^ ]*\\) ::" msg)
            (let ((missing-constraint (match-string 1 msg))
                  (function-name (match-string 2 msg)))
              (search-backward-regexp (concat (regexp-quote function-name) "[ \t]*::[ \t]*" )) ; find type sig
              (goto-char (match-end 0))
              (when (looking-at "forall\\|∀") ; skip quantifiers
                (search-forward "."))
              (skip-chars-forward "\n\t ") ; skip spaces
              (insert (concat missing-constraint " => "))))
           ((string-match "Unticked promoted constructor" msg)
            (goto-char (car (dante-ident-pos-at-point)))
            (insert "'"))
           ((string-match "Patterns not matched:" msg)
            (let ((patterns (mapcar #'string-trim (split-string (substring msg (match-end 0)) "\n" t " ")))) ;; patterns to match
            (if (string-match "In an equation for ‘\\(.*\\)’:" msg)
                (let ((function-name (match-string 1 msg)))
                  (end-of-line)
                  (dolist (pattern patterns)
                    (insert (concat "\n" function-name " " pattern " = _"))))
              (end-of-line) ;; assuming that the case expression is on multiple lines and that "of" is at the end of the line
              (dolist (pattern patterns)
                (haskell-indentation-newline-and-indent)
                (insert (concat pattern " -> _"))))))
           ((string-match "A do-notation statement discarded a result of type" msg)
            (goto-char (car (dante-ident-pos-at-point)))
            (insert "_ <- "))
           ((string-match "Failed to load interface for ‘\\(.*\\)’\n[ ]*Perhaps you meant[ \n]*\\([^ ]*\\)" msg)
            (let ((replacement (match-string 2 msg)))
              ;; ^^ delete-region may garble the matches
              (search-forward (match-string 1 msg))
              (delete-region (match-beginning 0) (point))
              (insert replacement)))
           ((string-match "Perhaps you want to add ‘\\(.*\\)’ to the import list[\n\t ]+in the import of[ \n\t]*‘.*’[\n\t ]+([^:]*:\\([0-9]*\\):[0-9]*-\\([0-9]*\\))" msg)
            (let ((missing (match-string 1 msg))
                  (line (string-to-number (match-string 2 msg)))
                  (end-col (string-to-number (match-string 3 msg))))
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column (1- end-col))
              (skip-chars-backward " \t")
              (unless (looking-back "(" (- (point) 2)) (insert ","))
              (insert missing)))
           ((string-match "Perhaps you meant ‘\\([^‘]*\\)’" msg)
            (let ((replacement (match-string 1 msg)))
              ;; ^^ delete-region may garble the matches
              (apply #'delete-region (dante-ident-pos-at-point))
              (insert replacement)))
           ((string-match "Perhaps you meant one of these:" msg)
            (let* ((replacements
                    (-map (lambda (r)
                            (string-match "‘\\(.*\\)’ (line [0-9]*)" r)
                            (match-string 1 r))
                          (split-string (substring msg (match-end 0)) "," t " ")))
                   (replacement (completing-read "replacement: " replacements)))
              (apply #'delete-region (dante-ident-pos-at-point))
              (insert replacement)))
           ((string-match "\\(Top-level binding\\|Pattern synonym\\) with no type signature:[\n ]*" msg)
            (beginning-of-line)
            (insert (concat (substring msg (match-end 0)) "\n")))
           ((string-match "Defined but not used" msg)
            (goto-char (car (dante-ident-pos-at-point)))
            (insert "_"))
           ((string-match "Unused quantified type variable ‘\\(.*\\)’" msg)
            ;; note there can be a kind annotation, not just a variable.
            (delete-region (point) (+ (point) (- (match-end 1) (match-beginning 1)))))
           ((string-match "The import of ‘[^’]*’ is redundant" msg)
            (beginning-of-line)
            (delete-region (point) (progn (next-logical-line) (point))))
           ((string-match "The import of ‘\\(.*\\)’ from ‘[^’]*’ is redundant" msg)
            (let ((redundant (match-string 1 msg)))
              (search-forward redundant)))
           ((string-match "Found type wildcard ‘.*’[ \t\n]*standing for ‘\\(.*\\)’" msg)
            (let ((type-expr (match-string 1 msg)))
            (apply #'delete-region (dante-ident-pos-at-point))
            (insert type-expr)))
           ((--any? (string-match it msg) dante-suggestible-extensions)
            (goto-char 1)
            (insert (concat "{-# LANGUAGE " (car (--filter (string-match it msg) dante-suggestible-extensions)) " #-}\n")))
           (t (error "Cannot fix the issue at point automatically")))
          (when (looking-back "[ \t]" (line-beginning-position))
            (delete-region (point) (+ (point) (skip-chars-forward " \t")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reploid

(defun dante-eval-loop (done block-end)
  "Evaluation loop iteration.
Calls DONE when done.  BLOCK-END is a marker for the end of the evaluation block."
  (while (and (looking-at "[ \t]*--")
              (not (looking-at "[ \t]*--[ \t]+>>>")))
    (forward-line))
  (if (not (search-forward-regexp "[ \t]*--[ \t]+>>>" (line-end-position) t 1))
      (funcall done)
    ;; found the next command; execute it and replace the result.
    (dante-cps-let ((res (dante-async-call (buffer-substring-no-properties (point) (line-end-position)))))
      (beginning-of-line)
      (forward-line)
      (save-excursion
        (delete-region (point)
                       ;; look for: empty comment line, next command or end of block.
                       (or (and (search-forward-regexp "[ \t]*--[ \t]*\\([ \t]>>>\\|$\\)" block-end t 1)
                                (match-beginning 0))
                           block-end)))
      (insert (apply 'concat (--map (concat "-- " it "\n") (--filter (not (s-blank? it)) (s-lines res)))))
      (beginning-of-line)
      (dante-eval-loop done block-end))))

(defun dante-eval-block ()
  "Evaluate the expression command(s) found after in the current command block >>> and insert the results."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((block-end (save-excursion (while (looking-at "[ \t]*--") (forward-line)) (point-marker))))
      (while (looking-at "[ \t]*--") (forward-line -1))
      (forward-line)
      (dante-cps-let (((done _load-messages) (dante-async-load-current-buffer t)))
        (dante-eval-loop done block-end)))))

(provide 'dante)

;;; dante.el ends here
