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
expands to: (fun1 arg1 (λ (x) (fun2 arg2 (λ (y z) body))))."
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
    (new-build . ,(lambda (root) (when (or (directory-files root nil ".*\\.cabal$") (file-exists-p "cabal.project"))
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

(defvar dante-command-line "command line used to start GHCi")

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
  (s-join ":"
   (-non-nil (list (format "%s" (dante-get-var 'dante-state))
                   (when (dante-get-var 'dante-callback)
                     (format "busy(%s)" (1+ (length (dante-get-var 'dante-queue)))))))))

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
;; Session-local variables. These are set *IN THE GHCi INTERACTION BUFFER*

(defvar-local dante-load-message nil "load messages")
(defvar-local dante-loaded-file "<DANTE:NO-FILE-LOADED>")
(defvar-local dante-queue nil "List of ready GHCi queries.")
(defvar-local dante-callback nil "Callback waiting for output.")
(defvar-local dante-package-name nil "The package name associated with the current buffer.")
(defvar-local dante-state nil
  "nil: initial state
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own. Do not try restarting
automatically. The user will have to manually run `dante-restart'
to destroy the buffer and create a fresh one without this variable enabled.
- other value: informative value for the user about what GHCi is doing
")

(defun dante-get-var (symbol)
  "Return the value of SYMBOL in the GHCi process buffer."
  (let ((bp (dante-buffer-p))) (when bp (buffer-local-value symbol bp))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive utils

(defun dante-list-buffers ()
  "List hidden process buffers created by dante.
You can use this to kill them or look inside."
  (interactive)
  (let ((buffers
         (--filter (string-match " dante:" (buffer-name it)) (buffer-list))))
    (if buffers
        (display-buffer (list-buffers-noselect nil buffers))
      (error "There are no Dante process buffers"))))

(defun dante-fontify-expression (expression)
  "Return a haskell-fontified version of EXPRESSION.
If `haskell-mode' is not loaded, just return EXPRESSION."
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

(defun dante-async-load-current-buffer (interpret cont)
  "Load and maybe INTERPRET the temp file for current buffer and run CONT in a session.
The continuation must call its first argument; see `dante-session'."
;; Note that the GHCi doc for :l and :r appears to be wrong. TEST before changing this code.
  (let* ((epoch (buffer-modified-tick))
         (unchanged (equal epoch dante-temp-epoch))
         (source-buffer (current-buffer))
         (fname (dante-temp-file-name (current-buffer)))
         (local-name (dante-local-name fname)))
    (unless unchanged ; so GHCi's :r may be a no-op; save some time if remote
      (setq dante-temp-epoch epoch)
      (write-region nil nil (dante-temp-file-name (current-buffer)) nil 0))
    (dante-cps-let (((buffer done) (dante-session))
                    (_ (dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code")))
                    (_ (dante-async-write buffer (if interpret (concat ":l *" local-name) (concat ":l " local-name))))
                    ((status err-messages loaded-modules) (dante-async-with-buffer buffer (apply-partially 'dante-load-loop "" nil))))
      (let ((load-msg (with-current-buffer buffer
                        (setq dante-loaded-file fname)
                        (if (and unchanged (eq status 'ok)) dante-load-message
                          (setq dante-load-message err-messages)))))
        ;; when no write was done, then GHCi does not repeat the warnings. So, we spit back the previous load messages.
        (with-current-buffer source-buffer (funcall cont done load-msg))))))

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-get-var 'dante-state) 'dead) (funcall cont 'interrupted)
    (dante-cps-let (((done messages) (dante-async-load-current-buffer nil)))
      (let* ((temp-file (dante-local-name (dante-temp-file-name (current-buffer)))))
      (funcall cont
               'finished
               (--remove (eq 'splice (flycheck-error-level it))
                         (--map (dante-fly-message it checker (current-buffer) temp-file) messages)))
      (funcall done)))))

(flycheck-define-generic-checker 'haskell-dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :modes '(haskell-mode literate-haskell-mode))

(defun dante-fly-message (matched checker buffer temp-file)
  "Convert the MATCHED message to flycheck format.
CHECKER and BUFFER are added if the error is in TEMP-FILE."
  (cl-destructuring-bind (file location-raw err-type msg) matched
    (let* ((type (cond
                  ((s-matches? "^warning: \\[-W\\(typed-holes\\|deferred-\\(type-errors\\|out-of-scope-variables\\)\\)\\]" err-type) 'error)
                  ((s-matches? "^warning:" err-type) 'warning)
                  ((s-matches? "^splicing " err-type) 'splice)
                  (t 'error)))
           (location (dante-parse-error-location location-raw)))
      (flycheck-error-new-at (plist-get location :line) (plist-get location :col) type (s-trim-right msg)
                             :checker checker
                             :buffer (when (string= temp-file file) buffer)
                             :filename (dante-buffer-file-name buffer)))))

(defun dante-parse-error-location (string)
  "Parse the line number from the error in STRING."
  (when (string-match (concat
                       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
                       "\\|"
                       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))") ;; "(289,5)-(291,36)"
                      string)
    (list :line (string-to-number (match-string 2 string))
          :col (string-to-number (match-string 4 string)))))

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
                         (_ (save-excursion (re-search-backward "import[\t ]*" (line-beginning-position) t)))
                         (import-end (match-end 0))
                         (import-start (match-beginning 0))
                         (is-import (eq import-end id-start)))
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
done when it is done sending commands.  (Only by calling 'done' can
other sub-sessions start running.)"
  (dante-async-with-buffer (or (dante-buffer-p) (dante-start)) #'dante-async-yield cont))

(defun dante-async-yield (cont)
  "Run CONT when GHCi becomes available for executing a session."
  (push cont dante-queue)
  (dante-schedule-next (current-buffer)))

(defun dante-schedule-next (buffer)
  "If GHCi is idle, run the next queued GHCi sub-session for BUFFER, if any.
Note that sub-sessions are not interleaved."
  (with-current-buffer buffer
    (if dante-callback (force-mode-line-update t)
      (let ((req (pop dante-queue)))
        (when req
          (funcall req buffer (apply-partially #'dante-schedule-next buffer)))))))

(defcustom dante-load-flags '("+c" "-fno-diagnostics-show-caret")
  "Flags to set whenever GHCi is started."
  :type (cons 'set (--map (list 'const :tag (concat (car it) ": " (cadr it)) (car it))
                          '(("+c" "Gather type information (necessary for `dante-type-at')")
                            ("-Wall" "Report all warnings")
                            ("-fdefer-typed-holes" "Accept typed holes, so that completion/type-at continues to work then.")
                            ("-fdefer-type-errors" "Accept incorrectly typed programs, so that completion/type-at continues to work then. (However errors in dependencies won't be detected as such)")
                            ("-fno-diagnostics-show-caret" "Cleaner error messages for GHC >=8.2 (ignored by earlier versions)")))))

(defun dante-start ()
  "Start a GHCi process and return its buffer."
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
          ((_start-messages
            (dante-async-call (s-join "\n" (--map (concat ":set " it) (-snoc dante-load-flags "prompt \"\\4%s|\""))))))
        (dante-set-state 'running))
      (set-process-filter
       process
       (lambda (process string)
         (when (memq 'inputs dante-debug) (message "[Dante] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (let ((callback dante-callback))
               (unless callback (error "Received output in %s (%s) but no callback is installed" (current-buffer) string))
               (setq dante-callback nil)
               (funcall callback (s-replace "\r" "" string)))))))
      (set-process-sentinel process 'dante-sentinel)
      buffer))

(defun dante-async-read (cont)
  "Install CONT as a callback for an unknown portion GHCi output.
Called in process buffer."
    (when dante-callback
      (error "Try to set a callback (%s), but one exists already! (%s)" cont dante-callback))
    (setq dante-callback cont)
    (force-mode-line-update t))

(defconst dante-ghci-prompt "\4\\(.*\\)|")
(defun dante-wait-for-prompt (acc cont)
  "ACC umulate input until prompt is found and call CONT."
  (if (string-match dante-ghci-prompt acc)
      (funcall cont (substring acc 0 (1- (match-beginning 1))) (match-string 1 acc))
    (dante-cps-let ((input (dante-async-read)))
      (dante-wait-for-prompt (concat acc input) cont))))

(defun dante-load-loop (acc err-msgs cont)
  "Parse the output of load command.
ACC umulate input and ERR-MSGS.  When done call (CONT status error-messages loaded-modules)."
  (setq dante-state 'loading)
  (let* ((success "^Ok, modules loaded:[ ]*\\([^\n ]*\\)\\( (.*)\\)?\.")
         (progress "^\\[\\([0-9]*\\) of \\([0-9]*\\)\\] Compiling \\([^ ]*\\).*")
         (err-regexp "^\\([A-Z]?:?[^ \n:][^:\n\r]+\\):\\([0-9()-:]+\\): \\(.*\\)\n\\(\\([ ]+.*\n\\)*\\)")
         (i (string-match (s-join "\\|" (list dante-ghci-prompt success err-regexp progress)) acc))
         (m (when i (match-string 0 acc)))
         (rest (when i (substring acc (match-end 0)))))
    (cond ((and m (string-match dante-ghci-prompt m))
           (setq dante-state 'ghc-reports-error)
           (funcall cont 'failed (nreverse err-msgs) (match-string 1 m)))
          ((and m (string-match progress m))
           (setq dante-state (list 'compiling (match-string 3 m)))
           (dante-load-loop rest err-msgs cont))
          ((and m (string-match success m))
           ;; With the +c setting, GHC (8.2) prints: 1. error
           ;; messages+warnings, if compiling only 2. if successful,
           ;; repeat the warnings
           (dante-cps-let (((_status warning-msgs loaded-mods) (dante-load-loop rest nil)))
             (setq dante-state (list 'loaded loaded-mods))
             (funcall cont 'ok (or (nreverse err-msgs) warning-msgs) loaded-mods)))
          ((and m (> (length rest) 0) (/= (elt rest 0) ? )) ;; make sure we're matching a full error message
           (dante-load-loop rest (cons (-take 4 (cdr (s-match err-regexp m))) err-msgs) cont))
          (t (dante-cps-let ((input (dante-async-read)))
               (dante-load-loop (concat acc input) err-msgs cont))))))

(defun dante-async-write (buffer cmd cont)
  "Write to dante BUFFER the CMD and call CONT."
  (when (memq 'outputs dante-debug) (message "[Dante] -> %s" cmd))
  (process-send-string (get-buffer-process buffer) (concat cmd "\n"))
  (funcall cont ()))

(defun dante-async-with-buffer (buffer f cont)
  "Save context, then in BUFFER, cps-call F call CONT with the results of the call in the restored context."
  (let ((source-marker (point-marker)))
    (with-current-buffer buffer
      (funcall f (lambda (&rest e)
                   (with-current-buffer (marker-buffer source-marker)
                     (save-excursion (goto-char source-marker)
                                     (apply cont e))))))))

(defun dante-async-call (cmd cont)
  "Send GHCi the command string CMD.
The response is passed to CONT as (CONT REPLY)."
  (dante-cps-let
      ((_ (dante-async-write (dante-buffer-p) cmd))
       ((s _) (dante-async-with-buffer (dante-buffer-p) (apply-partially #'dante-wait-for-prompt ""))))
    (when (memq 'responses dante-debug) (message "GHCi <= %s\n     => %s" cmd s))
    (funcall cont (s-trim-right s))))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (let ((buffer (process-buffer process)))
    (when (and (buffer-live-p buffer) (not (process-live-p process)))
      (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
          (message "GHCi process deleted.")
        (with-current-buffer buffer (setq dante-state 'dead))
        (dante-show-process-problem process change)))))

(defun dante-diagnose ()
  "Show all state info in a help buffer."
  (interactive)
  (let ((info (dante-debug-info (dante-buffer-p))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert info)))))

(defun dante-debug-info (buffer)
  "Show debug info for dante buffer BUFFER."
  (if buffer
      (with-current-buffer buffer
        (s-join "\n" (--map (format "%s %S" it (eval it))
                            '(default-directory dante-command-line dante-state dante-queue dante-callback dante-load-message))))
    (format "No GHCi interaction buffer")))

(defun dante-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Problem with GHCi process!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat "This is the buffer associated with the GHCi session. This buffer
is normally hidden, but the GHCi process ended.

EXTRA TROUBLESHOOTING INFO

Process state change: " change "
" (dante-debug-info (current-buffer)) "

WHAT TO DO NEXT

Verify that the GHCi REPL can be loaded manually, then try to
customize (probably file-locally or directory-locally)
`dante-project-root' and/or `dante-repl-command-line'.  If you
fixed the problem, just kill this buffer, Dante will make a fresh
one and attempt to restart GHCi automatically.

If you do not want Dante will not attempt to restart GHCi, just
leave this buffer around. You can always run `dante-restart' to
make it try again.
")
    'face 'compilation-error)))

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
      (fundamental-mode) ;; this has several effects, including resetting the local variables
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
;; Idle-hook (missing bit: check for errors)

;; (defvar dante-timer nil)
;; (defun dante-idle-function ()
;;   (when (bound-and-true-p dante-mode)
;;     (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
;;       (unless (or (nth 4 (syntax-ppss)) (nth 3 (syntax-ppss)) (s-blank? tap))
;;         (setq-local dante-idle-point (point))
;;         (dante-cps-let (((done _load-messages) (dante-async-load-current-buffer t))
;;                         (ty (dante-async-call (concat ":type-at " tap))))
;;           (when (eq (point) dante-idle-point)
;;             (unless (current-message)
;;               (message "%s" (s-collapse-whitespace (dante-fontify-expression ty)))))
;;           (funcall done))))))
;; (when dante-timer (cancel-timer dante-timer))
;; (setq dante-timer (run-with-idle-timer 1 t #'dante-idle-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-fix

(defcustom dante-suggestible-extensions
  '("AllowAmbiguousTypes" "BangPatterns" "ConstraintKinds" "DataKinds" "DeriveFoldable" "DeriveFunctor" "DeriveGeneric" "DeriveTraversable" "EmptyCase" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GADTs" "GeneralizedNewtypeDeriving" "InstanceSigs" "KindSignatures" "MultiParamTypeClasses" "PartialTypeSignatures" "PatternSynonyms" "PolyKinds" "RankNTypes" "RecordWildCards" "ScopedTypeVariables" "StandaloneDeriving" "TransformListComp" "TupleSections" "TypeApplications" "TypeFamilies" "TypeInType" "TypeOperators" "TypeSynonymInstances" "UndecidableSuperClasses" "UndecidableInstances" "ViewPatterns" )
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
            (insert (concat "(" type-expr ")"))))
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
      (insert (apply 'concat (--map (concat "-- " it "\n") (--remove (s-blank? it) (s-lines res)))))
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
