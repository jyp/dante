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
;; Package-Requires: ((flycheck "0.30") (emacs "25.1") (dash "2.13.0"))

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

(require 'json)
(require 'cl-lib)
(require 'eldoc)
(require 'dash)
(require 'xref)
(require 'flycheck)
(eval-when-compile
  (require 'wid-edit))

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
`dante-project-root' contents.  Customize as a file or directory
variable."
  :group 'dante
  :type '(repeat string))

(defcustom dante-project-root nil
  "The project root, as a string or nil.
When nil, dante will guess the value by looking for a cabal file.
Customize as a file or directory variable."
  :group 'dante
  :type '(choice (const nil) string))

(defun dante-project-root ()
  "Get the directory where the .cabal file is placed."
  (or dante-project-root
      (setq-local dante-project-root
            (file-name-directory (or (dante-cabal-find-file) (dante-buffer-file-name))))))

(defun dante-environment ()
  "Guess the project environment."
  (cond
   ((file-exists-p (concat (dante-project-root) "shell.nix")) 'nix)
   ((file-exists-p (concat (dante-project-root) "stack.yaml")) 'stack)
   (t 'bare)))

(defun dante-repl-command-line ()
  "Return a suitable command line to run GHCi.
Guessed if the variable dante-repl-command-line is nil."
  (or dante-repl-command-line
      (setq-local dante-repl-command-line
            (cl-case (dante-environment)
              (bare (list "cabal" "repl"))
              (nix (list "nix-shell" "--run" "cabal repl"))
              (stack '("stack" "repl"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar dante-mode-map (make-sparse-keymap)
  "Dante minor mode's map.")

(defun dante-status ()
  "Return dante's status for the current source buffer."
  (if (eq (dante-state) 'ready)
      (buffer-local-value 'dante-loaded-modules (dante-buffer-p))
    (symbol-name (dante-state))))

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
(define-key dante-mode-map (kbd "C-c '") 'dante-eval-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local dante-loaded-modules "" "Loaded modules as a string, reported by GHCi")
(defvar-local dante-queue nil "List of ready GHCi queries.")
(defvar-local dante-callback nil "Callback waiting for output.")
(defvar-local dante-package-name nil "The package name associated with the current buffer.")
(defvar-local dante-state nil
  "nil: initial state
- starting: GHCi starting
- running: GHCi running
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own. Do not try restarting
automatically. The user will have to manually run `dante-restart'
to destroy the buffer and create a fresh one without this variable enabled.")

(defun dante-state ()
  "Return dante-state for the current source buffer."
  (if (dante-buffer-p)
      (buffer-local-value 'dante-state (dante-buffer-p))
    'stopped))

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

(defvar haskell-mode-hook)
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

(defun dante-type-at ()
  "Get the type of the thing or selection at point."
  (interactive)
  (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
    (dante-cps-let ((_load-messages (dante-async-load-current-buffer nil))
              (ty (dante-async-call (concat ":type-at " tap))))
      (message "%s" (dante-fontify-expression ty)))))

(defun dante-info (ident)
  "Get the info about the IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (let ((package (dante-package-name))
        (help-xref-following nil)
        (origin (buffer-name)))
    (dante-cps-let ((_load-message (dante-async-load-current-buffer t))
              (info (dante-async-call (format ":i %s" ident))))
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
  "Load and maybe INTERPRET the temp file for buffer it and run CONT in a session."
  (let ((fname (dante-temp-file)))
    (dante-cps-let (((buffer done) (dante-start))
              (_ (dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code")))
              (load-message
               (dante-async-call
                (if (string-equal (buffer-local-value 'dante-loaded-file buffer) fname)
                    ":r" (concat ":l *" fname)))))
      (with-current-buffer buffer (setq dante-loaded-interpreted interpret))
      (funcall cont load-message)
      (funcall done))))

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-state) 'dead)
      (run-with-timer 0 nil cont 'interrupted)
    (dante-cps-let ((string (dante-async-load-current-buffer nil)))
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

(defun dante-parse-errors-warnings-splices (checker buffer string)
  "Parse flycheck errors and warnings.
CHECKER and BUFFER are added to each item parsed from STRING."
  (let ((messages (list))
        (temp-file (dante-temp-file-name buffer)))
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
;; Source buffer operations

(defun dante-thing-at-point ()
  "Return (list START END) of something at the point."
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (dante-ident-pos-at-point)))

(defun dante-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (dante-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cadr reg)))))

(defun dante-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    ;; Skip whitespace if we're on it.  That way, if we're at "map ", we'll
    ;; see the word "map".
    (if (and (not (eobp))
             (eq ?  (char-syntax (char-after))))
        (skip-chars-backward " \t"))

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

(defun dante-temp-file-name (buffer)
  "Return a filename suitable to store BUFFER's contents."
  (with-current-buffer buffer
    (or dante-temp-file-name
        (setq dante-temp-file-name
              (dante-canonicalize-path (make-temp-file "dante" nil (file-name-extension (buffer-file-name) t)))))))

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
    (set-dante-state 'deleting)
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
    (while (not result) (sleep-for 0.001))
    (car result)))

(defun dante-restart ()
  "Restart the process with the same configuration as before."
  (interactive)
  (when (dante-buffer-p) (dante-destroy))
  (dante-start (lambda (_buffer done) (funcall done))))

(defun dante-start (cont) ;; TODO: rename to "dante-session"
  "Run the CONT in a valid GHCi session for the current (source) buffer.
CONT is called as (CONT process-buffer done).  CONT must call done
when it is done."
  (let ((source-buffer (current-buffer))
        (buffer (dante-get-buffer-create)))
    (push (list :func cont :source-buffer source-buffer)
          dante-queue)
    ;; TODO: test process-live-p
    (if (get-buffer-process buffer)
        (dante-schedule-next buffer)
      (dante-start-process-in-buffer buffer source-buffer))))

(defun dante-start-process-in-buffer (buffer source-buffer)
  "Start a Dante worker in BUFFER for SOURCE-BUFFER."
  (if (eq (buffer-local-value 'dante-state buffer) 'dead)
      buffer
    (let* ((args (dante-repl-command-line))
           (process (with-current-buffer buffer
                      (when (memq 'command-line dante-debug)
                        (message "GHCi command line: %s" (combine-and-quote-strings args)))
                      (message "Dante: Starting GHCi ...")
                      (apply #'start-process "dante" buffer args))))
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer buffer
        (erase-buffer)
        (setq dante-callback nil) ;; todo: necessary?
        (setq-local dante-repl-command-line args))
      (with-current-buffer source-buffer
        (set-dante-state 'starting)
        (dante-cps-let
            ((_start-messages (dante-async-call
                               (concat ":set -Wall\n" ;; TODO: configure
                                       ":set +c\n" ;; collect type info
                                       ":set prompt \"\\4%s|\""))))
          (message "GHCi started!")
          (dante-schedule-next buffer)))
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
      buffer)))

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
      (error "Try to set a callback (%s)\n... but one exists already! (%s)" cont dante-callback))
    (setq dante-callback cont))

(defun dante-async-call (cmd cont)
  "Send GHCi the command string CMD.
The result is passed to CONT as (CONT REPLY).  Can only be called
from a valid session."
  (when (memq 'outputs dante-debug) (message "[Dante] -> %s" cmd))
  (let ((source-buffer (current-buffer)))
  (with-current-buffer (dante-buffer-p)
    (process-send-string (get-buffer-process (current-buffer)) (concat cmd "\n"))
    (dante-async (apply-partially #'dante-wait-for-prompt source-buffer cmd "" cont)))))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (when (buffer-live-p (process-buffer process))
    (when (not (process-live-p process))
      (let ((buffer (process-buffer process)))
        (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
            (message "GHCi process deleted.")
            (progn (with-current-buffer buffer (setq dante-state 'dead))
                   (dante-show-process-problem process change)))))))

(defun dante-debug-info (buffer)
  "Show debug info for dante buffer BUFFER."
  (if buffer
      (with-current-buffer buffer
        (format "Directory:%s\n Command line:%s\n Loaded: %s\n State: %s\n Queue: %s\n Callback: %s\n"
                default-directory (combine-and-quote-strings dante-repl-command-line) dante-loaded-modules dante-state dante-queue dante-callback))
    (format "Dante not started in %s" buffer)))

(defun dante-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Problem with GHCi process!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat
     "This where GHCi output is bufferized. This buffer is normally hidden,
but a problem occcured.

EXTRA TROUBLESHOOTING INFO

The GHCi process ended. Here is the reason that Emacs gives us: " change "
" (dante-debug-info (current-buffer)) "

WHAT TO DO NEXT

Try to customize (probably file or directory-locally)
`dante-project-root' and/or `dante-repl-command-line'.  If you
fixed the problem, just kill this buffer, Dante will make a fresh
one and attempt to restart GHCi automatically.

If you are unable to fix the problem, just leave this buffer
around and Dante will not attempt to restart GHCi.

You can always run M-x dante-restart to make it try again.
")
    'face 'compilation-error)))

(defun dante-wait-for-prompt (source-buf cmd acc cont s-in)
  "Loop waiting for a GHCi prompt for SOURCE-BUF after CMD.
Text is ACC umulated.  CONT is call with all concatenated S-IN."
  (let ((s (concat acc s-in)))
    (if (string-match "\4\\(.*\\)|" s)
        (progn
          (setq dante-loaded-modules (match-string 1 s))
          (let ((string (dante--kill-last-newline (substring s 0 (1- (match-beginning 1))))))
            (when (memq 'responses dante-debug) (message "GHCi <= %s\n     => %s" cmd string))
            (with-current-buffer source-buf (funcall cont acc))))
      (dante-async (apply-partially #'dante-wait-for-prompt source-buf cmd s cont)))))


(defun dante-read-buffer ()
  "Process GHCi output."
  (let ((callback dante-callback)
        (string (dante--strip-carriage-returns (buffer-string))))
    (unless dante-callback (error "Received output in %s (%s) but no callback" (current-buffer) string))
    (delete-region 1 (point-max))
    (setq dante-callback nil)
    (funcall callback string)))

(defun dante-schedule-next (buffer)
  "Run the next GHCi sub-session for BUFFER, if any."
  (unless dante-callback
    (let ((req (pop dante-queue)))
      (if (not req)
          (set-dante-state 'ready)
        (set-dante-state 'busy)
        (with-current-buffer (plist-get req :source-buffer)
          (funcall (plist-get req :func) buffer
                   (apply-partially #'dante-schedule-next buffer)))))))

(defun dante--strip-carriage-returns (string)
  "Strip the \\r from Windows \\r\\n line endings in STRING."
  (replace-regexp-in-string "\r" "" string))

(defun dante--kill-last-newline (string)
  "Strip the last newline character in STRING."
  (replace-regexp-in-string "\n$" "" string))

(defun dante-get-buffer-create ()
  "Get or create the buffer for GHCi.
Uses the directory of the current buffer for context."
  (let* ((root (dante-project-root))
         (cabal-file (dante-cabal-find-file))
         (package-name (if cabal-file
                           (dante-package-name cabal-file)
                         ""))
         (buffer-name (dante-buffer-name))
         (default-directory (if cabal-file
                                (file-name-directory cabal-file)
                              root)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq dante-package-name package-name)
      (fundamental-mode)
      (cd default-directory)
      (current-buffer))))

(defun set-dante-state (state)
  "Set the dante-state to STATE and redisplay the modeline."
  (with-current-buffer (dante-buffer-p) (setq-local dante-state state))
  (force-mode-line-update))

(defun dante-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (dante-buffer-name)))

(defun dante-buffer-name ()
  "Create a dante process buffer name."
  (let* ((root (dante-project-root))
         (package-name (dante-package-name)))
    (concat " dante:" package-name " " root)))


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

(defun dante--make-xref (string nm)
  "Turn the GHCi reference STRING in to an xref with description NM."
  (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                      string)
    (let ((file (match-string 1 string))
          (line (string-to-number (match-string 2 string)))
          (col (string-to-number (match-string 3 string))))
      (xref-make nm (xref-make-file-location
                     (if (string= file (dante-temp-file-name (current-buffer))) (buffer-file-name) file)
                     line
                     (1- col))))))

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (dante-cps-let ((ret (blocking-call))
            (_load-messages (dante-async-load-current-buffer nil))
            (target (dante-async-call (concat ":loc-at " symbol))))
    (let ((xref (dante--make-xref target "def")))
      (funcall ret (when xref (list xref))))))

(cl-defmethod xref-backend-references ((_backend (eql dante)) symbol)
  (dante-cps-let ((ret (blocking-call))
            (_load-messages (dante-async-load-current-buffer nil))
            (result (dante-async-call (concat ":uses " symbol))))
    (let* ((xref (dante--make-xref result "ref"))
           (refs nil))
      (while xref
        (setq result (substring result (match-end 0)))
        (push xref refs)
        (setq xref (dante--make-xref result "ref")))
      (funcall ret (nreverse refs)))))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-fix

(defcustom dante-suggestible-extensions
  '("AllowAmbiguousTypes" "ConstraintKinds" "DataKinds" "DeriveFoldable" "DeriveFunctor" "DeriveGeneric" "DeriveTraversable" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GADTs" "GeneralizedNewtypeDeriving" "InstanceSigs" "KindSignatures" "MultiParamTypeClasses" "PolyKinds" "RankNTypes" "RecordWildCards" "ScopedTypeVariables" "TypeApplications" "TypeFamilies" "TypeInType" "TypeOperators" "TypeSynonymInstances" "UndecidableSuperClasses" "UndecidableInstances" "ViewPatterns")
  "Language extensions that Dante will use to fix errors."
  :group 'dante
  :type '(repeat string))

(defun dante-auto-fix (pos)
  "Attempt to fix the flycheck error at POS."
  (interactive "d")
  (let ((messages (delq nil (mapcar #'flycheck-error-message
                                    (flycheck-overlay-errors-at pos)))))
    (when messages
      (let ((msg (car messages)))
        (save-excursion
          (cond
           ((string-match "A do-notation statement discarded a result of type" msg)
            (goto-char (car (dante-ident-pos-at-point)))
            (insert "_ <- "))
           ((string-match "Failed to load interface for ‘\\(.*\\)’\n[ ]*Perhaps you meant \\([^ ]*\\)" msg)
            (let ((replacement (match-string 2 msg)))
              (search-forward (match-string 1 msg))
              ;; ^^ delete-region may garble the matches
              (delete-region (match-beginning 0) (point))
              (insert replacement)))
           ((string-match "Perhaps you meant ‘\\([^‘]*\\)’" msg)
            (let ((replacement (match-string 1 msg)))
              ;; ^^ delete-region may garble the matches
              (apply #'delete-region (dante-ident-pos-at-point))
              (insert replacement)))
           ((--any? (string-match it msg) dante-suggestible-extensions)
            (goto-char 1)
            (insert (concat "{-# LANGUAGE " (car (--filter (string-match it msg) dante-suggestible-extensions)) " #-}\n")))
           ((string-match "Top-level binding with no type signature:[\n ]*" msg)
            (beginning-of-line)
            (insert (concat (substring msg (match-end 0)) "\n")))
           ((string-match "Defined but not used" msg)
            (goto-char (car (dante-ident-pos-at-point)))
            (insert "_"))
           ((string-match "The import of ‘.*’ is redundant" msg)
            (beginning-of-line)
            (delete-region (point) (progn (next-logical-line) (point))))
           (t (message "Cannot fix the issue at point automatically. Perhaps customize `dante-suggestible-extensions'."))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reploid

(defun dante-eval-block ()
  "Evaluate the expression command found in {-> <expr> -} and insert the result."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "{-> "))
        (message "Not in an evaluable block. (Expecting the line to start with '{->')")
      (let* ((beg (+ 4 (point)))
             (end (copy-marker (if (search-forward "-}" (line-end-position) t)
                                   (- (point) 2)
                                 (line-end-position)))))
        (dante-cps-let ((_load-messages (dante-async-load-current-buffer t))
                  (res (dante-async-call (buffer-substring-no-properties beg end))))
             (goto-char end)
             (skip-chars-backward "\t\n ")
             (delete-region (point) (- (search-forward "-}") 2))
             (backward-char 2)
             (insert (concat "\n\n" res "\n")))))))

(provide 'dante)

;;; dante.el ends here
