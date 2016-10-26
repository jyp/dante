;;; dante.el --- Development mode for Haskell -*- lexical-binding: t -*-

;; DANTE: Do Not Aim To Expand.

;; This is a mode for GHCi advanced "IDE" features. The mode depends
;; on GHCi only, keeping the logic simple. Additionally it aims to be
;; minimal as far as possible.

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
;; Package-Requires: ((flycheck "0.30") (emacs "25.1") (haskell-mode "13.0"))

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

;;; Code:

(require 'flycheck)
(require 'json)
(require 'warnings)
(require 'cl-lib)
(require 'comint)
(require 'eldoc)
(require 'dash)
(eval-when-compile
  (require 'wid-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup dante nil
  "Complete development mode for Haskell"
  :group 'haskell)

(defcustom dante-debug nil
  "Show debug output."
  :group 'dante
  :type 'boolean)

;; TODO: collapse dante-environment, dante-targets, dante-command-line
;; to one single variable.
(defcustom dante-environment nil
  "Environment to use: bare ghc(i), nix or stack.
You should set this as a file or directory variable if the guess is
wrong."
  :group 'dante
  :type '(choice (const :tag "Nix" nix)
                 (const :tag "Bare (just cabal)" bare)
                 (const :tag "Stack" stack)
                 (const :tag "Auto" nil)))

(defcustom dante-project-root nil
  "The project root.
When nil, dante will guess the value using the
`dante-project-root' function.  You should set this as a file or
directory variable if the guess is wrong."
  :group 'dante
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(defvar dante-mode-map (make-sparse-keymap)
  "Dante minor mode's map.")

(defun dante-lighter ()
  "Lighter for the dante minor mode."
  (concat " Dante:" (symbol-name (dante-state))))

;;;###autoload
(define-minor-mode dante-mode
  "Minor mode for Dante

\\{dante-mode-map}"
  :lighter (:eval (dante-lighter))
  :keymap dante-mode-map
  (if dante-mode
      (progn (flycheck-select-checker 'dante)
             (setq-local eldoc-documentation-function 'eldoc-dante))
      (progn (flycheck-select-checker 'haskell-ghc)
             (setq-local eldoc-documentation-function 'ignore))))

(define-key dante-mode-map (kbd "C-c C-x C-r") 'dante-restart)
(define-key dante-mode-map (kbd "C-c C-t") 'dante-type-at)
(define-key dante-mode-map (kbd "C-c C-i") 'dante-info)
(define-key dante-mode-map (kbd "C-c C-a") 'dante-auto-fix)

(defun turn-on-dante-mode ()
  "Turn on Dante in the current buffer."
  (interactive)
  (dante-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local dante-callbacks (list)
  "List of callbacks waiting for output.
LIST is a FIFO.")

(defvar-local dante-arguments (list)
  "Arguments used to call the stack process.")

(defvar-local dante-targets (list)
  "Targets used for the stack process.")

(defvar-local dante-package-name nil
  "The package name associated with the current buffer.")

(defvar-local dante-state nil
  "nil: initial state
- starting: GHCi starting
- running: GHCi running
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own. Do not try restarting
automatically. The user will have to manually run `dante-restart'
or `dante-targets' to destroy the buffer and create a fresh one
without this variable enabled.
  ")

(defun dante-state ()
  "Return dante-state for the current source buffer."
  (let ((process-buffer (dante-buffer-p)))
    (if process-buffer (buffer-local-value 'dante-state process-buffer) 'stopped)))

(defvar-local dante-starting nil
  "When non-nil, indicates that the dante process starting up.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands

(defun dante-toggle-debug ()
  "Toggle debugging mode on/off."
  (interactive)
  (setq dante-debug (not dante-debug))
  (message "Dante debugging is: %s" (if dante-debug "ON" "OFF")))

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
  "Return a haskell-fontified version of EXPRESSION."
  (with-temp-buffer
    (let ((haskell-mode-hook nil)) ;; to keep switching mode cheap
      (when (fboundp 'haskell-mode) (haskell-mode))
      (insert expression)
      (font-lock-ensure)
      (buffer-string))))

(defun dante-type-at (insert)
  "Get the type of the thing or selection at point.

With prefix argument INSERT, inserts the type above the current
line as a type signature."
  (interactive "P")
  (dante-async-load-current-buffer)
  (let ((ty (dante-blocking-call
             (concat ":type-at " (dante--ghc-subexp (dante-thing-at-point))))))
    (if insert
        (save-excursion
          (goto-char (line-beginning-position))
          (insert (dante-fontify-expression ty) "\n"))
      (message "%s" (dante-fontify-expression ty)))))

(defun dante-info (ident)
  "Get the info about the IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (let ((package (dante-package-name))
        (info (dante-get-info-of ident))
        (help-xref-following nil)
        (origin (buffer-name)))
    (help-setup-xref (list #'dante-call-in-buffer (current-buffer) #'dante-info ident)
                     (called-interactively-p 'interactive))
    (save-excursion
      (let ((help-xref-following nil))
        (with-help-window (help-buffer)
          (with-current-buffer (help-buffer)
            (insert
             (dante-fontify-expression ident)
             " in `"
             origin
             "'"
             " (" package ")"
             "\n\n"
             (dante-fontify-expression info))
            (goto-char (point-min))))))))

(defun dante-get-info-of (thing)
  "Get info for THING."
  (dante-async-load-current-buffer)
  (let ((optimistic-result
         (dante-blocking-call (format ":i %s" thing))))
    (if (string-match "^<interactive>" optimistic-result)
        ;; Load the module Interpreted so that we get information
        (progn (dante-async-call  ":set -fbyte-code")
               ;; ^^ Workaround for a bug of GHCi: info for external
               ;; ids can be gotten only so
               (dante-async-load-current-buffer)
               (prog1
                   (dante-blocking-call
                    (format ":i %s" thing))
                 (dante-async-call  ":set -fobject-code")))
      optimistic-result)))

(defun dante-restart ()
  "Restart the process with the same configuration as before."
  (interactive)
  (when (dante-buffer-p)
    (let ((targets (with-current-buffer (dante-buffer)
                     dante-targets)))
      (dante-destroy)
      (dante-get-worker-create targets))))

(defun dante-targets ()
  "Set the targets to use for cabal repl."
  (interactive)
  (let* ((old-targets
          (with-current-buffer (dante-buffer )
            dante-targets))
         (targets (split-string (read-from-minibuffer "Targets: " nil nil nil nil old-targets)
                                " "
                                t)))
    (dante-destroy)
    (dante-get-worker-create targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defvar-local dante-loaded-file "-")

(defun dante-async-load-current-buffer (&optional cont)
  "Load (interpreted) the temp buffer and run CONT."
  (let ((fname (dante-temp-file-name)))
    (if (string-equal (buffer-local-value 'dante-loaded-file (dante-buffer)) fname)
        (dante-async-call ":r" cont)
    (dante-async-call (concat ":l *" fname) cont)
    ;; Note the * so that we collect the type info for the current
    ;; module (also, probably faster.)
    (with-current-buffer (dante-buffer) (setq dante-loaded-file fname)))))

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-state) 'dead)
      (run-with-timer 0 nil cont 'interrupted)
    (dante-async-load-current-buffer
     (lambda (string)
       (let ((msgs (dante-parse-errors-warnings-splices
                    checker
                    (current-buffer)
                    string)))
         (funcall cont
                  'finished
                  (cl-remove-if (lambda (msg)
                                  (eq 'splice (flycheck-error-level msg)))
                                msgs)))))))

(flycheck-define-generic-checker 'dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :modes '(haskell-mode literate-haskell-mode))

(defun dante-parse-errors-warnings-splices (checker buffer string)
  "Parse flycheck errors and warnings.
CHECKER and BUFFER are added to each item parsed from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((messages (list))
          (temp-file (dante-temp-file-name buffer)))
      (while (search-forward-regexp
              (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                      "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
              nil t 1)
        (let* ((file (dante-canonicalize-path (match-string 1)))
               (location-raw (match-string 2))
               (msg (match-string 3)) ;; Replace gross bullet points.
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
          (setq messages
                (cons (flycheck-error-new-at
                       line column type
                       msg
                       :checker checker
                       :buffer (when (string= temp-file file) buffer)
                       :filename (dante-buffer-file-name buffer))
                      messages)))
        (forward-line -1))
      (delete-dups messages))))

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
;; ELDoc integration

(defvar-local eldoc-dante-cache (make-hash-table :test 'equal)
  "Cache for types of regions, used by `eldoc-dante'.
This is not for saving on requests (we make a request even if
something is in cache, overwriting the old entry), but rather for
making types show immediately when we do have them cached.")

(defun eldoc-dante-maybe-print (msg)
  "Print MSG with eldoc if eldoc would display a message now.
Like `eldoc-print-current-symbol-info', but just printing MSG
instead of using `eldoc-documentation-function'."
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
         (eldoc-message msg))))

(defun eldoc-dante ()
  "ElDoc backend for dante."
  (let ((pos (dante-thing-at-point)))
    (when pos
      (dante-async-call
       (concat ":type-at " (dante--ghc-subexp pos))
         (lambda (ty)
           (let ((response-status (dante-haskell-utils-repl-response-error-status ty)))
             (if (eq 'no-error response-status)
               (let ((msg (dante-fontify-expression
                           (replace-regexp-in-string "[ \n]+" " " ty))))
                 ;; Got an updated type-at-point, cache and print now:
                 (puthash pos msg eldoc-dante-cache)
                 (eldoc-dante-maybe-print msg))
               ;; But if we're seeing errors, invalidate cache-at-point:
               (remhash pos eldoc-dante-cache)))))
  ;; If we have something cached at point, print that first:
  (gethash pos eldoc-dante-cache))))

(defun dante-haskell-utils-repl-response-error-status (response)
  "Parse response REPL's RESPONSE for errors.
Returns one of the following symbols:

+ unknown-command
+ option-missing
+ interactive-error
+ no-error

*Warning*: this funciton covers only three kind of responses:

* \"unknown command …\"
  REPL missing requested command
* \"<interactive>:3:5: …\"
  interactive REPL error
* \"Couldn't guess that module name. Does it exist?\"
  (:type-at and maybe some other commands error)
* *all other reposnses* are treated as success reposneses and
  'no-error is returned."
  (let ((first-line (car (split-string response "\n" t))))
    (cond
     ((null first-line) 'no-error)
     ((string-match-p "^unknown command" first-line)
      'unknown-command)
     ((string-match-p
       "^Couldn't guess that module name. Does it exist?"
       first-line)
      'option-missing)
     ((string-match-p "^<interactive>:" first-line)
      'interactive-error)
     (t 'no-error))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer operations

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

(defvar-local dante-temp-text "-"
  "The contents the buffer last written to `dante-temp-file-name' ")

(defun dante-temp-file-name (&optional buffer)
  "Return the name of a temp file containing an up-to-date copy of BUFFER's contents."
  (with-current-buffer (or buffer (current-buffer))
    (prog1
        (or dante-temp-file-name
            (setq dante-temp-file-name
                  (dante-canonicalize-path (make-temp-file "dante" nil ".hs"))))
      (let ((contents (buffer-string)))
        (unless (string-equal contents dante-temp-text) ;; so ghci's :r may be noop
          (setq dante-temp-text contents)
          (write-region nil nil dante-temp-file-name 0))))))

(defun dante-canonicalize-path (path)
  "Return a standardized version of PATH.
Path names are standardised and drive names are
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query/commands

(defun dante--ghc-column-number-at-pos (pos)
  (1+ (save-excursion (goto-char pos) (current-column))))

(defun dante--ghc-subexp (reg)
  (pcase reg (`(,beg . (,end . nil))
              (format "%S %d %d %d %d %s"
                      (dante-temp-file-name)
                      (line-number-at-pos beg)
                      (dante--ghc-column-number-at-pos beg)
                      (line-number-at-pos end)
                      (dante--ghc-column-number-at-pos end)
                      (buffer-substring-no-properties beg end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

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

(defun dante-blocking-call (cmd)
  "Send GHCi the command string CMD and block pending its result."
  (let ((result nil))
    (dante-async-call
     cmd
     (lambda (reply) (setq result reply)))
    (while (not result) (sleep-for 0.0001))
    result))

(defun dante-buffer ()
  "Get the GHCi buffer for the current directory."
  (let ((buffer (dante-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (dante-get-worker-create nil))))

(defun dante-process () ;; TODO: remove
  "Get the GHCi process for the current directory."
  (get-buffer-process (dante-buffer )))

(defun dante-get-worker-create (&optional targets)
  "Start a GHCi session suitable for the current (source) buffer.
If provided, use the specified TARGETS."
  (let ((buffer (dante-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (dante-start-process-in-buffer buffer targets (current-buffer)))))

(defun dante-environment ()
  "Return environment for dante.
See variable dante-environment."
  (or dante-environment
      (setq dante-environment
            (cond
             ((file-exists-p (concat (dante-project-root) "stack.yaml")) 'stack)
             ((file-exists-p (concat (dante-project-root) "shell.nix")) 'nix)
             (t 'bare)))))

(defun dante-repl-command-line ()
  "Wrap the command line CMD according to `dante-environment'."
  (cl-case (dante-environment)
    (bare (list "cabal" "repl"))
    (nix (list "nix-shell" "--run" "cabal" "repl"))
    (stack '("stack" "repl"))))

(defun dante-start-process-in-buffer (buffer targets source-buffer)
  "Start a Dante worker in BUFFER, for the default or specified TARGETS.
Automatically performs initial actions in SOURCE-BUFFER."
  (if (eq (buffer-local-value 'dante-state buffer) 'dead)
      buffer
    (let* ((args (dante-repl-command-line))
           (process (with-current-buffer buffer
                      (when dante-debug
                        (message "GHCi command line: %s" (combine-and-quote-strings args)))
                      (message "Booting up dante ...")
                      (apply #'start-process "dante" buffer args))))
      (set-process-query-on-exit-flag process nil)
      (process-send-string process ":set -Wall\n") ;; TODO: configure
      (process-send-string process ":set +c\n") ;; collect type info
      (process-send-string process ":set -fobject-code\n") ;; so that compilation results are cached
      (process-send-string process ":set prompt \"\\4\"\n")
      (set-dante-state 'starting)
      (with-current-buffer buffer
        (erase-buffer)
        (fundamental-mode)
        (setq dante-arguments args)
        (setq dante-targets targets)
        (setq dante-callbacks
              (list (list
                     :source-buffer source-buffer
                     :func (lambda (_msg)
                             (set-dante-state 'ready))))))
      (set-process-filter
       process
       (lambda (process string)
         (when dante-debug
           (message "[Dante] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string)
             (dante-read-buffer)))))
      (set-process-sentinel process 'dante-sentinel)
      buffer)))

(defun dante-async-call (cmd &optional callback)
  "Send GHCi the command string CMD.
The result is passed to CALLBACK as (CALLBACK REPLY)."
  (let ((source-buffer (current-buffer))
        (buffer (dante-buffer)))
    (if (and buffer (process-live-p (get-buffer-process buffer)))
        (progn (set-dante-state 'busy)
               (with-current-buffer buffer
                 (setq dante-callbacks
                       (append dante-callbacks
                               (list (list :func (or callback #'ignore)
                                           :source-buffer source-buffer
                                           :cmd cmd)))))
               (when dante-debug
                 (message "[Dante] -> %s" cmd))
               (comint-simple-send (dante-process) cmd))
      (error "Dante process is not running: run M-x dante-restart to start it"))))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (when (buffer-live-p (process-buffer process))
    (when (not (process-live-p process))
      (let ((buffer (process-buffer process)))
        (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
            (message "Dante process deleted.")
            (progn (with-current-buffer buffer (setq dante-state 'dead))
                   (dante-show-process-problem process change)))))))

(defun dante-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Problem with Dante!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat
     "This is the buffer where Emacs talks to GHCi. It's normally hidden,
but a problem occcured.

TROUBLESHOOTING

It may be obvious if there is some text above this message
indicating a problem.

The process ended. Here is the reason that Emacs gives us:

"
     "  " change
     "\n"
     "For troubleshooting purposes, here are the arguments used to launch dante:

"
     (combine-and-quote-strings dante-arguments)
     "

WHAT TO DO NEXT

If you fixed the problem, just kill this buffer, Dante will make
a fresh one and attempt to start the process automatically as
soon as you start editing code again.

If you are unable to fix the problem, just leave this buffer
around in Emacs and Dante will not attempt to start the process
anymore.

You can always run M-x dante-restart to make it try again.

")
    'face 'compilation-error)))

(defun dante-read-buffer ()
  "In the process buffer, we read what's in it."
  (goto-char 1)
  (when (search-forward "\4" nil t 1)
    (let ((callback (pop dante-callbacks)))
      (let ((string (dante--kill-last-newline
                     (dante--strip-carriage-returns (buffer-substring 1 (1- (point)))))))
        (delete-region 1 (point))
        (if callback
            (progn (with-current-buffer (plist-get callback :source-buffer)
                     (set-dante-state 'ready)
                     (funcall (plist-get callback :func) string))
                   (dante-read-buffer))
          (when dante-debug
            (dante--warn "Received output but no callback in `dante-callbacks': %S"
                         string)))))))

(defun dante--strip-carriage-returns (string)
  "Strip the \\r from Windows \\r\\n line endings in STRING."
  (replace-regexp-in-string "\r" "" string))

(defun dante--kill-last-newline (string)
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
      (cd default-directory)
      (current-buffer))))

(defun set-dante-state (state)
  (with-current-buffer (dante-buffer-p) (setq-local dante-state state))
  (force-mode-line-update))

(defun dante-buffer-p ()
  "Return non-nil if a GHCi buffer exists."
  (get-buffer (dante-buffer-name)))

(defun dante-buffer-name ()
  "Create a dante process buffer name."
  (let* ((root (dante-project-root))
         (package-name (dante-package-name)))
    (concat " dante:" package-name " " root)))

(defun dante-project-root ()
  "Get the directory where the .cabal file is placed."
  (or dante-project-root
      (setq dante-project-root
            (file-name-directory (or (dante-cabal-find-file) (dante-buffer-file-name))))))

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

(defun dante--warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Equivalent to 'warn', but label the warning as coming from dante."
  (display-warning 'dante (apply 'format message args) :warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref support

(defun dante--xref-backend () "Dante xref backend." (when dante-mode 'dante))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dante)))
  (dante--ghc-subexp (dante-ident-pos-at-point)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dante)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (dante-async-load-current-buffer)
  (let ((result (dante-blocking-call (concat ":loc-at " symbol))))
    (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                        result)
      (let ((file (match-string 1 result))
            (line (string-to-number (match-string 2 result)))
            (col (string-to-number (match-string 3 result))))
            (list (xref-make "def" (xref-make-file-location
                                    (if (string= file (dante-temp-file-name)) (buffer-file-name) file)
                                    line
                                    (1- col))))))))

(cl-defmethod xref-backend-references ((_backend (eql dante)) symbol)
  (dante-async-load-current-buffer)
  (let ((result (dante-blocking-call (concat ":uses " symbol)))
        (start 0)
        (refs nil))
    (while start
      (if (not (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                             result start))
          (setq start nil)
        (setq start (match-end 0))
        (let ((file (match-string 1 result))
              (line (string-to-number (match-string 2 result)))
              (col (string-to-number (match-string 3 result))))
          (push (xref-make "ref" (xref-make-file-location
                                  (if (string= file (dante-temp-file-name)) (buffer-file-name) file)
                                  line
                                  (1- col)))
                refs))))
    (nreverse refs)))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-fix

(defcustom dante-suggestible-extensions
  '("MultiParamTypeClasses" "RankNTypes" "DeriveGeneric" "DeriveFunctor" "DeriveFoldable" "GADTs" "FlexibleContexts" "FlexibleInstances" "ViewPatterns" "RecordWildcards" "TypeOperators" "TypeFamilies" "FunctionalDependencies" "ScopedTypeVariables")
  "Language extensions that Dante will use to fix errors."
  :group 'dante
  :type '(list string))

(defun dante-auto-fix (pos)
  "Attempt to fix the flycheck error at POS."
  (interactive "d")
  (let ((messages (delq nil (mapcar #'flycheck-error-message
                                    (flycheck-overlay-errors-at pos)))))
    (when messages
      (let ((msg (car messages)))
        (save-excursion
          (cond
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
           (t (message "Cannot fix the issue at point automatically"))))))))

(provide 'dante)

;;; dante.el ends here
