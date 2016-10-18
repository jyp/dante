;;; dante.el --- Development mode for Haskell -*- lexical-binding: t -*-

;; DANTE: Do Not Aim To Expand.

;; This is a mode for GHCi advanced "IDE" features, which does not aim
;; to add other dependencies. It aims to be minimal as far as
;; possible.

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
;; Package-Requires: ((flycheck "0.30") (company "0.8") (emacs "25.1") (haskell-mode "13.0"))

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
;;
;; Mode that enables:
;;
;; * Flycheck type checking
;; * Go to definition
;; * Type of selection
;; * Info
;; TODO
;; * Find uses

;;; Code:

(require 'flycheck)
(require 'json)
(require 'warnings)
(require 'cl-lib)
(require 'company)
(require 'comint)
(require 'widget)
(require 'eldoc)
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

(defcustom dante-environment nil
  "Environment to use: nix or bare ghc(i)."
  :group 'dante
  :type '(choice (const :tag "Nix" nix)
                 (const :tag "Bare" bare)))
(make-local-variable 'dante-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(defvar dante-mode-map (make-sparse-keymap)
  "Dante minor mode's map.")

(defvar-local dante-lighter " Dante"
  "Lighter for the dante minor mode.")

;;;###autoload
(define-minor-mode dante-mode
  "Minor mode for Dante

\\{dante-mode-map}"
  :lighter dante-lighter
  :keymap dante-mode-map
  (when (bound-and-true-p interactive-haskell-mode)
    (when (fboundp 'interactive-haskell-mode)
      (message "Disabling interactive-haskell-mode ...")
      (interactive-haskell-mode -1)))
  (if dante-mode
      (progn (flycheck-select-checker 'dante)
             ;; (flycheck-mode)
             ;; (add-to-list (make-local-variable 'company-backends) 'company-dante)
             ;; (company-mode)
             (setq-local eldoc-documentation-function 'eldoc-dante))
    (message "Dante mode disabled.")))

(define-key dante-mode-map (kbd "C-c C-t") 'dante-type-at)
(define-key dante-mode-map (kbd "C-c C-i") 'dante-info)
(define-key dante-mode-map (kbd "C-c C-l") 'dante-repl-load)
(define-key dante-mode-map (kbd "C-c C-z") 'dante-repl)
(define-key dante-mode-map (kbd "C-c C-r") 'dante-apply-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables/state

(defvar dante-global-mode nil
  "Global mode is enabled?")

(defun global-dante-mode (active)
  "Enable Dante on all Haskell mode buffers."
  (interactive "p")
  (setq dante-global-mode (cl-case active
                            (nil (not dante-global-mode))
                            (1 t)
                            (t nil)))
  (if dante-global-mode
      (add-hook 'haskell-mode-hook 'dante-mode)
    (remove-hook 'haskell-mode-hook 'dante-mode))
  (when (eq this-command 'global-dante-mode)
    (message "Dante mode is now %s on all future Haskell buffers."
             (if dante-global-mode
                 "enabled" "disabled"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local dante-callbacks (list)
  "List of callbacks waiting for output.
LIST is a FIFO.")

(defvar-local dante-arguments (list)
  "Arguments used to call the stack process.")

(defvar-local dante-targets (list)
  "Targets used for the stack process.")

(defvar-local dante-source-buffer (list)
  "Buffer from which Dante was first requested to start.")

(defvar-local dante-project-root nil
  "The project root of the current buffer.

When nil, dante will guess the value using the
`dante-project-root' function.  Hint: It is probably a good idea
to set this as a file or directory variable if the guess is
wrong.")

(defvar-local dante-package-name nil
  "The package name associated with the current buffer.")

(defvar-local dante-deleting nil
  "The process of the buffer is being deleted.")

(defvar-local dante-give-up nil
  "When non-nil, give up trying to start the backend.
A true value indicates that the backend could not start, or could
not be installed.  The user will have to manually run
`dante-restart' or `dante-targets' to destroy the buffer and
create a fresh one without this variable enabled.")

(defvar-local dante-try-with-build nil
  "Try starting dante without --no-build.
This is slower, but will build required dependencies.")

(defvar-local dante-starting nil
  "When non-nil, indicates that the dante process starting up.")

(defvar-local dante-hoogle-port nil
  "Port that hoogle server is listening on.")

(defvar-local dante-suggestions nil
  "Auto actions for the buffer.")

(defvar-local dante-extensions nil
  "Extensions supported by the compiler.")

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
    (when (fboundp 'haskell-mode)
      (haskell-mode))
    (insert expression)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (font-lock-fontify-buffer))
    (buffer-string)))

(defun dante-type-at (insert)
  "Get the type of the thing or selection at point.

With prefix argument INSERT, inserts the type above the current
line as a type signature."
  (interactive "P")
  (let ((ty (apply #'dante-get-type-at (dante-thing-at-point))))
    (if insert
        (save-excursion
          (goto-char (line-beginning-position))
          (insert (dante-fontify-expression ty) "\n"))
      (message
       "%s" (dante-fontify-expression ty)))))

(defun dante-info (ident)
  "Get the info of the thing with IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (let ((origin-buffer (current-buffer))
        (package (dante-package-name))
        (info (dante-get-info-of ident))
        (help-xref-following nil)
        (origin (buffer-name)))
    (help-setup-xref (list #'dante-call-in-buffer origin-buffer 'dante-info ident)
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

(defun dante-restart ()
  "Simply restart the process with the same configuration as before."
  (interactive)
  (when (dante-buffer-p 'backend)
    (let ((targets (with-current-buffer (dante-buffer 'backend)
                     dante-targets)))
      (dante-destroy 'backend)
      (dante-get-worker-create 'backend targets (current-buffer)))))

(defun dante-targets ()
  "Set the targets to use for cabal repl"
  (interactive)
  (let* ((old-targets
          (with-current-buffer (dante-buffer 'backend)
            dante-targets))
         (targets (split-string (read-from-minibuffer "Targets: " nil nil nil nil old-targets)
                                " "
                                t)))
    (dante-destroy)
    (dante-get-worker-create 'backend targets (current-buffer))))

(defun dante-destroy (&optional worker)
  "Stop WORKER and kill its associated process buffer.
If not provided, WORKER defaults to the current worker process."
  (interactive)
  (if worker
      (dante-delete-worker worker)
    (dante-delete-worker 'backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (dante-gave-up 'backend)
      (run-with-timer 0
                      nil
                      cont
                      'interrupted)
    (let ((file-buffer (current-buffer)))
      (dante-async-call
       'backend
       (concat ":l " (dante-temp-file-name))
       (list :cont cont
             :file-buffer file-buffer
             :checker checker)
       (lambda (state string)
         (let ((compile-ok (string-match "OK, modules loaded: \\(.*\\)\\.$" string)))
           (with-current-buffer (plist-get state :file-buffer)
             (let ((modules (match-string 1 string))
                   (msgs (dante-parse-errors-warnings-splices
                          (plist-get state :checker)
                          (current-buffer)
                          string)))
               (dante-collect-compiler-messages msgs)
               (funcall (plist-get state :cont)
                        'finished
                        (cl-remove-if (lambda (msg)
                                        (eq 'splice (flycheck-error-level msg)))
                                      msgs))
               (when compile-ok
                 (dante-async-call 'backend
                                    (concat ":m + "
                                            (replace-regexp-in-string modules "," ""))
                                    nil
                                    (lambda (_st _))))))))))))


(flycheck-define-generic-checker 'dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :modes '(haskell-mode literate-haskell-mode))

(add-to-list 'flycheck-checkers 'dante)

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
                       :buffer (when (string= temp-file file)
                                 buffer)
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
  (with-current-buffer buffer
    (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(defconst dante-pragmas
  '("CONLIKE" "SCC" "DEPRECATED" "INCLUDE" "INCOHERENT" "INLINABLE" "INLINE"
    "LANGUAGE" "LINE" "MINIMAL" "NOINLINE" "NOUNPACK" "OPTIONS" "OPTIONS_GHC"
    "OVERLAPPABLE" "OVERLAPPING" "OVERLAPS" "RULES" "SOURCE" "SPECIALIZE"
    "UNPACK" "WARNING")
  "Pragmas that GHC supports.")

(defun company-dante (command &optional arg &rest ignored)
  "Company source for dante, with the standard COMMAND and ARG args.
Other arguments are IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dante))
    (prefix
     (unless (dante-gave-up 'backend)
       (let ((prefix-info (dante-completions-grab-prefix)))
         (when prefix-info
           (cl-destructuring-bind
               (beg end prefix _type) prefix-info
             prefix)))))
    (candidates
     (unless (dante-gave-up 'backend)
       (let ((prefix-info (dante-completions-grab-prefix)))
         (when prefix-info
           (cons :async
                 (-partial 'dante-company-callback
                           (current-buffer)
                           prefix-info))))))))

(defun dante-company-callback (source-buffer prefix-info cont)
  "Generate completions for SOURCE-BUFFER based on PREFIX-INFO and call CONT on the results."
  (cl-destructuring-bind
      (beg end prefix type) prefix-info
    (or (and (bound-and-true-p dante-mode)
             (cl-case type
               (haskell-completions-module-name-prefix
                (dante-get-repl-completions source-buffer (concat "import " prefix) cont)
                t)
                ;; DISABLED :complete-at is not in ghci
               ;; (haskell-completions-identifier-prefix
               ;;  (dante-get-completions source-buffer beg end cont)
               ;;  t)
               (haskell-completions-language-extension-prefix
                (dante-get-repl-completions
                 source-buffer
                 (concat ":set -X" prefix)
                 (-partial (lambda (cont results)
                             (funcall cont
                                      (mapcar (lambda (x)
                                                (replace-regexp-in-string "^-X" "" x))
                                              results)))
                           cont))
                t)
               (haskell-completions-pragma-name-prefix
                (funcall cont
                         (cl-remove-if-not
                          (lambda (candidate)
                            (string-match (concat "^" prefix) candidate))
                          dante-pragmas))
                t)))
        (dante-get-repl-completions source-buffer prefix cont))))

(defun dante-completions-grab-prefix (&optional minlen)
  "Grab prefix at point for possible completion.
If specified, MINLEN is the shortest completion which will be
considered."
  (when (dante-completions-can-grab-prefix)
    (let ((prefix (cond
                   ((dante-completions-grab-pragma-prefix))
                   ((dante-completions-grab-identifier-prefix)))))
      (cond ((and minlen prefix)
             (when (>= (length (nth 2 prefix)) minlen)
               prefix))
            (prefix prefix)))))

(defun dante-completions-can-grab-prefix ()
  "Check if the case is appropriate for grabbing completion prefix."
  (when (not (region-active-p))
    (when (looking-at-p (rx (| space line-end punct)))
      (when (not (bobp))
        (save-excursion
          (backward-char)
          (not (looking-at-p (rx (| space line-end)))))))))

(defun dante-completions-grab-identifier-prefix ()
  "Grab identifier prefix."
  (let ((pos-at-point (dante-ident-pos-at-point))
        (p (point)))
    (when pos-at-point
      (let* ((start (car pos-at-point))
             (end (cdr pos-at-point))
             (type 'haskell-completions-identifier-prefix)
             (case-fold-search nil)
             value)
        (when (<= p end)
          (setq end p)
          (setq value (buffer-substring-no-properties start end))
          (when (string-match-p (rx bos upper) value)
            (save-excursion
              (goto-char (line-beginning-position))
              (when (re-search-forward
                     (rx "import"
                         (? (1+ space) "qualified")
                         (1+ space)
                         upper
                         (1+ (| alnum ".")))
                     p    ;; bound
                     t)   ;; no-error
                (if (equal p (point))
                    (setq type 'haskell-completions-module-name-prefix)
                  (when (re-search-forward
                         (rx (| " as " "("))
                         start
                         t)
                    (setq type 'haskell-completions-identifier-prefix))))))
          (when (nth 8 (syntax-ppss))
            (setq type 'haskell-completions-general-prefix))
          (when value (list start end value type)))))))

(defun dante-completions-grab-pragma-prefix ()
  "Grab completion prefix for pragma completions.
Returns a list of form '(prefix-start-position
prefix-end-position prefix-value prefix-type) for pramga names
such as WARNING, DEPRECATED, LANGUAGE etc.  Also returns
completion prefixes for options in case OPTIONS_GHC pragma, or
language extensions in case of LANGUAGE pragma.  Obsolete OPTIONS
pragma is supported also."
  (when (nth 4 (syntax-ppss))
    ;; We're inside comment
    (let ((p (point))
          (comment-start (nth 8 (syntax-ppss)))
          (case-fold-search nil)
          prefix-start
          prefix-end
          prefix-type
          prefix-value)
      (save-excursion
        (goto-char comment-start)
        (when (looking-at (rx "{-#" (1+ (| space "\n"))))
          (let ((pragma-start (match-end 0)))
            (when (> p pragma-start)
              ;; point stands after `{-#`
              (goto-char pragma-start)
              (when (looking-at (rx (1+ (| upper "_"))))
                ;; found suitable sequence for pragma name
                (let ((pragma-end (match-end 0))
                      (pragma-value (match-string-no-properties 0)))
                  (if (eq p pragma-end)
                      ;; point is at the end of (in)complete pragma name
                      ;; prepare resulting values
                      (progn
                        (setq prefix-start pragma-start)
                        (setq prefix-end pragma-end)
                        (setq prefix-value pragma-value)
                        (setq prefix-type
                              'haskell-completions-pragma-name-prefix))
                    (when (and (> p pragma-end)
                               (or (equal "OPTIONS_GHC" pragma-value)
                                   (equal "OPTIONS" pragma-value)
                                   (equal "LANGUAGE" pragma-value)))
                      ;; point is after pragma name, so we need to check
                      ;; special cases of `OPTIONS_GHC` and `LANGUAGE` pragmas
                      ;; and provide a completion prefix for possible ghc
                      ;; option or language extension.
                      (goto-char pragma-end)
                      (when (re-search-forward
                             (rx (* anything)
                                 (1+ (regexp "\\S-")))
                             p
                             t)
                        (let* ((str (match-string-no-properties 0))
                               (split (split-string str (rx (| space "\n")) t))
                               (val (car (last split)))
                               (end (point)))
                          (when (and (equal p end)
                                     (not (string-match-p "#" val)))
                            (setq prefix-value val)
                            (backward-char (length val))
                            (setq prefix-start (point))
                            (setq prefix-end end)
                            (setq
                             prefix-type
                             (if (not (equal "LANGUAGE" pragma-value))
                                 'haskell-completions-ghc-option-prefix
                               'haskell-completions-language-extension-prefix
                               )))))))))))))
      (when prefix-value
        (list prefix-start prefix-end prefix-value prefix-type)))))

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
  (apply #'dante-get-type-at-async
         (lambda (beg end ty)
           (let ((response-status (dante-haskell-utils-repl-response-error-status ty)))
             (if (eq 'no-error response-status)
               (let ((msg (dante-fontify-expression
                           (replace-regexp-in-string "[ \n]+" " " ty))))
                 ;; Got an updated type-at-point, cache and print now:
                 (puthash (list beg end)
                          msg
                          eldoc-dante-cache)
                 (eldoc-dante-maybe-print msg))
               ;; But if we're seeing errors, invalidate cache-at-point:
               (remhash (list beg end) eldoc-dante-cache))))
         (dante-thing-at-point))
  ;; If we have something cached at point, print that first:
  (gethash (dante-thing-at-point) eldoc-dante-cache))

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
;; REPL

(defconst dante-prompt-regexp "^\4 ")

(defvar-local dante-repl-previous-buffer nil
  "Records the buffer to which `dante-repl-switch-back' should jump.
This is set by `dante-repl-buffer', and should otherwise be nil.")

(defun dante-repl-clear-buffer ()
  "Clear the current REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun dante-repl-load (&optional prompt-options)
  "Load the current file in the REPL.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (save-buffer)
  (let ((file (dante-temp-file-name))
        (repl-buffer (dante-repl-buffer prompt-options t)))
    (with-current-buffer repl-buffer
      (comint-simple-send
       (get-buffer-process (current-buffer))
       (concat ":l " file)))
    (pop-to-buffer repl-buffer)))

(defun dante-repl (&optional prompt-options)
  "Start up the REPL for this stack project.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (switch-to-buffer-other-window (dante-repl-buffer prompt-options t)))

(defun dante-repl-buffer (prompt-options &optional store-previous)
  "Start the REPL buffer.
If PROMPT-OPTIONS is non-nil, prompt with an options list.  When
STORE-PREVIOUS is non-nil, note the caller's buffer in
`dante-repl-previous-buffer'."
  (let* ((root (dante-project-root))
         (package-name (dante-package-name))
         (name (format "*dante:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name))
         (initial-buffer (current-buffer))
         (backend-buffer (dante-buffer 'backend)))
    (with-current-buffer
        (if (get-buffer name)
            (get-buffer name)
          (with-current-buffer
              (get-buffer-create name)
            (cd root)
            (dante-repl-mode)
            (dante-repl-mode-start backend-buffer
                                    (buffer-local-value 'dante-targets backend-buffer)
                                    prompt-options)
            (current-buffer)))
      (progn
        (when store-previous
          (setq dante-repl-previous-buffer initial-buffer))
        (current-buffer)))))

(defvar dante-hyperlink-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]  'dante-find-file-with-line:char)
    (define-key map [C-return] 'dante-find-file-with-line:char)
    map)
  "Keymap for clicking on links in REPL.")

(defun dante-find-file-with-line:char ()
  "Jump to the file and location indicated by text properties at point."
  (interactive)
  (let ((file (get-text-property (point) 'file))
        (line (get-text-property (point) 'line))
        (char (get-text-property (point) 'char)))
    (with-no-warnings (find-file-other-window file))
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- char))))

(defun dante-linkify-file-line-char (begin end)
  "Linkify all occurences of <file>:<line>:<char>: betwen BEGIN and END."
  (when (> end begin)
    (let ((end-marker (copy-marker end))
          ;; match - /path/to/file.ext:<line>:<char>:
          ;;       - /path/to/file.ext:<line>:<char>-
          ;;       - /path/to/file.ext:(<line>:<char>)
          (file:line:char-regexp "\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\)[:](?\\([0-9]+\\)[:,]\\([0-9]+\\)[:)-]"))
      (save-excursion
        (goto-char begin)
        ;; Delete unrecognized escape sequences.
        (while (re-search-forward file:line:char-regexp end-marker t)
          (let ((file (match-string-no-properties 1))
                (line (match-string-no-properties 2))
                (char (match-string-no-properties 3))
                (link-start (1+ (match-beginning 1)))
                (link-end   (1+ (match-end 2))))
            (add-text-properties
             link-start link-end
             (list 'keymap dante-hyperlink-map
                   'file   file
                   'line   (string-to-number line)
                   'char   (string-to-number char)
                   'help-echo "mouse-2: visit this file"))))))))

(defvar dante-last-output-newline-marker nil)

(defun dante-linkify-process-output (_)
  "Comint filter function to make <file>:<line>:<char>: into clickable links.

Note that this function uses the `dante-last-output-newline-marker',
to keep track of line breaks.  The `dante-linkify-file-line-char'
function is subsequently applied to each line, once."
  (unless dante-last-output-newline-marker
    (setq-local dante-last-output-newline-marker (make-marker))
    (set-marker dante-last-output-newline-marker (marker-position comint-last-output-start)))
  (let ((start-marker (if (and (markerp dante-last-output-newline-marker)
                               (eq (marker-buffer dante-last-output-newline-marker)
                                   (current-buffer))
                               (marker-position dante-last-output-newline-marker))
                          comint-last-output-start
                        (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward "[\n\r]" end-marker t)
        (dante-linkify-file-line-char dante-last-output-newline-marker (match-beginning 0))
        (set-marker dante-last-output-newline-marker (match-end 0))))))

(define-derived-mode dante-repl-mode comint-mode "Dante-REPL"
  "Interactive prompt for Dante."
  (when (and (not (eq major-mode 'fundamental-mode))
             (eq this-command 'dante-repl-mode))
    (error "You probably meant to run: M-x dante-repl"))
  (setq-local comint-prompt-regexp dante-prompt-regexp)
  (setq-local warning-suppress-types (cons '(undo discard-info) warning-suppress-types))
  (add-hook 'comint-output-filter-functions
            'dante-linkify-process-output
            t)
  (setq-local comint-prompt-read-only t)
  (add-to-list (make-local-variable 'company-backends) 'company-dante)
  (company-mode))

(defun dante-repl-mode-start (backend-buffer targets prompt-options)
  "Start the process for the repl in the current buffer.
BACKEND-BUFFER is used for options.
TARGETS is the targets to load.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (setq dante-targets targets)
  (let ((arguments (dante-make-options-list ;; FIXME
                    (or targets
                        (let ((package-name (buffer-local-value 'dante-package-name
                                                                backend-buffer)))
                          (unless (equal "" package-name)
                            (list package-name)))))))
    (insert (propertize
             (format "Starting:\n  stack ghci %s\n" (combine-and-quote-strings arguments))
             'face 'font-lock-comment-face))
    (let ((script (with-current-buffer (find-file-noselect (make-temp-file "dante-script"))
                    (insert ":set prompt \"\"
:set -fobject-code
:set prompt \"\\4 \"
")
                    (basic-save-buffer)
                    (dante-buffer-file-name))))
      (let ((process (get-buffer-process (apply #'make-comint-in-buffer "dante" (current-buffer) "FIXME" nil "ghci"
                                                (append arguments
                                                        (list "--ghci-options"
                                                              (concat "-ghci-script=" script)))))))
        (when (process-live-p process)
          (set-process-query-on-exit-flag process nil)
          (message "Started Dante process for REPL."))))))

(font-lock-add-keywords
 'dante-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?λ))))))

(define-key dante-repl-mode-map [remap move-beginning-of-line] 'dante-repl-beginning-of-line)
(define-key dante-repl-mode-map [remap delete-backward-char] 'dante-repl-delete-backward-char)
(define-key dante-repl-mode-map (kbd "C-c C-k") 'dante-repl-clear-buffer)
(define-key dante-repl-mode-map (kbd "C-c C-z") 'dante-repl-switch-back)

(defun dante-repl-delete-backward-char ()
  "Delete backwards, excluding the prompt."
  (interactive)
  (unless (looking-back dante-prompt-regexp (line-beginning-position))
    (call-interactively 'delete-backward-char)))

(defun dante-repl-beginning-of-line ()
  "Go to the beginning of the line, excluding the prompt."
  (interactive)
  (if (search-backward-regexp dante-prompt-regexp (line-beginning-position) t 1)
      (goto-char (+ 2 (line-beginning-position)))
    (call-interactively 'move-beginning-of-line)))

(defun dante-repl-switch-back ()
  "Switch back to the buffer from which this REPL buffer was reached."
  (interactive)
  (if dante-repl-previous-buffer
      (switch-to-buffer-other-window dante-repl-previous-buffer)
    (message "No previous buffer.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer operations

(defun dante-thing-at-point ()
  "Return (list START END) of something at the point."
  (if (region-active-p)
      (list (region-beginning)
            (region-end))
    (let ((pos (dante-ident-pos-at-point)))
      (if pos
          (list (car pos) (cdr pos))
        (list (point) (point))))))

(defun dante-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (dante-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

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
          (cons start end))))))

(defun dante-buffer-file-name (&optional buffer)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (let ((name (buffer-file-name buffer)))
    (when name
      (dante-canonicalize-path (substring-no-properties name)))))

(defvar-local dante-temp-file-name nil
  "The name of a temporary file to which the current buffer's content is copied.")

(defun dante-temp-file-name (&optional buffer)
  "Return the name of a temp file containing an up-to-date copy of BUFFER's contents."
  (with-current-buffer (or buffer (current-buffer))
    (prog1
        (or dante-temp-file-name
            (setq dante-temp-file-name
                  (dante-canonicalize-path (make-temp-file "dante" nil ".hs"))))
      (let ((contents (buffer-string)))
        (with-temp-file dante-temp-file-name
          (insert contents))))))

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

(defun dante-get-type-at (beg end)
  "Get the type at the given region denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (dante-blocking-call
    'backend
    (dante-format-get-type-at beg end))))

(defun dante-get-type-at-async (cont beg end)
  "Call CONT with type of the region denoted by BEG and END.
CONT is called within the current buffer, with BEG, END and the
type as arguments."
  (dante-async-call
   'backend
   (dante-format-get-type-at beg end)
   (list :cont cont
         :source-buffer (current-buffer)
         :beg beg
         :end end)
   (lambda (state reply)
     (with-current-buffer (plist-get state :source-buffer)
       (funcall (plist-get state :cont)
                (plist-get state :beg)
                (plist-get state :end)
                (replace-regexp-in-string "\n$" "" reply))))))

(defun dante-format-get-type-at (beg end)
  "Compose a request for getting types in region from BEG to END."
  (format ":type-at %S %d %d %d %d %S"
          (dante-temp-file-name)
          (save-excursion (goto-char beg)
                          (line-number-at-pos))
          (save-excursion (goto-char beg)
                          (1+ (current-column)))
          (save-excursion (goto-char end)
                          (line-number-at-pos))
          (save-excursion (goto-char end)
                          (1+ (current-column)))
          (buffer-substring-no-properties beg end)))

(defun dante-get-info-of (thing)
  "Get info for THING."
  (let ((optimistic-result
         (replace-regexp-in-string
          "\n$" ""
          (dante-blocking-call
           'backend
           (format ":i %s" thing)))))
    (if (string-match "^<interactive>" optimistic-result)
        ;; Load the module Interpreted so that we get information,
        ;; then restore bytecode.
        (progn (dante-async-call
                'backend
                ":set -fbyte-code")
               (set-buffer-modified-p t)
               (save-buffer)
               (unless (member 'save flycheck-check-syntax-automatically)
                 (dante-async-call
                  'backend
                  (concat ":l " (dante-temp-file-name))))
               (dante-async-call
                'backend
                ":set -fobject-code")
               (replace-regexp-in-string
                "\n$" ""
                (dante-blocking-call
                 'backend
                 (format ":i %s" thing))))
      optimistic-result)))

(defun dante-get-loc-at (beg end)
  "Get the location of the identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (dante-blocking-call
    'backend
    (format ":loc-at %S %d %d %d %d %S"
            (dante-temp-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun dante-get-uses-at (beg end)
  "Return usage list for identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (dante-blocking-call
    'backend
    (format ":uses %S %d %d %d %d %S"
            (dante-temp-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun dante-get-completions (source-buffer beg end cont)
  "Get completions and send to SOURCE-BUFFER.
Prefix is marked by positions BEG and END.  Completions are
passed to CONT in SOURCE-BUFFER."
  (dante-async-call
   'backend
   (format ":complete-at %S %d %d %d %d %S"
           (dante-temp-file-name)
           (save-excursion (goto-char beg)
                           (line-number-at-pos))
           (save-excursion (goto-char beg)
                           (1+ (current-column)))
           (save-excursion (goto-char end)
                           (line-number-at-pos))
           (save-excursion (goto-char end)
                           (1+ (current-column)))
           (buffer-substring-no-properties beg end))
   (list :cont cont :source-buffer source-buffer)
   (lambda (state reply)
     (with-current-buffer
         (plist-get state :source-buffer)
       (funcall
        (plist-get state :cont)
        (if (string-match "^*** Exception" reply)
            (list)
          (mapcar
           (lambda (x)
             (replace-regexp-in-string "\\\"" "" x))
           (split-string reply "\n" t))))))))

(defun dante-get-repl-completions (source-buffer prefix cont)
  "Get REPL completions and send to SOURCE-BUFFER.
Completions for PREFIX are passed to CONT in SOURCE-BUFFER."
  (dante-async-call
   'backend
   (format ":complete repl %S" prefix)
   (list :cont cont :source-buffer source-buffer)
   (lambda (state reply)
     (with-current-buffer
         (plist-get state :source-buffer)
       (funcall
        (plist-get state :cont)
        (mapcar
         (lambda (x)
           (replace-regexp-in-string "\\\"" "" x))
         (cdr (split-string reply "\n" t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun dante-delete-worker (worker)
  "Delete the given WORKER."
  (when (dante-buffer-p worker)
    (with-current-buffer (dante-get-buffer-create worker)
      (when (get-buffer-process (current-buffer))
        (setq dante-deleting t)
        (kill-process (get-buffer-process (current-buffer)))
        (delete-process (get-buffer-process (current-buffer))))
      (kill-buffer (current-buffer)))))

(defun dante-blocking-call (worker cmd)
  "Send WORKER the command string CMD and block pending its result."
  (let ((result (list nil)))
    (dante-async-call
     worker
     cmd
     result
     (lambda (result reply)
       (setf (car result) reply)))
    (with-current-buffer (dante-buffer worker)
      (while (not (null dante-callbacks))
        (sleep-for 0.0001)))
    (car result)))

(defun dante-async-call (worker cmd &optional state callback)
  "Send WORKER the command string CMD.
The result, along with the given STATE, is passed to CALLBACK
as (CALLBACK STATE REPLY)."
  (let ((buffer (dante-buffer worker)))
    (if (and buffer (process-live-p (get-buffer-process buffer)))
        (progn (with-current-buffer buffer
                 (setq dante-callbacks
                       (append dante-callbacks
                               (list (list state
                                           (or callback #'ignore)
                                           cmd)))))
               (when dante-debug
                 (message "[Dante] -> %s" cmd))
               (comint-simple-send (dante-process worker) cmd))
      (error "Dante process is not running: run M-x dante-restart to start it"))))

(defun dante-buffer (worker)
  "Get the WORKER buffer for the current directory."
  (let ((buffer (dante-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (dante-get-worker-create worker nil (current-buffer)))))

(defun dante-process (worker)
  "Get the WORKER process for the current directory."
  (get-buffer-process (dante-buffer worker)))

(defun dante-get-worker-create (worker &optional targets source-buffer)
  "Start the given WORKER.
If provided, use the specified TARGETS and SOURCE-BUFFER."
  (let* ((buffer (dante-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (dante-start-process-in-buffer buffer targets source-buffer))))

(defun dante-environment ()
  (if dante-environment dante-environment
    (if (file-exists-p (concat (dante-project-root) "shell.nix"))
        'nix
      'bare)))

(defun dante-command-line (cmd)
  (cl-case (dante-environment)
    (nix (list "nix-shell" "--run" (combine-and-quote-strings cmd)))
    (bare cmd)))

(defun dante-start-process-in-buffer (buffer &optional targets source-buffer)
  "Start a Dante worker in BUFFER, for the default or specified TARGETS.
Automatically performs initial actions in SOURCE-BUFFER, if specified."
  (if (buffer-local-value 'dante-give-up buffer)
      buffer
    (let* ((args (dante-command-line (list "cabal" "repl")))
           (process (with-current-buffer buffer
                      (when dante-debug
                        (message "Dante command line: %s" (combine-and-quote-strings args)))
                      (message "Booting up dante ...")
                      (apply #'start-process "dante" buffer args))))
      (set-process-query-on-exit-flag process nil)
      (process-send-string process ":set +c\n")
      (process-send-string process ":set -fbyte-code\n") ;; when this is set one cannot get the types of functions
      (process-send-string process ":set prompt \"\\4\"\n")
      (with-current-buffer buffer
        (erase-buffer)
        (setq dante-arguments args)
        (setq dante-targets targets)
        (setq dante-source-buffer source-buffer)
        (setq dante-starting t)
        (setq dante-callbacks
              (list (list (cons source-buffer
                                buffer)
                          (lambda (buffers _msg)
                            (let ((source-buffer (car buffers))
                                  (process-buffer (cdr buffers)))
                              (with-current-buffer process-buffer
                                (setq-local dante-starting nil))
                              (when source-buffer
                                (with-current-buffer source-buffer
                                  (when flycheck-mode
                                    (run-with-timer 0 nil
                                                    'dante-call-in-buffer
                                                    (current-buffer)
                                                    'dante-flycheck-buffer)))))
                            (message "Booted up dante!"))))))
      (set-process-filter
       process
       (lambda (process string)
         (when dante-debug
           (message "[Dante] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string)
             (when (and dante-try-with-build
                        dante-starting)
               (let ((last-line (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
                 (if (string-match "^Progress" last-line)
                     (message "Booting up dante (building dependencies: %s)"
                              (downcase
                               (or (car (split-string (replace-regexp-in-string
                                                       "\u0008+" "\n"
                                                       last-line)
                                                      "\n" t))
                                   "...")))
                   (message "Booting up dante ..."))))
             (dante-read-buffer)))))
      (set-process-sentinel process 'dante-sentinel)
      buffer)))

(defun dante-flycheck-buffer ()
  "Run flycheck in the buffer.
Restarts flycheck in case there was a problem and flycheck is stuck."
  (flycheck-mode -1)
  (flycheck-mode)
  (flycheck-buffer))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (when (buffer-live-p (process-buffer process))
    (when (and (not (process-live-p process)))
      (let ((buffer (process-buffer process)))
        (if (with-current-buffer buffer dante-deleting)
            (message "Dante process deleted.")
          (if (and (dante-unsatisfied-package-p buffer)
                   (not (buffer-local-value 'dante-try-with-build buffer)))
              (progn (with-current-buffer buffer (setq-local dante-try-with-build t))
                     (dante-start-process-in-buffer
                      buffer
                      (buffer-local-value 'dante-targets buffer)
                      (buffer-local-value 'dante-source-buffer buffer)))
            (progn (with-current-buffer buffer (setq-local dante-give-up t))
                   (dante-show-process-problem process change))))))))

(defun dante-unsatisfied-package-p (buffer)
  "Return non-nil if BUFFER contain GHCi's unsatisfied package complaint."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "cannot satisfy -package" nil t 1))))

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
  (let ((repeat t))
    (while repeat
      (setq repeat nil)
      (goto-char (point-min))
      (when (search-forward "\4" (point-max) t 1)
        (let* ((next-callback (pop dante-callbacks))
               (state (nth 0 next-callback))
               (func (nth 1 next-callback)))
          (let ((string (strip-carriage-returns (buffer-substring (point-min) (1- (point))))))
            (if next-callback
                (progn (with-temp-buffer
                         (funcall func state string))
                       (setq repeat t))
              (when dante-debug
                (dante--warn "Received output but no callback in `dante-callbacks': %S"
                      string)))))
        (delete-region (point-min) (point))))))

(defun strip-carriage-returns (string)
  "Strip the \\r from Windows \\r\\n line endings in STRING."
  (replace-regexp-in-string "\r" "" string))

(defun dante-get-buffer-create (worker)
  "Get or create the stack buffer for WORKER.
Uses the directory of the current buffer for context."
  (let* ((root (dante-project-root))
         (cabal-file (dante-cabal-find-file))
         (package-name (if cabal-file
                           (dante-package-name cabal-file)
                         ""))
         (buffer-name (dante-buffer-name worker))
         (default-directory (if cabal-file
                                (file-name-directory cabal-file)
                              root)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq dante-package-name package-name)
      (cd default-directory)
      (current-buffer))))

(defun dante-gave-up (worker)
  "Return non-nil if starting WORKER or installing dante failed."
  (and (dante-buffer-p worker)
       (let ((buffer (get-buffer (dante-buffer-name worker))))
         (buffer-local-value 'dante-give-up buffer))))

(defun dante-buffer-p (worker)
  "Return non-nil if a buffer exists for WORKER."
  (get-buffer (dante-buffer-name worker)))

(defun dante-buffer-name (worker)
  "For a given WORKER, create a buffer name."
  (let* ((root (dante-project-root))
         (package-name (dante-package-name)))
    (concat " dante:"
            (format "%s" worker)
            ":"
            package-name
            " "
            root)))

(defun dante-project-root ()
  "Get the current stack config directory.
This is the directory where the .cabal file is placed for
this project."
  (if dante-project-root
      dante-project-root
    (file-name-directory (or (dante-cabal-find-file) (dante-buffer-file-name)))))

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
;; Multiselection

(defun dante-multiswitch (title options)
  "Displaying TITLE, read multiple flags from a list of OPTIONS.
Each option is a plist of (:key :default :title) wherein:

  :key should be something comparable with EQUAL
  :title should be a string
  :default (boolean) specifies the default checkedness"
  (let ((available-width (window-total-width)))
    (save-window-excursion
      (with-temp-buffer
        (rename-buffer (generate-new-buffer-name "multiswitch"))
        (widget-insert (concat title "\n\n"))
        (widget-insert (propertize "Hit " 'face 'font-lock-comment-face))
        (widget-create 'push-button :notify
                       (lambda (&rest ignore)
                         (exit-recursive-edit))
                       "C-c C-c")
        (widget-insert (propertize " to apply these choices.\n\n" 'face 'font-lock-comment-face))
        (let* ((me (current-buffer))
               (choices (mapcar (lambda (option)
                                  (append option (list :value (plist-get option :default))))
                                options)))
          (cl-loop for option in choices
                   do (widget-create
                       'toggle
                       :notify (lambda (widget &rest ignore)
                                 (setq choices
                                       (mapcar (lambda (choice)
                                                 (if (equal (plist-get choice :key)
                                                            (plist-get (cdr widget) :key))
                                                     (plist-put choice :value (plist-get (cdr widget) :value))
                                                   choice))
                                               choices)))
                       :on (concat "[x] " (plist-get option :title))
                       :off (concat "[ ] " (plist-get option :title))
                       :value (plist-get option :default)
                       :key (plist-get option :key)))
          (let ((lines (line-number-at-pos)))
            (select-window (split-window-below))
            (switch-to-buffer me)
            (goto-char (point-min)))
          (use-local-map
           (let ((map (copy-keymap widget-keymap)))
             (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
             (define-key map (kbd "C-g") 'abort-recursive-edit)
             map))
          (widget-setup)
          (recursive-edit)
          (kill-buffer me)
          (mapcar (lambda (choice)
                    (plist-get choice :key))
                  (cl-remove-if-not (lambda (choice)
                                      (plist-get choice :value))
                                    choices)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hoogle

(defun dante-hoogle-blocking-query (query)
  "Make a request of QUERY using the local hoogle server.
If running, otherwise returns nil.

It is the responsibility of the caller to make sure the server is
running; the user might not want to start the server
automatically."
  (let ((buffer (dante-hoogle-get-buffer)))
    (when buffer
      (let ((url (dante-hoogle-url buffer query)))
        (with-current-buffer (url-retrieve-synchronously url t)
          (search-forward "\n\n" nil t 1)
          (json-read-from-string
           (buffer-substring (line-beginning-position)
                             (line-end-position))))))))

(defun dante-hoogle-url (buffer query)
  "Via hoogle server BUFFER make the HTTP URL for QUERY."
  (format "http://127.0.0.1:%d/?hoogle=%s&mode=json"
          (buffer-local-value 'dante-hoogle-port buffer)
          (url-encode-url query)))

(defun dante-hoogle-get-worker-create ()
  "Get or create the hoogle worker."
  (let* ((buffer (dante-hoogle-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (dante-start-hoogle-process-in-buffer buffer))))

(defun dante-start-hoogle-process-in-buffer (buffer)
  "Start the process in BUFFER, returning BUFFER."
  (let* ((port (dante-free-port))
         (process (with-current-buffer buffer
                    (message "Booting up hoogle ...")
                    (setq dante-hoogle-port port)
                    (start-process "hoogle"
                                   buffer
                                   "hoogle"
                                   "server"
                                   "--local"
                                   "--port"
                                   (number-to-string port)))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process 'dante-hoogle-sentinel)
    buffer))

(defun dante-free-port ()
  "Get the next free port to use."
  (let ((proc (make-network-process
               :name "port-check"
               :family 'ipv4
               :host "127.0.0.1"
               :service t
               :server t)))
    (delete-process proc)
    (process-contact proc :service)))

(defun dante-hoogle-sentinel (process change)
  "For the hoogle PROCESS there is a CHANGE to handle."
  (message "Hoogle sentinel: %S %S" process change))

(defun dante-hoogle-get-buffer-create ()
  "Get or create the Hoogle buffer for the current stack project."
  (let* ((root (dante-project-root))
         (buffer-name (dante-hoogle-buffer-name root))
         (buf (get-buffer buffer-name))
         (default-directory root))
    (if buf
        buf
      (with-current-buffer (get-buffer-create buffer-name)
        (cd default-directory)
        (current-buffer)))))

(defun dante-hoogle-get-buffer ()
  "Get the Hoogle buffer for the current stack project."
  (let* ((root (dante-project-root))
         (buffer-name (dante-hoogle-buffer-name root)))
    (get-buffer buffer-name)))

(defun dante-hoogle-buffer-name (root)
  "For a given worker, create a buffer name using ROOT."
  (concat "*Hoogle:" root "*"))

(defun dante-hoogle-ready-p ()
  "Is hoogle ready to be started?"
  (with-temp-buffer
    (cl-case (call-process "stack" nil (current-buffer) t
                           "hoogle" "--no-setup" "--verbosity" "silent")
      (0 t))))

(defun dante-hoogle-supported-p ()
  "Is the stack hoogle command supported?"
  (with-temp-buffer
    (cl-case (call-process "stack" nil (current-buffer) t
                           "hoogle" "--help")
      (0 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting information from compiler messages

(defun dante-collect-compiler-messages (msgs)
  "Collect information from compiler MSGS.

This may update in-place the MSGS objects to hint that
suggestions are available."
  (setq dante-suggestions nil)
  (let ((extension-regex (regexp-opt (dante-extensions)))
        (quoted-symbol-regex "[‘`‛]\\([^ ]+\\)['’]"))
    (cl-loop
     for msg in msgs
     do (let ((text (flycheck-error-message msg))
              (note nil))
          ;; Messages of this format:
          ;;
          ;; Can't make a derived instance of ‘Functor X’:
          ;;       You need DeriveFunctor to derive an instance for this class
          ;;       Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
          ;;       In the newtype declaration for ‘X’
          (let ((start 0))
            (while (string-match extension-regex text start)
              (setq note t)
              (add-to-list 'dante-suggestions
                           (list :type 'add-extension
                                 :extension (match-string 0 text)))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;; Defaulting the following constraint(s) to type ‘Integer’
          ;;   (Num a0) arising from the literal ‘1’
          ;; In the expression: 2
          ;; In an equation for ‘x'’: x' = 2
          (let ((start 0))
            (while (string-match
                    " Defaulting the following constraint" text start)
              (setq note t)
              (add-to-list 'dante-suggestions
                           (list :type 'add-ghc-option
                                 :option "-fno-warn-type-defaults"))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;;     This binding for ‘x’ shadows the existing binding
          (let ((start 0))
            (while (string-match
                    " This binding for ‘\\(.*\\)’ shadows the existing binding" text start)
              (setq note t)
              (add-to-list 'dante-suggestions
                           (list :type 'add-ghc-option
                                 :option "-fno-warn-name-shadowing"))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;; The import of ‘Control.Monad’ is redundant
          ;;   except perhaps to import instances from ‘Control.Monad’
          ;; To import instances alone, use: import Control.Monad()... (dante)
          (when (string-match
                 " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant"
                 text)
            (setq note t)
            (add-to-list 'dante-suggestions
                         (list :type 'remove-import
                               :module (match-string 2 text)
                               :line (flycheck-error-line msg))))
          ;; Messages of this format:
          ;;
          ;; Not in scope: ‘putStrn’
          ;; Perhaps you meant one of these:
          ;;   ‘putStr’ (imported from Prelude),
          ;;   ‘putStrLn’ (imported from Prelude)
          ;;
          ;; Or this format:
          ;;
          ;; error:
          ;;    • Variable not in scope: lopSetup :: [Statement Exp']
          ;;    • Perhaps you meant ‘loopSetup’ (line 437)
          (when (string-match
                 "[Nn]ot in scope: \\(data constructor \\|type constructor or class \\)?[‘`‛]?\\([^'’ ]+\\).*\n.*Perhaps you meant"
                 text)
            (let ((typo (match-string 2 text))
                  (start (min (length text) (1+ (match-end 0)))))
              (while (string-match quoted-symbol-regex text start)
                (setq note t)
                (add-to-list 'dante-suggestions
                             (list :type 'fix-typo
                                   :typo typo
                                   :replacement (match-string 1 text)
                                   :column (flycheck-error-column msg)
                                   :line (flycheck-error-line msg)))
                (setq start (min (length text) (1+ (match-end 0)))))))
          ;; Messages of this format:
          ;;
          ;;     Top-level binding with no type signature: main :: IO ()
          (when (string-match
                 "Top-level binding with no type signature:"
                 text)
            (let ((start (min (length text) (match-end 0))))
              (setq note t)
              (add-to-list 'dante-suggestions
                           (list :type 'add-signature
                                 :signature (mapconcat #'identity (split-string (substring text start)) " ")
                                 :line (flycheck-error-line msg)))))
          ;; Messages of this format:
          ;;
          ;;     Redundant constraints: (Arith var, Bitwise var)
          ;; Or
          ;;     Redundant constraint: Arith var
          ;; Or
          ;;     Redundant constraints: (Arith var,
          ;;                             Bitwise var,
          ;;                             Functor var,
          ;;                             Applicative var,
          ;;                             Monad var)
          (when (string-match "Redundant constraints?: " text)
            (let* ((redundant-start (match-end 0))
                   (parts (with-temp-buffer
                            (insert (substring text redundant-start))
                            (goto-char (point-min))
                            ;; A lone unparenthesized constraint might
                            ;; be multiple sexps.
                            (while (not (eq (point) (point-at-eol)))
                              (forward-sexp))
                            (let ((redundant-end (point)))
                              (search-forward-regexp ".*\n.*In the ")
                              (cons (buffer-substring (point-min) redundant-end)
                                    (buffer-substring (match-end 0) (point-max)))))))
              (setq note t)
              (add-to-list
               'dante-suggestions
               (let ((rest (cdr parts))
                     (redundant (let ((raw (car parts)))
                                  (if (eq (string-to-char raw) ?\()
                                      (substring raw 1 (1- (length raw)))
                                    raw))))
                 (list :type 'redundant-constraint
                       :redundancies (mapcar #'string-trim
                                             (dante-parse-comma-list redundant))
                       :signature (mapconcat #'identity (split-string rest) " ")
                       :line (flycheck-error-line msg))))))
          ;; Add a note if we found a suggestion to make
          (when note
            (setf (flycheck-error-message msg)
                  (concat text
                          "\n\n"
                          (propertize "(Hit `C-c C-r' in the Haskell buffer to apply suggestions)"
                                      'face 'font-lock-warning-face)))))))
  (setq dante-lighter
        (if (null dante-suggestions)
            " Dante"
          (format " Dante:%d" (length dante-suggestions)))))

(defun dante-extensions ()
  "Get extensions for the current project's GHC."
  (with-current-buffer (dante-buffer 'backend)
    (or dante-extensions
        (setq dante-extensions
              (split-string
               (shell-command-to-string
                (combine-and-quote-strings
                 (dante-command-line (list "ghc" "--supported-extensions")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto actions

(defun dante-parse-comma-list (text)
  "Parse a list of comma-separated expressions in TEXT."
  (cl-loop for tok in (split-string text "[[:space:]\n]*,[[:space:]\n]*")
           with acc = nil
           append (let* ((clist (string-to-list tok))
                         (num-open (-count (lambda (c) (or (eq c ?\() (eq c ?\[)))
                                           clist))
                         (num-close (-count (lambda (c) (or (eq c ?\)) (eq c ?\])))
                                            clist)))
                    (cond
                     ((> num-open num-close) (progn (add-to-list 'acc tok) nil))
                     ((> num-close num-open) (let ((tmp (reverse (cons tok acc))))
                                               (setq acc nil)
                                               (list (string-join tmp ", "))))
                     (t (list tok))))))

(defun dante-apply-suggestions ()
  "Prompt and apply the suggestions."
  (interactive)
  (if (null dante-suggestions)
      (message "No suggestions to apply")
    (let ((to-apply
           (dante-multiswitch
            (format "There are %d suggestions to apply:" (length dante-suggestions))
            (cl-remove-if-not
             #'identity
             (mapcar
              (lambda (suggestion)
                (cl-case (plist-get suggestion :type)
                  (add-extension
                   (list :key suggestion
                         :title (concat "Add {-# LANGUAGE "
                                        (plist-get suggestion :extension)
                                        " #-}")
                         :default t))
                  (add-ghc-option
                   (list :key suggestion
                         :title (concat "Add {-# OPTIONS_GHC "
                                        (plist-get suggestion :option)
                                        " #-}")
                         :default t))
                  (remove-import
                   (list :key suggestion
                         :title (concat "Remove: import "
                                        (plist-get suggestion :module))
                         :default t))
                  (fix-typo
                   (list :key suggestion
                         :title (concat "Replace ‘"
                                        (plist-get suggestion :typo)
                                        "’ with ‘"
                                        (plist-get suggestion :replacement)
                                        "’")
                         :default (null (cdr dante-suggestions))))
                  (add-signature
                   (list :key suggestion
                         :title (concat "Add signature: "
                                        (plist-get suggestion :signature))
                         :default t))
                  (redundant-constraint
                   (list :key suggestion
                         :title (concat
                                 "Remove redundant constraints: "
                                 (string-join (plist-get suggestion :redundancies)
                                              ", ")
                                 "\n    from the "
                                 (plist-get suggestion :signature))
                         :default nil))))
              dante-suggestions)))))
      (if (null to-apply)
          (message "No changes selected to apply.")
        (let ((sorted (sort to-apply
                            (lambda (lt gt)
                              (let ((lt-line   (or (plist-get lt :line)   0))
                                    (lt-column (or (plist-get lt :column) 0))
                                    (gt-line   (or (plist-get gt :line)   0))
                                    (gt-column (or (plist-get gt :column) 0)))
                                (or (> lt-line gt-line)
                                    (and (= lt-line gt-line)
                                         (> lt-column gt-column))))))))
          ;; # Changes that do not increase/decrease line numbers
          ;;
          ;; Update in-place suggestions
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (fix-typo
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (move-to-column (- (plist-get suggestion :column) 1))
                   (delete-char (length (plist-get suggestion :typo)))
                   (insert (plist-get suggestion :replacement))))))
          ;; # Changes that do increase/decrease line numbers
          ;;
          ;; Remove redundant constraints
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (redundant-constraint
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (search-forward-regexp "[[:alnum:][:space:]\n]*=>")
                   (backward-sexp 2)
                   (let ((start (1+ (point))))
                     (forward-sexp)
                     (let* ((end (1- (point)))
                            (constraints (dante-parse-comma-list
                                          (buffer-substring start end)))
                            (nonredundant
                             (cl-loop for r in (plist-get suggestion :redundancies)
                                      with nonredundant = constraints
                                      do (setq nonredundant (delete r nonredundant))
                                      finally return nonredundant)))
                       (goto-char start)
                       (delete-char (- end start))
                       (insert (string-join nonredundant ", "))))))))

          ;; Add a type signature to a top-level binding.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-signature
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (insert (plist-get suggestion :signature))
                   (insert "\n")))))

          ;; Remove import lines from the file. May remove more than one
          ;; line per import.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (remove-import
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (delete-region (line-beginning-position)
                                  (or (when (search-forward-regexp "\n[^ \t]" nil t 1)
                                        (1- (point)))
                                      (line-end-position)))))))
          ;; Add extensions to the top of the file
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-extension
                 (save-excursion
                   (goto-char (point-min))
                   (insert "{-# LANGUAGE "
                           (plist-get suggestion :extension)
                           " #-}\n")))
                (add-ghc-option
                 (save-excursion
                   (goto-char (point-min))
                   (insert "{-# OPTIONS_GHC "
                           (plist-get suggestion :option)
                           " #-}\n"))))))))))

(defun dante--warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Equivalent to 'warn', but label the warning as coming from dante."
  (display-warning 'dante (apply 'format message args) :warning))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref support

(defun dante--xref-backend () (when dante-mode 'dante))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dante)))
  (dante-ident-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (dante-async-call 'backend (concat ":l " (dante-temp-file-name)))
  (let ((result (apply #'dante-get-loc-at (dante-thing-at-point))))
    (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                        result)
      (let ((file (match-string 1 result))
            (line (string-to-number (match-string 2 result)))
            (col (string-to-number (match-string 3 result))))
            (list (xref-make "def" (xref-make-file-location
                                    (if (string= file (dante-temp-file-name)) (buffer-file-name) file)
                                    line
                                    (1- col))))))))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dante)

;;; dante.el ends here
