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
;; Package-Requires: ((dash "2.12.0") (emacs "27.1") (f "0.19.0") (flycheck "0.30") (company "0.9") (flymake "1.0") (s "1.11.0") (lcr "1.5"))
;; Version: 0-pre

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

;; DANTE: Do Aim Not To Expand.

;; This is a mode to provide Emacs interface for GHCi.  The mode
;; depends on GHCi only, keeping the logic simple.  Additionally it
;; aims to be minimal as far as possible.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'flycheck)
(require 'flymake)
(require 's)
(require 'xref)
(require 'lcr)
(eval-when-compile (require 'company))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup dante nil
  "Interactive development mode for Haskell."
  :group 'haskell)

(defcustom dante-debug nil
  "Show debug output."
  :group 'dante :safe t
  :type '(set (const inputs) (const outputs) (const responses) (const command-line)))

(defcustom dante-repl-command-line nil
  "Command line to start GHCi, as a list: the executable and its arguments.
When nil, dante will guess the value depending on
the variable `dante-project-root'  This should usually be customized
as a file or directory variable.  Each element of the list is a
sexp which is evaluated to a string before being passed to the
shell."
  :group 'dante
  :type '(repeat sexp))

(defcustom dante-project-root nil
  "The project root, as a string or nil.
When nil, dante will guess the value by looking for a cabal file.
Customize as a file or directory variable."
  :group 'dante :safe #'stringp
  :type '(choice (const nil) string))

(defcustom dante-target nil
  "The target to demand from cabal repl, as a string or nil.
Customize as a file or directory variable.  Different targets
will be in loaded in different GHCi sessions."
  :group 'dante :safe #'stringp
  :type '(choice (const nil) string))

(defun dante-cabal-new-nix (d)
  "Non-nil iff directory D hosts a nix file and a cabal file."
  (and (directory-files d t "shell.nix\\|default.nix")
       (directory-files d t "cabal.project.local")))

(defun dante-cabal-nix (d)
  "Non-nil iff directory D hosts a nix file and a cabal file."
  (and (directory-files d t "shell.nix\\|default.nix")
       (directory-files d t ".cabal$")))

(defcustom dante-methods-alist
  `((new-flake-impure "flake.nix" ("nix" "develop" "--impure" "-c" "cabal" "v2-repl" dante-target "--builddir=dist/dante"))
    (new-flake "flake.nix" ("nix" "develop" "-c" "cabal" "v2-repl" dante-target "--builddir=dist/dante"))
    (flake-impure "flake.nix" ("nix" "develop" "--impure" "-c" "cabal" "v1-repl" dante-target "--builddir=dist/dante"))
    (flake "flake.nix" ("nix" "develop" "-c" "cabal" "v1-repl" dante-target "--builddir=dist/dante"))
    (styx "styx.yaml" ("styx" "repl" dante-target))
    ; (snack ,(lambda (d) (directory-files d t "package\\.\\(yaml\\|nix\\)")) ("snack" "ghci" dante-target)) ; too easy to trigger, confuses too many people.
    (new-impure-nix dante-cabal-new-nix ("nix-shell" "--run" (concat "cabal v2-repl " dante-target " --builddir=dist/dante")))
    (new-nix dante-cabal-new-nix ("nix-shell" "--pure" "--run" (concat "cabal v2-repl " dante-target " --builddir=dist/dante")))
    (nix dante-cabal-nix ("nix-shell" "--pure" "--run" (concat "cabal v1-repl " dante-target " --builddir=dist/dante")))
    (impure-nix dante-cabal-nix ("nix-shell" "--run" (concat "cabal v1-repl " dante-target " --builddir=dist/dante")))
    (new-build "cabal.project.local" ("cabal" "new-repl" dante-target "--builddir=dist/dante"))
    (nix-ghci ,(lambda (d) (directory-files d t "shell.nix\\|default.nix")) ("nix-shell" "--pure" "--run" "ghci"))
    (stack "stack.yaml" ("stack" "repl" dante-target))
    (mafia "mafia" ("mafia" "repl" dante-target))
    (bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "v2-repl" dante-target "--builddir=newdist/dante"))
    (bare-v1-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "v1-repl" dante-target "--builddir=dist/dante"))
    (bare-ghci ,(lambda (_) t) ("ghci")))
"How to automatically locate project roots and launch GHCi.
This is an alist from method name to a pair of
a `locate-dominating-file' argument and a command line."
  :type '(alist :key-type symbol :value-type (list (choice (string :tag "File to locate") (function :tag "Predicate to use")) (repeat sexp))))

(defcustom dante-methods (-map 'car dante-methods-alist)
  "Keys in `dante-methods-alist' to try, in order.
Consider setting this variable as a directory variable."
   :group 'dante :safe t :type '(repeat symbol))

(put 'dante-methods 'safe-local-variable #'listp)

(defun dante-initialize-method ()
  "Initialize the method used to run GHCi.
Set `dante-project-root', `dante-repl-command-line' and
`dante-target'.  Do so according to `dante-methods' and previous
values of the above variables."
  (unless dante-target ; Get the current package name from a nearby .cabal file
    (setq-local dante-target
          (let ((cabal-file (dante-cabal-find-file)))
            (if cabal-file
                (replace-regexp-in-string
                 ".cabal$" ""
                 (file-name-nondirectory cabal-file))
              ""))))
  (or (--first (let ((root (locate-dominating-file default-directory (nth 0 it))))
                 (when root
                   (setq-local dante-project-root (or dante-project-root root))
                   (setq-local dante-repl-command-line (or dante-repl-command-line (nth 1 it)))))
               (-non-nil (--map (alist-get it dante-methods-alist)
                                dante-methods)))
      (error "No GHCi loading method applies.  Customize `dante-methods' or
      (`dante-repl-command-line' and `dante-project-root') and perhaps `dante-target'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session-local variables. These are set *IN THE GHCi INTERACTION BUFFER*

(defvar-local dante-ghci-path nil "Path where GHCi runs.
Variable `dante-project-root' can be different because of cabal behaviour.")
(defvar-local dante-flymake-token 1000)
(defvar-local dante-command-line nil "Command line used to start GHCi.")
(defvar-local dante-load-message nil "Load messages.")
(defvar-local dante-loaded-file "<DANTE:NO-FILE-LOADED>")
(defvar-local dante-queue nil "List of ready GHCi queries.")
(defvar-local dante-state nil
  "The current state.
- nil: initial state
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own.  Do not try restarting
automatically.  The user will have to manually run `dante-restart'
to destroy the buffer and create a fresh one without this variable enabled.
- other value: informative value for the user about what GHCi is doing.")

(defmacro dante-get-var (var)
  "Return the value of SYMBOL in the GHCi process buffer."
  `(when-let ((bp (dante-buffer-p))) (buffer-local-value ',var bp)))

(add-hook
 'lcr-context-switch-hook
 (defun dante-schedule-next ()
   "If no green thread is running, run the next queued one, if any."
   ;; when whatever green thread was running is over, we're back in
   ;; the original source buffer. It's time to check if anything
   ;; queued should be run.
   (if-let ((buffer (dante-buffer-p)))
       (with-current-buffer buffer
         (unless lcr-process-callback
           ;; Note that dante green threads are not interleaved,
           ;; because they only yield by placing a callback.
           (let ((req (pop dante-queue)))
             (when req (funcall req buffer))))))
   ;; we're about to yield back to emacs main loop. Inform the user of status.
   (force-mode-line-update t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar dante-mode-map (make-sparse-keymap) "Dante minor mode's map.")

(defun dante-status ()
  "Return dante's status for the current source buffer."
  (let ((buf (dante-buffer-p))
        (fname (buffer-file-name (current-buffer))))
    (if (not buf) "stopped"
      (with-current-buffer buf
        (concat
         (if lcr-process-callback "busy " "")
         (pcase dante-state
           (`(ghc-err (compiling ,mod)) (format "error(%s)" mod))
           (`(loaded ,_loaded-mods) (if (s-equals? dante-loaded-file fname) "loaded" (format "loaded(%s)" (file-name-base dante-loaded-file))))
           ;; (`(,hd . ,_tl) (format "%s" hd))
           (_ (format "%s" dante-state)))
         (if dante-queue (format "+%s" (length dante-queue)) ""))))))

(defcustom dante-modeline-prefix " Danté:" "Modeline prefix." :group 'dante :type 'string)

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
  :lighter (:eval (concat dante-modeline-prefix (dante-status)))
  :keymap dante-mode-map
  :group 'dante
  (if dante-mode
      (progn (add-hook 'flymake-diagnostic-functions 'dante-flymake nil t)
             (add-hook 'eldoc-documentation-functions 'dante-eldoc-type nil t))
    (remove-hook 'flymake-diagnostic-functions 'dante-flymake t)
    (remove-hook 'eldoc-documentation-functions 'dante-eldoc-type t)))

(define-key dante-mode-map (kbd "C-c .") 'dante-type-at)
(define-key dante-mode-map (kbd "C-c ,") 'dante-info)
(define-key dante-mode-map (kbd "C-c /") 'attrap-attrap) ;; deprecated keybinding
(define-key dante-mode-map (kbd "C-c \"") 'dante-eval-block)
(define-key dante-mode-map (kbd "C-c C-c") 'dante-exec)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive utils

(defun dante-fontify-expression (expression)
  "Return a haskell-fontified version of EXPRESSION.
If `haskell-mode' is not loaded, just return EXPRESSION."
  (if (fboundp 'haskell-mode)
      ;; From https://github.com/lunaryorn/ansible-doc.el/blob/master/ansible-doc.el#L211
      ;; See also http://emacs.stackexchange.com/a/5408/227
      (with-temp-buffer
        (insert expression)
        (delay-mode-hooks
          (haskell-mode)
          (font-lock-mode))
        (font-lock-ensure)
        (buffer-string))
    expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type, info and doc at point

(defun dante-doc (ident)
  "Get the haddock about IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (lcr-spawn
    (let ((info (lcr-call dante-async-call (format ":doc %s" ident))))
      (with-help-window (help-buffer)
        (princ info)))))

(defun dante-type-at (insert)
  "Get the type of the thing or selection at point.
When the universal argument INSERT is non-nil, insert the type in the buffer."
  (interactive "P")
  (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
    (lcr-spawn
      (lcr-call dante-async-load-current-buffer nil nil)
      (let ((ty (lcr-call dante-async-call (concat ":type-at " tap))))
        (if insert (save-excursion (goto-char (line-beginning-position))
                                   (insert (dante-fontify-expression ty) "\n"))
          (message "%s" (dante-fontify-expression ty)))))))

(defun dante-info (ident)
  "Get the info about the IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (lcr-spawn
    (lcr-call dante-async-load-current-buffer t nil)
    (let ((help-xref-following nil)
          (origin (buffer-name))
          (info (lcr-call dante-async-call (format ":info %s" ident))))
      (help-setup-xref (list #'dante-call-in-buffer (current-buffer) #'dante-info ident)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ (concat (dante-fontify-expression ident)
                       " in `" origin "'"
                       "\n\n"
                       (dante-fontify-expression info)))))))

(defvar-local dante-temp-fingerprint -1
  "The value of `sha1' of source buffer’s contents when the contents were last loaded.")

(defvar-local dante-interpreted nil)

(defvar dante-original-buffer-map (make-hash-table :test 'equal)
  "Hash table from (local) temp file names to the file they originate.")

(lcr-def dante-async-load-current-buffer (interpret err-fn)
  "Load and maybe INTERPRET the temp file for current buffer.
Interpreting puts all symbols from the current module in
scope.  Compiling to avoids re-interpreting the dependencies over
and over."
  (let* ((fingerprint (sha1 (current-buffer)))
         (unchanged (equal fingerprint dante-temp-fingerprint))
         (src-fname (buffer-file-name (current-buffer)))
         (fname (dante-temp-file-name (current-buffer)))
         (buffer (lcr-call dante-session))
         (same-target (and (or dante-interpreted (not interpret))
                       (s-equals? (buffer-local-value 'dante-loaded-file buffer) src-fname))))
    (if (and unchanged same-target) ; see #52
        (buffer-local-value 'dante-load-message buffer)
      (setq dante-temp-fingerprint fingerprint)
      (setq dante-interpreted interpret)
      (puthash (dante-local-name fname) src-fname dante-original-buffer-map)
      (write-region nil nil fname nil 0)
      ;; GHCi will interpret the buffer if both -fbyte-code and :l * are used.
      (lcr-call dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code"))
      (with-current-buffer buffer
        (setq-local dante-status 'loading)
        (dante-async-write (if same-target ":r!"
                             (concat ":l! " (if interpret "*" "") (dante-local-name fname))))
        (cl-destructuring-bind (_status err-messages _loaded-modules)
            (lcr-call dante-load-loop "" nil err-fn)
          (setq dante-loaded-file src-fname)
          (setq dante-load-message err-messages))))))

(defun dante-local-name (fname)
  "Local name of FNAME on the remote host."
  (string-remove-prefix (or (file-remote-p fname) "") fname))

;;;;;;;;;;;;;;;;;;;;;
;; Flycheck checker

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-get-var dante-state) 'dead) (funcall cont 'interrupted)
    (lcr-spawn
      (let* ((messages (lcr-call dante-async-load-current-buffer nil nil))
             (temp-file (dante-local-name (dante-temp-file-name (current-buffer)))))
        (funcall cont
                 'finished
                 (-non-nil (--map (dante-fly-message it checker (current-buffer) temp-file) messages)))))))

(flycheck-define-generic-checker 'haskell-dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :predicate (lambda () dante-mode)
  :modes '(haskell-mode haskell-literate-mode))

(add-to-list 'flycheck-checkers 'haskell-dante)

(defcustom dante-flycheck-types
  '(("^warning: \\[-W\\(typed-holes\\|deferred-\\(type-errors\\|out-of-scope-variables\\)\\)\\]" . error)
    ("^warning" . warning)
    ("^splicing " . nil)
    ("" . error))
  "Map of regular expressions to flycheck error types, ordered by priority."
  :group 'dante :type '(repeat cons (regex symbol)))

(defun dante-fly-message (matched checker buffer temp-file)
  "Convert the MATCHED message to flycheck format.
CHECKER and BUFFER are added if the error is in TEMP-FILE."
  (cl-destructuring-bind (file location-raw err-type msg) matched
    (let* ((type (cdr (--first (s-matches? (car it) err-type) dante-flycheck-types)))
           (location (dante-parse-error-location location-raw)))
      ;; FIXME: sometimes the "error type" contains the actual error too.
      (when type
        (flycheck-error-new-at (car location) (cadr location) type
                               (replace-regexp-in-string (regexp-quote temp-file)
                                                         (dante-buffer-file-name buffer)
                                                         (concat err-type "\n" (s-trim-right msg)))
                               :checker checker
                               :buffer buffer
                               :filename (if (string= (dante-canonicalize-path temp-file)
                                                      (dante-canonicalize-path file))
                                             (dante-buffer-file-name buffer)
                                           file))))))

(defun dante-parse-error-location (string)
  "Parse the line/col numbers from the error in STRING."
  (--map (when it (string-to-number it))
         (cdr (s-match (concat
                        "\\(?1:[0-9]+\\):\\(?2:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
                        "\\|"
                        "(\\(?1:[0-9]+\\),\\(?2:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?4:[0-9]+\\))") ;; "(289,5)-(291,36)"
                       string))))

(defun dante-call-in-buffer (buffer func &rest args)
  "In BUFFER, call FUNC with ARGS."
  (with-current-buffer buffer (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(lcr-def dante-complete (prefix)
  (lcr-call dante-async-load-current-buffer nil nil)
  (let* ((reply (lcr-call dante-async-call (format ":complete repl %S" prefix)))
         (lines (s-lines reply))
         (common (nth 2 (read (concat "(" (car lines) ")")))))
    (--map (concat common (read it)) (cdr lines))))

(defun dante--in-a-comment ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(declare-function company-begin-backend 'company)

(defun dante-company (command &optional arg &rest _ignored)
  "Company backend for dante.
See ``company-backends'' for the meaning of COMMAND, ARG and _IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'dante-company))
    (sorted t)
    (prefix (when (and dante-mode (not (dante--in-a-comment)) (dante-ident-pos-at-point -1))
              (let* ((id-start (car (dante-ident-pos-at-point -1)))
                     (_ (save-excursion (re-search-backward "import[\t ]*" (line-beginning-position) t)))
                     (import-end (match-end 0))
                     (import-start (match-beginning 0))
                     (is-import (eq import-end id-start)))
                (buffer-substring-no-properties (if is-import import-start id-start) (point)))))
    (candidates
     (unless (eq (dante-get-var dante-state) 'dead)
       (cons :async (lambda (callback) (lcr-spawn (lcr-halt callback (lcr-call dante-complete arg)))))))))

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
  (when-let ((reg (dante-ident-pos-at-point)))
    (apply #'buffer-substring-no-properties reg)))

(defun dante-ident-pos-at-point (&optional offset)
  "Return the span of the (qualified) identifier at point+OFFSET.
Nil if none found."
  (let* ((qualifier-regex "\\([[:upper:]][[:alnum:]]*\\.\\)")
         (ident-regex (concat qualifier-regex "*\\(\\s.+\\|\\(\\sw\\|\\s_\\)+\\)"))) ; note * for many qualifiers
    (save-excursion
      (goto-char (+ (point) (or offset 0)))
      (when (looking-at ident-regex)
        (let ((end (match-end 0)))
          (skip-syntax-backward (if (looking-at "\\s.") "." "w_")) ;; find start of operator/variable
          (while (save-excursion
                   (and (re-search-backward (concat "\\b" qualifier-regex) (line-beginning-position) t)
                        (s-matches? (concat "^" ident-regex "$") (buffer-substring-no-properties (point) end))))
            (goto-char (match-beginning 0)))
          (list (point) end))))))

(defun dante-buffer-file-name (&optional buffer)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (let ((name (buffer-file-name buffer)))
    (when name
      (dante-canonicalize-path (substring-no-properties name)))))

(defvar-local dante-temp-file-name nil
  "The name of a temporary file to which the current buffer's content is copied.")

(require 'tramp)
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
              (setq result (concat (make-temp-name prefix) suffix))
              (if (file-exists-p result)
                  (setq result nil)))
                ;; This creates the file by side effect.
            (set-file-times result)
            (set-file-modes result #o700)
            result))
      (make-temp-file "dante" nil suffix))))

(defun dante-temp-file-name (buffer)
  "Return a (possibly remote) filename suitable to store BUFFER's contents."
  (with-current-buffer buffer
    (or dante-temp-file-name (setq dante-temp-file-name (dante-tramp-make-tramp-temp-file buffer)))))

(defun dante-canonicalize-path (path)
  "Return a standardized version of PATH.
On Windows, forward slashes are changed to backslashes and the
drive letter is capitalized."
  (let ((standard-path (convert-standard-filename path)))
    (if (eq system-type 'windows-nt)
        (dante-capitalize-drive-letter (s-replace "/" "\\" standard-path))
      standard-path)))

(defun dante-capitalize-drive-letter (path)
  "Ensures the drive letter is capitalized in PATH.
This applies to paths of the form x:\\foo\\bar"
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
  (pcase reg (`(,beg ,end)
              (format "%S %d %d %d %d %s"
                      (dante-local-name (dante-temp-file-name (current-buffer)))
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
  (when-let ((buf (dante-buffer-p)))
    (dante-set-state 'deleting)
    (when-let ((process (get-buffer-process buf)))
      (kill-process process)
      (delete-process process))
    (kill-buffer buf)))

(defun dante-restart ()
  "Restart GHCi with the same configuration (root, command line, …) as before."
  (interactive)
  (when (dante-buffer-p)
    (dante-destroy))
  (lcr-spawn (lcr-call dante-start))
  (when flymake-mode (flymake-start t t))) ; re-enable backend

(defun dante-session (cont)
  "Get the session or create one if none exists.
If WAIT is nil, abort if Dante is busy.  Pass the dante buffer to CONT"
  (if-let* ((buf (dante-buffer-p)))
      (if (buffer-local-value 'lcr-process-callback buf)
          (lcr-context-switch
              (with-current-buffer buf
                (when dante-queue
                  (message "Overriding previously queued GHCi request."))
                (setq dante-queue (cons (lambda (x) (lcr-resume cont x)) nil))))
        (funcall cont buf))
    (dante-start cont)))

(defcustom dante-load-flags '("+c" "-fdiagnostics-color=never" "-fno-diagnostics-show-caret" "-Wwarn=missing-home-modules" "-ferror-spans" )
  "Flags to set whenever GHCi is started."
  :type (cons 'set (--map (list 'const :tag (concat (car it) ": " (cadr it)) (car it))
                          '(("+c" "Gather type information (necessary for `dante-type-at')")
                            ("-Wall" "Report all warnings")
                            ("-ferror-spans" "Report span in error messages (used in flymake only)")
                            ("-fdefer-typed-holes" "Accept typed holes, so that completion/type-at continues to work then.")
                            ("-Wwarn=missing-home-modules" "Do not error-out if a module is missing in .cabal file")
                            ("-fdiagnostics-color=never" "No color codes in error messages (color codes will trigger bugs in Dante)")
                            ("-fno-diagnostics-show-caret" "Cleaner error messages for GHC >=8.2 (ignored by earlier versions)")))))

(lcr-def dante-start ()
  "Start a GHCi process and return its buffer."
  (let* ((buffer (dante-buffer-create))
         (args (-non-nil (-map #'eval dante-repl-command-line)))
         (process (with-current-buffer buffer
                    (message "Dante: Starting GHCi: %s" (combine-and-quote-strings args))
                    (apply #'start-file-process "dante" buffer args))))
    (set-process-query-on-exit-flag process nil)
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local dante-command-line (process-command process)))
    (dante-set-state 'starting)
    (lcr-process-initialize buffer)
    (set-process-sentinel process 'dante-sentinel) ; only now can we interact with GHCi
    (lcr-call dante-async-call (s-join "\n" (--map (concat ":set " it) (-snoc dante-load-flags "prompt \"\\4%s|\""))))
    (let ((dir (lcr-call dante-async-call ":!pwd")))
      (with-current-buffer buffer (setq dante-ghci-path dir)))
    (dante-set-state 'started)
    buffer))

(defun dante-debug (category msg &rest objects)
  "Append a debug message MSG to the current buffer.
Do so iff CATEGORY is enabled in variable `dante-debug'."
  (when (memq category dante-debug)
    (goto-char (1- (point-max)))
    (insert (apply #'format msg objects))))

(lcr-def dante-async-read ()
  "Read input from GHCi.
Must be called from GHCi process buffer."
  (let* ((input (lcr-call lcr-process-read (current-buffer))))
    (dante-debug 'inputs "%s" input)
    (s-replace "\r" "" input)))

(defconst dante-ghci-prompt "\4\\(.*\\)|")

(defun dante-regexp-disjoin (&rest regexps)
  "Return a regexp matching any of REGEXPS."
  (s-join "\\|" regexps))

(lcr-def dante-load-loop (acc err-msgs err-fn)
  "Parse the output of load command.
ACC umulate input and ERR-MSGS."
  (let ((success (dante-regexp-disjoin
                  "^Ok, modules loaded:[ ]*\\([^\n ]*\\)\\( (.*)\\)?\."
                  "^Ok, .*modules loaded." ;; .* stands for a number in english (two, three, ...) (GHC 8.2)
                  "^Ok, one module loaded."))
        (progress "^\\[\\([0-9]*\\) of \\([0-9]*\\)\\] Compiling \\([^ \n]*\\).*")
        (err-regexp "^\\([A-Z]?:?[^ \n:][^:\n\r]+\\):\\([0-9()-:]+\\): \\(.*\\)\n\\(\\([ ]+.*\n\\)*\\)")
        result cur-file)
    (while (not result)
      (let* ((i (string-match (dante-regexp-disjoin dante-ghci-prompt success err-regexp progress) acc))
             (m (when i (match-string 0 acc)))
             (rest (when i (substring acc (match-end 0)))))
        (cond ((and m (string-match dante-ghci-prompt m))
               (setq dante-state (list 'ghc-err (pcase dante-state (`(compiling ,module) module)
                                                       (_ cur-file)))) ; when the module name is wrong, ghc does not output any "Compiling ..." message
               (setq result (list 'failed (nreverse err-msgs) (match-string 1 m))))
              ((and m (string-match progress m))
               (setq dante-state (list 'compiling (match-string 3 m))))
              ((and m (string-match success m))
               ;; With the +c setting, GHC (8.2) prints: 1. error
               ;; messages+warnings, if compiling only 2. if successful,
               ;; repeat the warnings
               (setq dante-state 'process-warnings)
               (cl-destructuring-bind (_status warning-msgs loaded-mods) (lcr-call dante-load-loop rest nil nil)
                 (setq dante-state (list 'loaded loaded-mods))
                 (setq result (list 'ok (or (nreverse err-msgs) warning-msgs) loaded-mods))))
              ((and m (> (length rest) 0) (/= (elt rest 0) ? )) ;; make sure we're matching a full error message
               (let ((err-msg (-take 4 (cdr (s-match err-regexp m)))))
                 (setq cur-file (car err-msg))
                 (push err-msg err-msgs)
                 (when err-fn (funcall err-fn (list err-msg)))))
              (t (setq rest (concat acc (lcr-call dante-async-read)))))
        (setq acc rest)))
    result))

(defun dante-async-write (cmd)
  "Write to GHCi associated with current buffer the CMD."
  (dante-debug 'outputs "\n[Dante] -> %s\n" cmd)
  (process-send-string (get-buffer-process (current-buffer)) (concat cmd "\n")))

(lcr-def dante-async-call (cmd)
  "Send GHCi the command string CMD and return the answer."
  (with-current-buffer (dante-buffer-p)
    (dante-async-write cmd)
    (let ((acc "")
          (matched nil))
      (while (not matched)
        (setq acc (concat acc (lcr-call dante-async-read)))
        (setq matched (string-match dante-ghci-prompt acc)))
      (s-trim-right (substring acc 0 (1- (match-beginning 1)))))))

(defun dante-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (let ((buffer (process-buffer process)))
    (when (and (buffer-live-p buffer) (not (process-live-p process)))
      (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
          (message "GHCi process deleted.")
        (with-current-buffer buffer (setq dante-state 'dead))
        (dante-show-process-problem buffer change)))))

(defun dante-diagnose ()
  "Show all state info in a help buffer."
  (interactive)
  (let ((info (dante-debug-info (dante-buffer-p))))
    (with-help-window (help-buffer)
      (princ info))))

(defun dante-debug-info (buffer)
  "Show debug info for dante buffer BUFFER."
  (if buffer
      (with-current-buffer buffer
        (s-join "\n" (--map (format "%s %S" it (eval it))
                            '(default-directory dante-ghci-path dante-command-line dante-state dante-queue dante-loaded-file dante-load-message lcr-process-callback))))
    "No GHCi interaction buffer"))

(defun dante-show-process-problem (buf change)
  "Report to the user that GHCi in BUF reported CHANGE, causing it to end."
  (message "Problem with GHCi process!")
  (display-buffer buf 'display-buffer-pop-up-window)
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n---\n\n")
    (insert
     (propertize
      (concat "This is the buffer associated with the GHCi session. This buffer
is normally hidden, but the GHCi process ended.

WHAT TO DO NEXT: Verify that the GHCi REPL can be loaded
manually, then try to customize (probably file-locally or
directory-locally) a combination of the following variables:
`dante-method', `dante-project-root', `dante-repl-command-line',
`dante-target'.  If you fixed the problem, just kill this buffer,
Dante will make a fresh one and attempt to restart GHCi
automatically.  If you leave this buffer around Dante will not
attempt to restart GHCi.  You can always run `dante-restart' to
make it try again.

EXTRA TROUBLESHOOTING INFO

Process state change: " change "
" (dante-debug-info (current-buffer))))
     'face 'compilation-error)))

(defun dante-buffer-create ()
  "Create the buffer for GHCi."
  (let* ((dir dante-project-root))
    (with-current-buffer (get-buffer-create (dante-buffer-name))
      (cd dir)
      (fundamental-mode) ;; this has several effects, including resetting the local variables
      (buffer-disable-undo)
      (current-buffer))))

(defun dante-set-state (state)
  "Set the `dante-state' to STATE."
  (with-current-buffer (dante-buffer-p) (setq-local dante-state state)))

(defun dante-buffer-name ()
  "Name of the GHCi interaction buffer.
First, if needed, initialize the GHCi invokation method.  Then construct the
appropriate buffer name on this basis."
  (unless dante-project-root (dante-initialize-method))
  (concat "*dante:" dante-target ":" dante-project-root "*"))

(defun dante-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (dante-buffer-name)))

(defun dante-cabal-find-file (&optional file)
  "Search for directory of cabal file.
Search upwards in the directory structure, starting from FILE (or
`default-directory' if nil)."
  (let ((dir (locate-dominating-file (or file default-directory)
                                     (lambda (d) (directory-files d t ".\\.cabal\\'")))))
    (when dir (car (directory-files dir t ".\\.cabal\\'")))))


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
       (or (gethash file dante-original-buffer-map)
           (expand-file-name file (dante-get-var dante-ghci-path)))
       line (1- col)))))

(defun dante--summarize-src-spans (spans file)
  "Add summary strings to a list of source SPANS in FILE."
  (if (not (and file (file-readable-p file)))
      (--map (xref-make "<unreadable>" it) spans)
    (let* ((lines (s-lines (f-read file)))
           (wanted (--map (1- (xref-file-location-line it)) spans))
           (lines (-select-by-indices wanted lines)))
      (-zip-with #'xref-make lines spans))))

(defun dante--make-xrefs (string)
  "Make xref objects for the source spans in STRING."
  (--mapcat (funcall #'dante--summarize-src-spans (cdr it) (car it))
            (--group-by (xref-file-location-file it)
                        (-non-nil (-map #'dante--match-src-span
                                        (s-lines string))))))

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (lcr-spawn-and-wait
   (lcr-call dante-async-load-current-buffer nil nil)
   (dante--make-xrefs (lcr-call dante-async-call (concat ":loc-at " symbol)))))

(cl-defmethod xref-backend-references ((_backend (eql dante)) symbol)
  (lcr-spawn-and-wait
   (lcr-call dante-async-load-current-buffer nil nil)
   (dante--make-xrefs (lcr-call dante-async-call (concat ":uses " symbol)))))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc support

(defun dante-eldoc-type (callback &rest _ignored)
  "Document type of function at point via CALLBACK.
Intended for `eldoc-documentation-functions'"
  (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
    (unless (or (eq (dante-get-var dante-state) 'dead) ;; GHCi dead?
                (dante-get-var lcr-process-callback) ;; GHCi busy?
                (dante--in-a-comment)
                (nth 3 (syntax-ppss)) ;; in a string
                (s-blank? tap))
      (lcr-spawn
        (lcr-call dante-async-load-current-buffer t nil)
        (let ((ty (lcr-call dante-async-call (concat ":type-at " tap))))
          (unless (s-match "^<interactive>" ty)
            (funcall callback (s-collapse-whitespace (dante-fontify-expression ty))))))
      ;; TODO: improve by reporting :thing separately
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reploid

(defun dante-eval-block ()
  "Evaluate the GHCi command(s) found at point and insert the results.
The command block is indicated by the >>> symbol."
  (interactive)
  (push-mark)
  (beginning-of-line)
  (let ((block-end (save-excursion
                     (while (looking-at "[ \t]*--") (forward-line))
                     ;; ensure that there is a newline at the end of buffer
                     (when (eq (point) (point-max)) (newline))
                     (point-marker))))
    (while (looking-at "[ \t]*--") (forward-line -1))
    (forward-line)
    (lcr-spawn
      (lcr-call dante-async-load-current-buffer t nil)
      (while (search-forward-regexp "[ \t]*--[ \t]+>>>" (line-end-position) t 1)
        ;; found a command; execute it and replace the result.
        (let* ((cmd (buffer-substring-no-properties (point) (line-end-position))) 
               (res (lcr-call dante-async-call cmd)))
          (beginning-of-line)
          (forward-line)
          (save-excursion
            (delete-region (point)
                           ;; look for: empty comment line, next command or end of block.
                           (or (and (search-forward-regexp "[ \t]*--[ \t]*\\([ \t]>>>\\|$\\)" block-end t 1)
                                    (match-beginning 0))
                               block-end)))
          (insert (apply #'concat (--map (concat "-- " it "\n") (--remove (s-blank? it) (s-lines res)))))
          (beginning-of-line)
          ;; skip any non-executable comment
          (while (and (looking-at "[ \t]*--")
                      (not (looking-at "[ \t]*--[ \t]+>>>")))
            (forward-line)))))))

(defcustom dante-exec-default "main"
  (substitute-command-keys "Default command to run by `dante-exec'.")
  :group 'dante :safe t :type 'string)

(defun dante-exec (command)
  "Execute COMMAND in GHCi and show the result in the echo area."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       dante-exec-default)))
  (dante-set-state 'running)
  (lcr-spawn
    (lcr-call dante-async-load-current-buffer nil nil)
    (message "%s" (lcr-call dante-async-call command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake

(defun dante-flymake (report-fn &rest _args)
  "Run a check and pass the status onto REPORT-FN."
  (let* ((src-buffer (current-buffer))
         (buf0 (dante-buffer-p))
         (temp-file (dante-local-name (dante-temp-file-name src-buffer)))
         (nothing-done t)
         ;; flymake raises errors when any report is made using an
         ;; "old" call to the backend. However, we must deal with all
         ;; GHCi output, so we must let the loop run to completion. So
         ;; we simply disable messages if another call to this
         ;; function is detected.  This token must be set before any
         ;; context switch occurs, otherwise we cannot detect if we
         ;; got another call from flymake in between.
         (local-token (if buf0 (with-current-buffer buf0 (setq dante-flymake-token (1+ dante-flymake-token)))
                        dante-flymake-token)))
    (if (eq (dante-get-var dante-state) 'dead) (funcall report-fn :panic :explanation "Ghci is dead")
      (lcr-spawn
        (let* ((buf (lcr-call dante-session)) ; yield until GHCi is ready to process the request
               (token-guard (lambda () (eq (buffer-local-value 'dante-flymake-token buf) local-token)))
               (msg-fn (lambda (messages)
                         (when (funcall token-guard)
                           (setq nothing-done nil)
                           (funcall report-fn
                                    (-non-nil
                                     (--map (dante-fm-message it src-buffer temp-file) messages)))))))
          (when (funcall token-guard) ; don't try to load if we're too late.
            (let ((messages (lcr-call dante-async-load-current-buffer nil msg-fn)))
              (when nothing-done ; clears previous messages and deals with #52
                ;; this can happen when the file did not change
                (funcall msg-fn messages)))))))))

(defun dante-pos-at-line-col (buf l c)
  "Translate line L and column C into a position within BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- l))
      (move-to-column (1- c))
      (point))))

(defun dante-fm-message (matched buffer temp-file)
  "Convert the MATCHED message to flymake format.
Or nil if BUFFER / TEMP-FILE are not relevant to the message."
  (cl-destructuring-bind (file location-raw first-line msg) matched
    ;; Flymake bug: in fact, we would want to report all errors,
    ;; with buffer = (if (string= temp-file file) buffer (find-buffer-visiting file)),
    ;; but flymake actually ignores the buffer argument of flymake-make-diagnostic (?!).
    (when (string= temp-file file)
      (let* ((type-analysis
              (cl-destructuring-bind (typ msg-start) (s-split-up-to ":" first-line 1)
                (cond ((string-equal typ "warning")
                       (if (s-matches? "\\[-W\\(typed-holes\\|deferred-\\(type-errors\\|out-of-scope-variables\\)\\)\\]" msg-start)
                           (list :error "")
                         (list :warning "")))
                      ((string-equal typ "splicing") (list :info ""))
                      (t (list :error msg-start)))))
             (location (dante-parse-error-location location-raw))
             (r (pcase location
                  (`(,l1 ,c1 ,l2 ,c2) (cons (dante-pos-at-line-col buffer l1 c1) (dante-pos-at-line-col buffer (or l2 l1) (1+ c2))))
                  (`(,l ,c) (flymake-diag-region buffer l c)))))
        (when r
          (cl-destructuring-bind (type msg-first-line) type-analysis
            (let* ((final-msg (s-trim (concat msg-first-line "\n" (replace-regexp-in-string "^    " "" msg)))))
              (flymake-make-diagnostic buffer (car r) (cdr r) type final-msg))))))))

(provide 'dante)

;;; dante.el ends here
