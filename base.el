;;; -*- lexical-binding: t -*-

;;; Utilities

(defconst init-var-directory
  (expand-file-name "var" user-emacs-directory)
  "Directory for various files created by Emacs.")

(defconst init-xdg-config-home
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config"))
  "XDG config home directory.")

(defconst init-xdg-data-home
  (or (getenv "$XDG_DATA_HOME") (expand-file-name "~/.local/share"))
  "XDG data home directory.")

(defun init-kill-buffer-current ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun init-kill-region-or-backward-word (arg)
  "Kill selected region or backward word."
  (interactive "p")
  (if (region-active-p)
      (kill-region (mark) (point) 'region)
    (backward-kill-word arg)))

(defun init-special-buffer-p ()
  "Checks if current buffer is special."
  (string-prefix-p "*" (buffer-name)))

(defun init-xdg-config (path)
  "Convert relative PATH to absolute using XDG config home for the parent directory."
  (expand-file-name path init-xdg-config-home))

(defun init-xdg-data (path)
  "Convert relative PATH to absolute using XDG data home for the parent directory."
  (expand-file-name path init-xdg-data-home))

(defmacro init-when-file-exists (path sexp)
  "Evaluates (append SEXP (PATH)) if file exists."
  (declare (indent defun))
  `(let ((file ,path))
     (when (file-exists-p file)
       (,@sexp file))))

;;; Global Configuration

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add NixOS directories to load path.
(dolist (path '("~/.nix-profile/share/emacs/site-lisp"
                "/run/current-system/sw/share/emacs/site-lisp"))
  (init-when-file-exists path (add-to-list 'load-path)))

;; Store auto-saves and backups in emacs.d/var.
(let ((adir (expand-file-name "autosaves/" init-var-directory))
      (ldir (expand-file-name "auto-save-list/" init-var-directory))
      (bdir (expand-file-name "backups/" init-var-directory)))
  (make-directory adir t)
  (make-directory bdir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat adir "\\1") t))
        auto-save-list-file-prefix (concat ldir "/saves-")
        backup-directory-alist `((".*" . ,bdir))))

(let ((fonts (font-family-list)))
  (when (member "Symbola" fonts)
    ;; Use Symbola for mathematical operators.
    (set-fontset-font t '(#x2200 . #x22FF) "Symbola 14"))

  (cond ((member "Hasklig" fonts)
         (set-frame-font "Hasklig Light 14")

         (defconst init-hasklig-prettify-symbols-alist
           (let ((codepoint #Xe100))
             (mapcar (lambda (lig)
                       (let ((pair (cons lig (string ?\t codepoint))))
                         (setq codepoint (1+ codepoint))
                         pair))
                     '("&&"  "***" "*>"  "\\\\" "||"  "|>"  "::"
                       "=="  "===" "==>" "=>"   "=<<" "!!"  ">>"
                       ">>=" ">>>" ">>-" ">-"   "->"  "-<"  "-<<"
                       "<*"  "<*>" "<|"  "<|>"  "<$>" "<>"  "<-"
                       "<<"  "<<<" "<+>" ".."   "..." "++"  "+++"
                       "/="  ":::" ">=>" "->>"  "<=>" "<=<" "<->"))))

         (defconst init-hasklig-prettify-symbols-common-alist
           (let ((keep '("&&" "||" "==" "===" "=>" "->" "<<" ">>" ">>>"
                         ".." "..." "++" "<=>")))
             (append
              '(("!=" . "	")
                ("lambda" . "λ")
                ("not" . "¬"))
              (seq-filter (lambda (pair)
                            (member (car pair) keep))
                          init-hasklig-prettify-symbols-alist))))

         (setq prettify-symbols-alist init-hasklig-prettify-symbols-common-alist)

         (defun init-prettify-symbols ()
           (unless (derived-mode-p 'haskell-mode)
             (prettify-symbols-mode)))

         (add-hook 'prog-mode-hook #'init-prettify-symbols))
        ((member "Inconsolata" fonts)
         (set-frame-font "Inconsolata 15"))))

;; Simplify prompts.
(fset 'yes-or-no-p #'y-or-n-p)

;; Reduce noise.
(setq auto-revert-mode-text ""
      confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function #'ignore)

(diminish 'subword-mode "")

;; Prevent accidental closure.
(setq confirm-kill-emacs #'y-or-n-p)

;; Display column number in modeline.
(setq column-number-mode t)

;; Collect garbage less frequently.
(setq gc-cons-threshold 104857600)

;; Delete the trailing newline.
(setq kill-whole-line t)

;; Adjust indentation and line wrapping.
(let ((spaces 2)
      (max-line-length 80))
  (setq-default fill-column max-line-length
                indent-tabs-mode nil
                tab-width spaces
                tab-stop-list (number-sequence spaces max-line-length spaces)))

(defalias 'ar #'align-regexp)
(defalias 'rs #'replace-string)
(defalias 'sb #'speedbar)
(defalias 'sl #'sort-lines)

(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-h C-f" #'find-function-at-point)
(bind-key "C-h C-k" #'find-function-on-key)
(bind-key "C-h C-v" #'find-variable-at-point)
(bind-key "C-w" #'init-kill-region-or-backward-word)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'init-kill-buffer-current)
(bind-key "M-/" #'hippie-expand)
(bind-key "M-o" #'other-window)

(global-subword-mode 1)

;;; Packages

(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("DICPATH" "PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))
