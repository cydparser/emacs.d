;;; -*- lexical-binding: t -*-

(require 'init-utils)

;;; Commands

(defun init-delete-excess-whitespace (&optional n)
  "Deletes excess spaces and newlines.
When on an non-empty line, delete one or N spaces (see
`just-one-space')."
  (interactive "*p")
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*$"))
      (delete-blank-lines)
    (just-one-space n)))

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

(defun init-sexp-downcase (&optional arg)
  "Convert the sexp to lower case.
ARG determines the direction and number of sexps."
  (interactive "*P")
  (mark-sexp arg)
  (downcase-region (region-beginning) (region-end)))

(defun init-sexp-upcase (&optional arg)
  "Convert the sexp to upper case.
ARG determines the direction and number of sexps."
  (interactive "*P")
  (mark-sexp arg)
  (upcase-region (region-beginning) (region-end)))

;;; Global Configuration

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(make-directory init-config-directory :parents)

;; Store auto-saves and backups in emacs.d/var.
(let ((adir (expand-file-name "autosaves/" init-var-directory))
      (ldir (expand-file-name "auto-save-list/" init-var-directory))
      (bdir (expand-file-name "backups/" init-var-directory)))
  (make-directory adir t)
  (make-directory bdir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat adir "\\1") t))
        auto-save-list-file-prefix (concat ldir "/saves-")
        backup-directory-alist `((".*" . ,bdir))))

;; Disable lock files.
(setq create-lockfiles nil)

;; Split windows horizontally.
(setq split-height-threshold nil)

;; Simplify prompts.
(setq use-short-answers t)

;; Reduce noise.
(setq auto-revert-mode-text ""
      confirm-kill-processes nil
      confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function #'ignore
      xref-prompt-for-identifier nil)

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
                standard-indent spaces
                tab-width spaces))

(defalias 'ar #'align-regexp)
(defalias 'rs #'replace-string)
(defalias 'sb #'speedbar)
(defalias 'sl #'sort-lines)

(define-key input-decode-map [(control ?I)] (kbd "<backtab>"))

(bind-key "C-S-C" #'kill-ring-save)
(bind-key "C-S-V" #'yank)
(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-h C-f" #'find-function-at-point)
(bind-key "C-h C-k" #'find-function-on-key)
(bind-key "C-h C-v" #'find-variable-at-point)
(bind-key "C-w" #'init-kill-region-or-backward-word)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'init-kill-buffer-current)
(bind-key "M-/" #'hippie-expand)
(bind-key "M-L" #'init-sexp-downcase)
(bind-key "M-R" #'repeat)
(bind-key "M-SPC" #'init-delete-excess-whitespace)
(bind-key "M-U" #'init-sexp-upcase)

(delete-selection-mode 1)

(global-subword-mode 1)
(diminish 'subword-mode "")

(when (string-equal (seq-take emacs-version 2) "25")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional _param)
       (list start end))))

;;; Packages

(use-package align
  :ensure nil
  :config
  (progn
    (dolist (mode '(haskell-mode ruby-mode))
      (add-to-list 'align-open-comment-modes mode))

    (dolist (tr '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                  (haskell-assignment  . "\\(\\s-+\\)\\(=\\|:=\\|.=\\)\\s-+")
                  (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))
      (add-to-list 'align-rules-list
                   `(,(car tr) (regexp . ,(cdr tr))
                     (modes . '(haskell-mode literate-haskell-mode)))))))

(use-package async
  :demand
  :hook (after-init-hook . async-bytecomp-package-mode)
  :init (setq async-bytecomp-allowed-packages '(all)))

(use-package autorevert
  :ensure nil
  :init (setq auto-revert-verbose nil
              auto-revert-tail-mode-text " ⭛"))

(use-package bookmark
  :ensure nil
  :init (setq bookmark-default-file (expand-file-name "bookmarks" init-var-directory)
              bookmark-save-flag 1))

(use-package calendar
  :ensure nil
  :custom (calendar-week-start-day 1))

(use-package diff-mode
  :ensure nil
  :config
  (progn
    (unbind-key "o" diff-mode-shared-map)))

(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode . ""))

(use-package electric
  :ensure nil
  :init (setq-default electric-indent-inhibit t))

(use-package etags
  :ensure nil
  :init (setq tags-add-tables nil
              tags-revert-without-query t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("DICPATH" "PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(use-package files
  :ensure nil
  :custom
  (save-some-buffers-default-predicate 'save-some-buffers-root))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package help
  :ensure nil
  :custom
  (describe-bindings-outline t))

(use-package help-fns
  :ensure nil
  :custom
  (help-enable-symbol-autoload t))

(use-package hippie-exp
  :ensure nil
  :init (setq hippie-expand-try-functions-list (seq-difference hippie-expand-try-functions-list
                                                               '(try-expand-line try-expand-list))))

(use-package minibuffer
  :ensure nil
  :custom
  (completions-detailed t)
  (read-minibuffer-restore-windows nil))

(use-package mwheel
  :ensure nil
  :init (setq mouse-wheel-flip-direction t
              mouse-wheel-tilt-scroll t))

(use-package newcomment
  :ensure nil
  :bind ("M-;" . comment-line))

(use-package simple
  :ensure nil
  :bind (("M-n" . next-error)
         ("M-p" . previous-error))
  :custom
  (next-error-message-highlight t))

(use-package xt-mouse
  :ensure nil
  :hook (after-init-hook . xterm-mouse-mode))
