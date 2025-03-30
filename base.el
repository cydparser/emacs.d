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

;; Disable implicit frame resizing.
(setq frame-inhibit-implied-resize t)

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load custom-file)

(set-default-coding-systems 'utf-8)

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
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function #'ignore
      xref-prompt-for-identifier nil)

;; Prevent accidental closure.
(setq confirm-kill-emacs #'y-or-n-p)

;; Display column number in modeline.
(setq column-number-mode t)

;; Collect garbage less frequently.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))))

;; Increase undo history.
(let ((mib (expt 2 20)))
  (setq undo-limit (* 1 mib)
        undo-strong-limit (* 16 mib)
        undo-outer-limit (* 128 mib)))

(setq read-process-output-max (* 1024 1024))

;; Adjust indentation and line wrapping.
(let ((spaces 2)
      (max-line-length 100))
  (setq-default fill-column max-line-length
                indent-tabs-mode nil
                sentence-end-double-space nil
                standard-indent spaces
                tab-width spaces))

(defalias 'ar #'align-regexp)
(defalias 'rs #'replace-string)
(defalias 'sb #'speedbar)
(defalias 'sl #'sort-lines)

(define-key input-decode-map [(control ?I)] (kbd "<backtab>"))
(define-key input-decode-map [(control ?\[)] (kbd "<C-[>"))

(bind-key "C-S-C" #'kill-ring-save)
(bind-key "C-S-V" #'yank)
(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-h C-f" #'find-function-at-point)
(bind-key "C-h C-k" #'find-function-on-key)
(bind-key "C-h C-v" #'find-variable-at-point)
(bind-key "C-w" #'init-kill-region-or-backward-word)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x k" #'init-kill-buffer-current)
(bind-key "M-/" #'hippie-expand)
(bind-key "M-L" #'init-sexp-downcase)
(bind-key "M-R" #'repeat)
(bind-key "M-SPC" #'init-delete-excess-whitespace)
(bind-key "M-U" #'init-sexp-upcase)

(delete-selection-mode 1)

(global-subword-mode 1)
(diminish 'subword-mode "")

;; Use tree-sitter modes.
(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-mode . c-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        ))

;;; Packages

(use-package abbrev
  :ensure nil
  :diminish ""
  :hook (minibuffer-setup-hook . init-abbrev-minibuffer-setup)
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" init-config-directory))
  :init
  (progn
    (setq-default abbrev-mode t)

    (defun init-abbrev-minibuffer-setup ()
      (abbrev-mode -1))
    ))

(use-package align
  :ensure nil
  :config
  (progn
    (dolist (mode '(haskell-mode))
      (add-to-list 'align-open-comment-modes mode))

    (dolist (tr '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                  (haskell-assignment  . "\\(\\s-+\\)\\(=\\|:=\\|.=\\)\\s-+")
                  (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))
      (add-to-list 'align-rules-list
                   `(,(car tr) (regexp . ,(cdr tr))
                     (modes . '(haskell-mode literate-haskell-mode)))))))

(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '("secrets:Login" (abbrev-file-name (init-xdg-config "authinfo.gpg")))))

(use-package async
  :demand
  :hook (after-init-hook . async-bytecomp-package-mode))

(use-package autorevert
  :ensure nil
  :init (setq auto-revert-verbose nil
              auto-revert-tail-mode-text " ⭛"))

(use-package bookmark
  :ensure nil
  :init (setq bookmark-default-file (expand-file-name "bookmarks" init-var-directory)
              bookmark-save-flag 1))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-default-scheme "https"))

(use-package calendar
  :ensure nil
  :custom (calendar-week-start-day 1))

(use-package compile
  :ensure nil
  :diminish (compilation-shell-minor-mode . "©")
  :bind (:map compilation-mode-map
              ("n" . next-error)
              ("p" . previous-error))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-scroll-output 'first-error))

(use-package dictionary
  :ensure nil
  :bind (("M-#" . dictionary-lookup-definition))
  :custom
  (dictionary-use-single-buffer t)
  )

(use-package diff-mode
  :ensure nil
  :config
  (progn
    (unbind-key "o" diff-mode-shared-map)))

(use-package dnd
  :ensure nil
  :custom
  (dnd-indicate-insertion-point t)
  (dnd-scroll-margin 1)
  )

(use-package editorconfig
  :demand
  :hook (after-init-hook . editorconfig-mode)
  :custom
  (editorconfig-mode-lighter "")
  )

(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode . "")
  :init
  (progn
    (setq-default eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))
  :config
  (progn
    (eldoc-add-command-completions "sp-")))

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
  (backup-by-copying t)
  (delete-old-versions t)
  (delete-old-versions t)
  (kept-new-versions 8)
  (safe-local-variable-values
   '((flycheck-emacs-lisp-load-path . inherit)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))
  (save-some-buffers-default-predicate 'save-some-buffers-root)
  (version-control t))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-mode-line-lighter "⚙")
  (flymake-show-diagnostics-at-end-of-line nil))

(use-package frame
  :ensure nil
  :config
  (progn
    (unbind-key "C-z")
    (unbind-key "C-x C-z")))

(use-package indent-aux
  :ensure nil
  :hook (after-init-hook . kill-ring-deindent-mode))

(use-package help
  :ensure nil
  :custom
  (describe-bindings-outline t))

(use-package help-fns
  :ensure nil
  :custom
  (help-enable-symbol-autoload t))

(use-package hideshow
  :ensure nil
  :diminish (hs-minor-mode . "…"))

(use-package hydra
  :demand)

(use-package kmacro
  :ensure nil
  :config
  (progn
    (defalias 'kmacro-insert-macro #'insert-kbd-macro)
    (define-key kmacro-keymap (kbd "i") #'kmacro-insert-macro)))

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

(use-package paren
  :ensure nil
  :custom
  (show-paren-context-when-offscreen 'overlay))

(use-package proced
  :ensure nil
  :custom
  (proced-enable-color-flag t))

(use-package project
  :ensure nil
  :custom
  (project-list-file (expand-file-name "projects" init-var-directory)))

(use-package simple
  :ensure nil
  :bind (("M-n" . next-error)
         ("M-p" . previous-error))
  :custom
  (next-error-message-highlight t))

(use-package smerge-mode
  :ensure nil
  :after hydra
  :bind (:map smerge-mode-map
              ("C-c m n" . hydra-smerge/smerge-next)
              ("C-c m p" . hydra-smerge/smerge-prev)
              ("C-c m a" . hydra-smerge/smerge-keep-all)
              ("C-c m l" . hydra-smerge/smerge-keep-lower)
              ("C-c m u" . hydra-smerge/smerge-keep-upper))
  :custom
  (smerge-command-prefix (kbd "C-c m"))
  :config
  (progn
    (defhydra hydra-smerge ()
      "smerge"
      ("n" smerge-next "next")
      ("p" smerge-prev "prev")
      ("e" smerge-ediff "ediff")
      ("r" smerge-refine "refine")
      ("a" smerge-keep-all "keep-all")
      ("c" smerge-keep-current "keep-current")
      ("l" smerge-keep-lower "keep-lower")
      ("u" smerge-keep-upper "keep-upper")
      ("w" save-buffer "done" :color blue)
      ("q" nil "quit"))))

(use-package time
  :ensure nil
  :custom
  (world-clock-time-format "%a %d %b %R %Z"))

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-obey-display-actions t))

(use-package xt-mouse
  :ensure nil
  :hook (after-init-hook . xterm-mouse-mode))
