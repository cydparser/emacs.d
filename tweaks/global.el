;;; sensory

;; startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; display flymake messages in minibuffer
(init-package-require 'flymake-cursor)

;; enable syntax highlighting
(global-font-lock-mode 1)

;; display matchign parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; disable prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; disable bell
(setq ring-bell-function (lambda ()))

(setq echo-keystrokes 0.125)
(setq column-number-mode t)

;;; formatting

;; tabs
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (number-sequence 4 120 4))

;; line-wrapping
(setq-default fill-column 100)

(global-subword-mode 1)

;;; behavior

(setq mode-compile-always-save-buffer-p t)

;;; bindings

(global-set-key (kbd "C-c C-SPC") 'delete-trailing-whitespace)
(global-set-key (kbd "M-/") 'hippie-expand)
