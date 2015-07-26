;;; sensory

;; startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message ";; *scratch*\n\n")

;; enable syntax highlighting
(global-font-lock-mode 1)

(set-default-font "Inconsolata 14")

(global-prettify-symbols-mode 1)

;; simplify prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; disable prompts
(setq confirm-nonexistent-file-or-buffer nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; disable bell
(setq ring-bell-function 'ignore)

(setq echo-keystrokes 0.125)
(setq column-number-mode t)

;;; formatting

;; indenting
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (number-sequence 4 120 4))

;; line-wrapping
(setq-default fill-column 100)

(global-subword-mode 1)

(setq kill-whole-line t)

;;; behavior

(setq compilation-auto-jump-to-first-error t
      mode-compile-always-save-buffer-p t)

(setq gc-cons-threshold 20000000)

;;; aliases

(defalias 'ar 'align-regex)
(defalias 'rs 'replace-string)
(defalias 'sl 'sort-lines)

;;; bindings

(global-set-key (kbd "C-c C-SPC") 'delete-trailing-whitespace)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-g M-f") 'first-error)

;;; exiting

(setq confirm-kill-emacs 'yes-or-no-p)
