;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package aggressive-indent
  :diminish "⃕")

(use-package elisp-slime-nav
  :diminish ""
  :hook ((emacs-lisp-mode-hook ielm-mode-hook) . turn-on-elisp-slime-nav-mode))

(use-package lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode-hook . init-emacs-lisp-mode)
  :init
  (progn
    (defun init-emacs-lisp-mode ()
      (setq mode-name "elisp")
      (add-to-list (make-local-variable 'company-backends) #'company-elisp)
      (unless (init-special-buffer-p)
        (aggressive-indent-mode)
        (rainbow-delimiters-mode)))))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package pcre2el
  :hook (emacs-lisp-mode-hook . rxt-mode))

(use-package rainbow-delimiters)
