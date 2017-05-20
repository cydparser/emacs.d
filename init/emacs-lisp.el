;;; -*- lexical-binding: t -*-

(use-package aggressive-indent
  :defer t)

(use-package elisp-slime-nav
  :defer t
  :diminish ""
  :init
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook #'turn-on-elisp-slime-nav-mode))))

(use-package rainbow-delimiters
  :defer t)

(use-package lisp-mode
  :defer t
  :ensure nil
  :init
  (progn
    (defun init-emacs-lisp-mode ()
      (setq mode-name "elisp")
      (add-to-list (make-local-variable 'company-backends) #'company-elisp)
      (unless (init-special-buffer-p)
        (aggressive-indent-mode)
        (rainbow-delimiters-mode)))

    (add-hook 'emacs-lisp-mode-hook #'init-emacs-lisp-mode)))
