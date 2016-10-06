(use-package aggressive-indent
  :defer t)

(use-package rainbow-delimiters
  :defer t)

(use-package lisp-mode
  :defer t
  :ensure nil
  :init
  (progn
    (defun init-emacs-lisp-mode ()
      (setq mode-name "elisp")
      (unless (init-special-buffer-p)
        (aggressive-indent-mode)
        (rainbow-delimiters-mode)))

    (add-hook 'emacs-lisp-mode-hook #'init-emacs-lisp-mode)))
