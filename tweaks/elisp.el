(init-package-install 'aggressive-indent)
(init-package-install 'rainbow-delimiters)

(defun tweak-emacs-lisp-mode ()
  (unless (equal (buffer-name) "*scratch*")
    (aggressive-indent-mode)
    (rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'tweak-emacs-lisp-mode)
