(init-package-install 'aggressive-indent)

(defun tweak-emacs-lisp-mode ()
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook 'tweak-emacs-lisp-mode)
