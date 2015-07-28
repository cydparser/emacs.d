(defun init-emacs-lisp-mode ()
  (unless (equal (buffer-name) "*scratch*")
    (aggressive-indent-mode)
    (rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'init-emacs-lisp-mode)
