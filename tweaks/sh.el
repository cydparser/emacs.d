(let ((spaces 2))
  (setq sh-basic-offset spaces
        sh-indentation spaces))

(defun tweak-sh-mode ()
  (sh-set-shell "/bin/bash" t t))

(add-hook 'sh-mode-hook 'tweak-sh-mode)
