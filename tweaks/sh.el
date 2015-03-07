(let ((spaces 2))
  (setq sh-basic-offset spaces
        sh-indentation spaces))

(defun tweak-sh-mode ()
  (unless (file-exists-p buffer-file-name)
    (sh-set-shell "/bin/bash" t t)))

(add-hook 'sh-mode-hook 'tweak-sh-mode)
