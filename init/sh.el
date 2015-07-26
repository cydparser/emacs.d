(let ((spaces 2))
  (setq sh-basic-offset spaces
        sh-indentation spaces))

(defun init-sh-mode ()
  (unless (file-exists-p buffer-file-name)
    (sh-set-shell "/bin/bash" t t)))

(add-hook 'sh-mode-hook 'init-sh-mode)
