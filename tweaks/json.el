(init-package-require 'json-mode)

(defun tweak-json-mode ()
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'tweak-json-mode)
