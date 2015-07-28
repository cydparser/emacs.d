(require 'json-mode)

(defun init-json-mode ()
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'init-json-mode)
