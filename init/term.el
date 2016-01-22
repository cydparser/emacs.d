(defalias 'sh 'multi-term)

(defun init-term-mode ()
  (init-whitespace-disable))

(add-hook 'term-mode-hook 'init-term-mode)
