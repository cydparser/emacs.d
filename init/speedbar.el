(defalias 'sb 'speedbar)

(defun init-speedbar ()
  (speedbar-add-supported-extension '(".hs" ".lhs")))

(add-hook 'speedbar-mode-hook 'init-speedbar)
