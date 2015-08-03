(defalias 'sb 'speedbar)

(autoload 'speedbar-add-supported-extension "speedbar")

(defun init-speedbar ()
  (speedbar-add-supported-extension '(".hs" ".lhs")))

(add-hook 'speedbar-mode-hook 'init-speedbar)
