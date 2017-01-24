(use-package sdcv
  :defer t
  :bind (("C-c d" . init-define-word)
         :map sdcv-mode-map
         ("<RET>" . sdcv-search-pointer))
  :commands (init-define-word)
  :config
  (progn
    (defun init-define-word (input)
      (interactive "P")
      (if input
          (sdcv-search-input)
        (sdcv-search-pointer)))))
