(use-package yasnippet
  :demand
  :diminish (yas-minor-mode . "")
  :init
  (progn
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
    (yas-global-mode))
  :config
  (progn
    (defun init-yas-uncapitalize (cap)
      (concat (downcase (substring cap 0 1))
              (substring cap 1)))))
