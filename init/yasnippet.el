(use-package yasnippet
  :demand
  :diminish (yas-minor-mode . "")
  :init (yas-global-mode)
  :config
  (progn
    (defun init-yas-uncapitalize (cap)
      (concat (downcase (substring cap 0 1))
              (substring cap 1)))
    (yas-load-directory (expand-file-name "snippets" user-emacs-directory))))
