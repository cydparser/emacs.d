;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :demand
  :diminish (yas-minor-mode . "")
  :hook (after-init-hook . yas-global-mode)
  :init
  (progn
    (setq-default yas-indent-line 'fixed)
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  :config
  (progn
    (defun init-yas-uncapitalize (cap)
      (concat (downcase (substring cap 0 1))
              (substring cap 1)))

    (unbind-key "TAB" yas-minor-mode-map)
    (unbind-key "<tab>" yas-minor-mode-map)))
