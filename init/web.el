;;; -*- lexical-binding: t -*-

(use-package css-mode
  :ensure nil
  :hook (web-mode-hook . init-css)
  :mode (("\\.gss\\'" . css-mode))
  :init
  (progn
    (defun init-css ()
      (add-to-list (make-local-variable 'company-backends) #'company-css))))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode))
  :hook (web-mode-hook . init-web)
  :init
  (progn
    (defun init-web ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml)))
  :config
  (progn
    (unbind-key "C-c C-f" web-mode-map)
    ))
