;;; -*- lexical-binding: t -*-

;; npm install -g csslint jsonlint

(use-package css-mode
  :ensure nil
  :hook (web-mode-hook . init-css)
  :config
  (progn
    (defun init-css ()
      (add-to-list (make-local-variable 'company-backends) #'company-css))))

(use-package haml-mode)

(use-package json-mode)

(use-package scss-mode
  :hook (web-mode-hook . init-scss)
  :config
  (progn
    (defun init-scss ()
      (add-to-list (make-local-variable 'company-backends) #'company-css))))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode))
  :hook (web-mode-hook . init-web)
  :config
  (progn
    (defun init-web ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml))))
