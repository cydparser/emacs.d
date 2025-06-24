;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package markdown-mode)

(use-package nxml-mode
  :ensure nil
  :hook (web-mode-hook . init-nxml)
  :init
  (progn
    (defun init-nxml ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml)))
  :config
  (progn
    (unbind-key "C-c C-f" nxml-mode-map)))

(use-package tex
  :ensure nil
  :custom
  (TeX-auto-save t)
  (TeX-newline-function 'newline-and-indent)
  (TeX-parse-self t))
