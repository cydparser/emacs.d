;;; -*- lexical-binding: t -*-

(require 'init-hasklig)

(use-package prog-mode
  :ensure nil
  :hook (prog-mode-hook . init-prog-mode)
  :init
  (progn
    (defun init-prettify-symbols ()
      (unless (derived-mode-p 'haskell-mode)
        (setq-local prettify-symbols-alist
                    init-hasklig-prettify-symbols-common-alist)
        (prettify-symbols-mode)))

    (defun init-prog-mode ()
      (init-prettify-symbols))))
