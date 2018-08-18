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

(use-package string-inflection
  :bind (("C-c r c -" . string-inflection-kebab-case)      ; kebab-case
         ("C-c r c c" . string-inflection-camelcase)       ; CamelCase
         ("C-c r c l" . string-inflection-lower-camelcase) ; lowerCamelCase
         ("C-c r c s" . string-inflection-underscore)      ; snake_case
         ("C-c r c u" . string-inflection-upcase)))        ; UPPER_SNAKE_CASE
