;;; -*- lexical-binding: t -*-

(require 'init-hasklig)

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :init
  (progn
    (setq hl-todo-keyword-faces
          '(("FAIL"  . "#cc6666")
            ("FIXME" . "#cc6666")
            ("HACK"  . "#de935f")
            ("NB"    . "#f0c674")
            ("NOTE"  . "#f0c674")
            ("TODO"  . "#de935f")
            ("XXX"   . "#de935f")))))

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

(use-package repl-toggle
  :diminish ""
  :bind (("C-c t r" . rtog/toggle-repl))
  :hook (prog-mode-hook . rtog/activate)
  :init (setq rtog/goto-buffer-fun #'pop-to-buffer
              rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
                                     (js-mode . nodejs-repl)
                                     (lisp-interaction-mode . ielm)
                                     (ruby-mode . inf-ruby))))

(use-package string-inflection
  :bind (("C-c r c -" . string-inflection-kebab-case)      ; kebab-case
         ("C-c r c c" . string-inflection-camelcase)       ; CamelCase
         ("C-c r c l" . string-inflection-lower-camelcase) ; lowerCamelCase
         ("C-c r c s" . string-inflection-underscore)      ; snake_case
         ("C-c r c u" . string-inflection-upcase)))        ; UPPER_SNAKE_CASE
