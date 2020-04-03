;;; -*- lexical-binding: t -*-

(require 'init-hasklig)

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :init
  (progn
    (setq hl-todo-keyword-faces
          '(("WTF"   . "#c82829")
            ("XXX"   . "#c82829")
            ("FAIL"  . "#cc6666")
            ("FIXME" . "#de935f")
            ("HACK"  . "#de935f")
            ("TODO"  . "#f0c674")
            ("NB"    . "#8abeb7")
            ("NOTE"  . "#8abeb7")))))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode-hook . init-prog-mode)
  :init
  (progn
    (setq prettify-symbols-unprettify-at-point 'right-edge)

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
              rtog/mode-repl-alist '((dhall-mode . dhall-repl-show)
                                     (emacs-lisp-mode . ielm)
                                     (js-mode . nodejs-repl)
                                     (lisp-interaction-mode . ielm)
                                     (ruby-mode . inf-ruby))))

(use-package string-inflection
  :commands (string-inflection-lower-camelcase-function
             string-inflection-underscore-function)
  :bind (("C-c r c -" . string-inflection-kebab-case)      ; kebab-case
         ("C-c r c c" . string-inflection-camelcase)       ; CamelCase
         ("C-c r c l" . string-inflection-lower-camelcase) ; lowerCamelCase
         ("C-c r c s" . string-inflection-underscore)      ; snake_case
         ("C-c r c u" . string-inflection-upcase)))        ; UPPER_SNAKE_CASE

(use-package sql
  :ensure nil
  :hook (sql-mode-hook . init-sql)
  :init
  (progn
    (defun init-sql ()
      (setq indent-line-function #'init-sql-indent))

    (defun init-sql-indent ()
      (indent-line-to
       (save-excursion
         (forward-line -1)
         (beginning-of-line)
         (cond ((looking-at-p ".+;[	 ]*\\(--.*\\)?$") 0)
               ((looking-at-p "^[	 ]*\\($\\|--\\)") 0)
               ((looking-at-p "^[a-zA-Z\\]") standard-indent)
               (t (back-to-indentation)
                  (max (current-column) standard-indent))))))))
