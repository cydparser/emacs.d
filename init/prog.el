;;; -*- lexical-binding: t -*-

(use-package hl-todo
  :hook ((prog-mode-hook . hl-todo-mode)
         (haskell-cabal-mode-hook . hl-todo-mode)
         (org-mode-hook . hl-todo-mode)
         (toml-ts-mode-hook . hl-todo-mode)
         (typst-ts-mode-hook . hl-todo-mode)
         (yaml-ts-mode-hook . hl-todo-mode))
  :init
  (progn
    (with-eval-after-load 'magit
      (require 'hl-todo)
      (add-hook 'magit-log-wash-summary-hook
                #'hl-todo-search-and-highlight 90)
      (add-hook 'magit-revision-wash-message-hook
                #'hl-todo-search-and-highlight 90)))
  :custom
  (hl-todo-keyword-faces
   '(("WTF"   . "#c82829")
     ("XXX"   . "#c82829")
     ("FAIL"  . "#cc6666")
     ("FIXME" . "#de935f")
     ("HACK"  . "#de935f")
     ("TODO"  . "#f0c674")
     ("NB"    . "#81a2be")
     ("NOTE"  . "#81a2be"))))

(use-package lisp-mode
  :hook ((emacs-lisp-mode-hook . init-lisp)
         (lisp-mode-hook . init-lisp))
  :ensure nil
  :config
  (progn
    (require 'init-font)

    (defconst init-lisp-prettify-symbols
      (list
       (init-font-lig-rule-replace "lambda" ?λ)))

    (defun init-lisp ()
      (setq-local prettify-symbols-alist init-lisp-prettify-symbols)
      (prettify-symbols-mode))))

(use-package lua-mode
  :custom
  (lua-indent-level 2)
  :config (setq-default lua-electric-flag nil))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode-hook . init-prog-mode)
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :init
  (progn
    (defun init-prog-mode ()
      (electric-indent-local-mode))))

(use-package repl-toggle
  :diminish ""
  :bind (("C-c t r" . rtog/toggle-repl))
  :hook (prog-mode-hook . rtog/activate)
  :init (setq rtog/goto-buffer-fun #'pop-to-buffer
              rtog/mode-repl-alist '((dhall-mode . dhall-repl-show)
                                     (emacs-lisp-mode . ielm)
                                     (js-mode . nodejs-repl)
                                     (lisp-interaction-mode . ielm))))

(use-package rust-mode
  :bind (:map rust-mode-map
              (":" . init-rust-insert-colon)
              ("M-<return>" . eglot-x-on-enter)
              ("C-c C-f" . rust-format-buffer))
  :custom
  (rust-mode-treesitter-derive t)
  :config
  (progn
    (require 'init-utils)

    (defun init-rust-insert-colon ()
      "Insert a colon unless the previous character is a colon or the following
character is a space or colon"
      (interactive)
      (let ((prev-char (char-before)))
        (insert-char ?:)
        (unless (or (char-equal ?: prev-char)
                    (char-equal ?{ prev-char)
                    (init-treesit-first-ancestor-with-type
                     [token_tree_pattern string_content] :include-node 'rust))
          (let ((char (read-key nil :disable)))
            (when char
              (cond ((not
                      (and (characterp char)
                           (<= ?\s char)
                           (or (>= ?z char)
                               ;; The only modifier pressed is shift.
                               (= 0 (logand char
                                            ;; Codes extracted from `event-modifiers'.
                                            (logior ?\M-\0 ; meta
                                                    ?\C-\0 ; ctrl
                                                    ?\H-\0 ; hyper
                                                    ?\s-\0 ; super
                                                    ?\A-\0 ; alt
                                                    ))))))
                     (setq unread-command-events (list char)))
                    ((or (char-equal ?: char)
                         (char-equal ?\s char))
                     (insert-char char))
                    (t
                     (insert-char ?:)
                     (insert-char char))))))))

    (require 'rust-compile)

    ;; thread 'tests::test_div_ceil2' (1769198) panicked at bitfield/impl/src/lib.rs:80:9:
    (add-to-list 'compilation-error-regexp-alist-alist
                 (cons 'rustc-thread-panicked
                       (cons (concat "^thread [^ ]+ [^ ]+ panicked at " rustc-compilation-location) '(2 3 4 0 1))))

    (add-to-list 'compilation-error-regexp-alist-alist
                 (cons 'rustc-arrow
                       (cons (concat "^ +--> " rustc-compilation-location) '(2 3 4 0 1))))))

(use-package rustic
  :after (rust-mode)
  :custom
  (rustic-lsp-client 'eglot))

(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

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
  :custom (sql-product 'postgres)
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
