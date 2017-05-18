;;; -*- lexical-binding: t -*-

;; stack install apply-refact codex hasktags hlint

(use-package cmm-mode
  :defer t)

(use-package company-cabal
  :defer t
  :init
  (progn
    (defun init-haskell-cabal ()
      (add-to-list (make-local-variable 'company-backends) #'company-cabal))

    (add-hook 'haskell-cabal-mode-hook #'init-haskell-cabal)))

(use-package dante
  :defer t
  :diminish " Δ"
  :bind (:map dante-mode-map
              ("C-c C-i" . dante-info)
              ("C-c C-r" . dante-auto-fix)
              ("C-c C-t" . dante-type-at))
  :init (defalias 'init-dante 'dante-mode)
  :config
  (progn
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    (unbind-key "C-c ." dante-mode-map)
    (unbind-key "C-c ," dante-mode-map)
    (unbind-key "C-c /" dante-mode-map)))

(use-package haskell-mode
  :defer t
  :diminish (haskell-mode . " λ")
  :bind (:map haskell-mode-map
              ("M-g i" . haskell-navigate-imports)
              ("M-g M-i" . haskell-navigate-imports))
  :init
  (progn
    (setq haskell-font-lock-symbols t
          haskell-font-lock-symbols-alist
          '(("." "∘" haskell-font-lock-dot-is-not-composition)))
    (setq haskell-compile-cabal-build-alt-command
          "cd %s && stack clean && stack build --ghc-options -ferror-spans"
          haskell-compile-cabal-build-command
          "cd %s && stack build --ghc-options -ferror-spans"
          haskell-compile-command
          "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s")
    (defvar init-haskell-backend-function 'init-intero)

    (defconst init-haskell-prettify-symbols-alist
      (let ((exclude '("&&" "||")))
        (append
         '(("\\" . "λ")
           ("&&" . "∧")
           ("||" . "∨")
           ("not" . "¬")
           ("empty" . "∅")
           ("forall" . "∀")
           ("pi" . "π")
           ("undefined" . "⊥")
           ("`elem`" . "∈")
           ("`notElem`" . "∉")
           ("`member`" . "∈")
           ("`notMember`" . "∉")
           ("`isSubsetOf`" . "⊆")
           ("`isProperSubsetOf`" . "⊂")
           ("`intersection`" . "∩")
           ("`union`" . "∪"))
         (seq-remove (lambda (pair) (member (car pair) exclude))
                     init-hasklig-prettify-symbols-alist))))

    (defun init-haskell-change-backend (backend)
      "Change Haskell backend for future buffers."
      (interactive (list (completing-read "Import: " '("dante" "intero" " ") nil t)))
      (setq init-haskell-backend-function
            (and backend
                 (not (string-blank-p backend))
                 (intern (concat "init-" backend)))))

    (defun init-haskell-format-imports ()
      "Align and sort all imports."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (haskell-navigate-imports)
        (haskell-mode-format-imports)

        (while (progn (haskell-navigate-imports)
                      (looking-at "^import "))
          (haskell-sort-imports))))

    (defun init-haskell ()
      (cond ((and buffer-file-name
                  (string-match "^\\(/nix\\|.+/\\.\\(cabal\\|stack\\|stack-work\\)\\)/.+"
                                buffer-file-name))
             (eldoc-mode -1)
             (flycheck-mode -1))
            (t
             (setq company-dabbrev-downcase nil
                   company-dabbrev-ignore-case :ignore-case)
             (set (make-local-variable 'projectile-tags-command) "codex update")
             (setq prettify-symbols-alist init-haskell-prettify-symbols-alist)
             (prettify-symbols-mode)
             (when (fboundp init-haskell-backend-function)
               (funcall init-haskell-backend-function)))))

    (add-hook 'haskell-mode-hook #'init-haskell))
  :config
  (progn
    (remove-hook 'haskell-mode-hook #'interactive-haskell-mode)))

(use-package haskell-snippets
  :defer t)

(use-package hlint-refactor
  :defer t
  :diminish ""
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package intero
  :defer t
  :diminish " η"
  :bind (:map intero-mode-map
              ("C-c r i" . init-intero-add-import)
              ("M-." . init-intero-goto-definition))
  :init (defalias 'init-intero 'intero-mode)
  :config
  (progn
    (defun init-intero-insert-import (callback)
      "Insert a module using completing read.

The string 'import ' will be inserted as well, if missing."
      (interactive '(nil))
      (beginning-of-line)
      (if (looking-at "^import ")
          (progn
            (end-of-line)
            (just-one-space))
        (insert "import "))
      (let ((p (point)))
        (intero-get-repl-completions
         (current-buffer) "import "
         (lambda (modules)
           (save-excursion
             (goto-char p)
             (insert (completing-read "Import: " modules))
             (if callback (funcall callback)))))))

    (defun init-intero-add-import ()
      "Add an import, format imports, and keep current position."
      (interactive)
      (save-excursion
        (haskell-navigate-imports)
        (open-line 1)
        (init-intero-insert-import #'init-haskell-format-imports)))

    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (xref-find-definitions (xref-backend-identifier-at-point
                                  (xref-find-backend)))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
