;; stack install apply-refact codex hasktags hlint

(use-package cmm-mode
  :defer t)

(use-package company-cabal
  :defer t
  :init
  (progn
    (defun init-haskell-cabal ()
      (add-to-list 'company-backends #'company-cabal))

    (add-hook 'haskell-cabal-mode-hook #'init-haskell-cabal)))

(use-package haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-." . haskell-sort-imports))
  :init
  (progn
    (setq haskell-compile-cabal-build-alt-command
          "cd %s && stack clean && stack build --ghc-options -ferror-spans"
          haskell-compile-cabal-build-command
          "cd %s && stack build --ghc-options -ferror-spans"
          haskell-compile-command
          "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s"))
  :config
  (progn
    (speedbar-add-supported-extension '(".hs" ".lhs"))))

(use-package haskell-snippets
  :defer t)

(use-package hlint-refactor
  :defer t
  :diminish ""
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package intero
  :defer t
  :diminish " λ"
  :bind (:map intero-mode-map
              ("M-." . init-intero-goto-definition))
  :init
  (progn
    (defun init-intero ()
      "Enable Intero unless visiting a cached dependency."
      (if (and buffer-file-name
               (string-match ".+/\\.\\(stack\\|stack-work\\)/.+" buffer-file-name))
          (flycheck-mode -1)
        (intero-mode)
        (eldoc-mode)))

    (add-hook 'haskell-mode-hook #'init-intero))
  :config
  (progn
    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (find-tag (find-tag-default))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
