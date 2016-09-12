;; stack install apply-refact hlint

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
  :defer t
  :init
  (progn
    ;; TODO: Investigate haskell-snippets errors during snippet expansion.
    (with-eval-after-load 'yasnippet
      (haskell-snippets-initialize))))

(use-package hlint-refactor
  :defer t
  :diminish ""
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package intero
  :defer t
  :diminish " λ"
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
