;; stack install hasktags hindent hlint hoogle

(init-package-install 'haskell-mode)

(setq haskell-cabal-commands '("bench" "build" "clean" "docker" "dot" "exec" "ghc" "ghci" "haddock"
                               "ide" "image" "init" "install" "new" "path" "runghc" "setup" "solver"
                               "test" "uninstall" "unpack" "update" "upgrade" "upload")
      haskell-compile-cabal-build-alt-command "cd %s && stack clean && stack build --ghc-options -ferror-spans"
      haskell-compile-cabal-build-command "cd %s && stack build --ghc-options -ferror-spans"
      haskell-compile-command "stack ghc -Wall -ferror-spans -fforce-recomp -c %s"
      haskell-process-args-cabal-repl "ghci --ghc-options -ferror-spans"
      haskell-process-args-ghci "ghci --ghc-options -ferror-spans"
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      ;; HACK: `haskell-process-do-cabal` always uses `haskell-process-path-cabal`.
      haskell-process-path-cabal "stack"
      haskell-process-path-ghci "stack"
      haskell-process-suggest-hoogle-imports t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type 'stack-ghci
      hindent-style "chris-done")

(when (executable-find "hasktags")
  (setq haskell-tags-on-save t))

(defalias 'hoo 'haskell-hoogle)

(defun init-haskell-process-insert-type ()
  "Insert the type of the identifier at point."
  (interactive)
  (haskell-process-do-type :insert-value))

(defun init-haskell-process-reload-switch ()
  "Reload file and switch to the REPL."
  (interactive)
  (haskell-process-load-or-reload)
  (haskell-interactive-switch))

(defun init-haskell-process-type-ghci (f &rest args)
  "HACK: Temporarily modify `haskell-process-type` since 'stack-ghci is not fully functional."
  (let ((haskell-process-type 'ghci))
    (apply f args)))

(advice-add 'haskell-process-do-cabal :around #'init-haskell-process-type-ghci)

(defun init-haskell-mode-map (map)
  (define-key map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key map (kbd "C-c c") 'haskell-process-cabal)
  (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear))

(with-eval-after-load 'haskell-mode
  (let ((map haskell-mode-map))
    (init-haskell-mode-map map)
    (define-key map (kbd "C-c t") 'init-haskell-process-insert-type)
    (define-key map (kbd "C-c C-r") 'init-haskell-process-reload-switch)
    (define-key map (kbd "SPC") 'haskell-mode-contextual-space)
    (define-key map (kbd "C-c C-p") 'haskell-navigate-imports)
    ))

(with-eval-after-load 'haskell-cabal
  (let ((map haskell-cabal-mode-map))
    (init-haskell-mode-map map)
    (define-key map (kbd "C-c C-r") 'haskell-interactive-switch)))

(with-eval-after-load 'haskell-interactive-mode
  (let ((map haskell-interactive-mode-map))
    (define-key map (kbd "C-c C-r") 'previous-multiframe-window)))

(defun init-haskell-mode ()
  (haskell-auto-insert-module-template)
  (haskell-indentation-mode)
  (hindent-mode)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'init-haskell-mode)

(defun init-haskell-interactive-mode ()
  (flycheck-mode -1))

(add-hook 'haskell-interactive-mode-hook 'init-haskell-interactive-mode)
