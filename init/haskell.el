;; stack install apply-refact hasktags hindent hlint hoogle

(setq haskell-compile-cabal-build-alt-command "cd %s && stack clean && stack build --ghc-options -ferror-spans"
      haskell-compile-cabal-build-command "cd %s && stack build --ghc-options -ferror-spans"
      haskell-compile-command "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s"
      haskell-interactive-popup-errors nil
      haskell-process-args-stack-ghci
      (list "--fast"
            "--ghc-options"
            (concat
             "-XNamedWildCards -XPartialTypeSignatures -ferror-spans -fdefer-type-errors"
             " -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-type-defaults"))
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-hayoo-imports t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type 'stack-ghci
      hindent-style "chris-done")

(when (executable-find "hasktags")
  (setq haskell-tags-on-save t))

(defalias 'hay 'haskell-hayoo)
(defalias 'hoo 'haskell-hoogle)

(defun init-haskell-goto-next-error ()
  "Go to the next Haskell or flycheck error."
  (interactive)
  (if (init-haskell-check-overlays-p)
      (haskell-goto-next-error)
    (flycheck-next-error)))

(defun init-haskell-goto-prev-error ()
  "Go to the previous Haskell or flycheck error."
  (interactive)
  (if (init-haskell-check-overlays-p)
      (haskell-goto-prev-error)
    (flycheck-previous-error)))

(defun init-haskell-check-overlays-p ()
  (car (haskell-check-filter-overlays
        (overlays-in (point-min) (point-max)))))

(defun init-haskell-process-insert-type ()
  "Insert the type of the identifier at point."
  (interactive)
  (haskell-process-do-type :insert-value))

(defun init-haskell-process-reload-switch ()
  "Reload file and switch to the REPL."
  (interactive)
  (haskell-process-load-or-reload)
  (haskell-interactive-switch))

(defun init-haskell-mode-map (map)
  (define-key map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key map (kbd "C-c c") 'haskell-process-cabal)
  (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear))

(with-eval-after-load 'haskell-mode
  (let ((map haskell-mode-map))
    (init-haskell-mode-map map)
    (define-key map (kbd "C-c t") 'init-haskell-process-insert-type)
    (define-key map (kbd "C-c o") 'init-haskell-process-reload-switch)
    (define-key map (kbd "C-c C-p") 'haskell-navigate-imports)))

(with-eval-after-load 'haskell
  (let ((map interactive-haskell-mode-map))
    (define-key map (kbd "M-n") 'init-haskell-goto-next-error)
    (define-key map (kbd "M-p") 'init-haskell-goto-prev-error)))

(with-eval-after-load 'haskell-cabal
  (let ((map haskell-cabal-mode-map))
    (init-haskell-mode-map map)
    (define-key map (kbd "C-c o") 'haskell-interactive-switch)))

(with-eval-after-load 'haskell-interactive-mode
  (let ((map haskell-interactive-mode-map))
    (define-key map (kbd "C-c o") 'haskell-interactive-switch-back)))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-cabal)
  (add-to-list 'company-backends 'company-ghci))

(with-eval-after-load 'company-ghci
  (when (and (executable-find "hoogle") (file-exists-p (init-xdg-data "hoogle")))
    ;; Use local database.
    (defun company-ghci/hoogle-info (symbol)
      "Use hoogle --info to search documentation of SYMBOL"
      (shell-command-to-string (format "hoogle --info -d %s %s" (init-xdg-data "hoogle") symbol)))))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

(defun init-haskell-mode ()
  (hindent-mode)
  (hlint-refactor-mode)
  (interactive-haskell-mode)
  (company-mode))

(add-hook 'haskell-mode-hook 'init-haskell-mode)

(defun init-haskell-cabal-mode ()
  (flycheck-mode -1))

(add-hook 'haskell-cabal-mode-hook 'init-haskell-cabal-mode)

(defun init-haskell-interactive-mode ()
  (company-mode)
  (flycheck-mode -1))

(add-hook 'haskell-interactive-mode-hook 'init-haskell-interactive-mode)
