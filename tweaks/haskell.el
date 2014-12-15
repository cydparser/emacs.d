(init-package-install 'haskell-mode)
(init-package-install 'flymake-haskell-multi)

(setq haskell-tags-on-save t
      ;; interactive-haskell-mode
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

(defun tweak-haskell-mode ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (flymake-haskell-multi-load)
  (interactive-haskell-mode))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

(defun tweak-haskell-ghc-mode-mode ()
  (ghc-init))

(add-hook 'haskell-mode-hook 'tweak-haskell-mode)
