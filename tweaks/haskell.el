(init-package-install 'haskell-mode)
(init-package-install 'flymake-haskell-multi)

(setq haskell-tags-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

(defun tweak-haskell-mode ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (flymake-haskell-multi-load)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'tweak-haskell-mode)
