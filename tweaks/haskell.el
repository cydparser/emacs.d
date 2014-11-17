(init-package-install 'haskell-mode)

(setq haskell-tags-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun tweak-haskell-mode ()
  (ghc-init))

(add-hook 'haskell-mode-hook 'tweak-haskell-mode)
