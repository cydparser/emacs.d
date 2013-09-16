(init-package-install 'haskell-mode)
(init-package-install 'flymake-haskell-multi)

(setq haskell-tags-on-save t)

(require 'speedbar)
(speedbar-add-supported-extension ".hs")

(defun tweak-haskell-mode ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (flymake-haskell-multi-load))

(add-hook 'haskell-mode-hook 'tweak-haskell-mode)
