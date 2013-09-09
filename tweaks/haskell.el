(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indentation)))
