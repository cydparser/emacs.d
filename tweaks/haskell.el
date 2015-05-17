;; cabal install happy hasktags hlint present stylish-haskell
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(init-package-install 'ghc)
(init-package-install 'haskell-mode)
;; (init-package-install 'flymake-haskell-multi)

(setq
 haskell-tags-on-save t
 haskell-process-auto-import-loaded-modules t
 haskell-process-log t
 haskell-process-suggest-remove-import-lines t
 haskell-process-type 'cabal-repl)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun tweak-haskell-mode ()
  (ghc-init)
  (turn-on-haskell-doc-mode)
  (haskell-indentation-mode)
  ;; (flymake-haskell-multi-load)
  ;; (interactive-haskell-mode)
  (let ((hm haskell-mode-map))
    ;; (define-key m (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key hm (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key hm (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key hm (kbd "C-c C-o") 'haskell-compile)
    (define-key hm (kbd "C-c C-n C-t") 'haskell-process-do-type)
    ;; (define-key m (kbd "C-c C-n c") 'haskell-process-cabal)
    (define-key hm (kbd "C-c C-p") 'haskell-navigate-imports)
    (define-key hm (kbd "C-c C-y") 'haskell-mode-stylish-buffer)
    (define-key hm (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key hm (kbd "SPC") 'haskell-mode-contextual-space)))

(add-hook 'haskell-mode-hook 'tweak-haskell-mode)

(eval-after-load 'haskell-cabal
  '(progn
     (let ((cm haskell-cabal-mode-map))
       (define-key cm (kbd "C-c C-c") 'haskell-process-cabal-build)
       (define-key cm (kbd "C-c C-k") 'haskell-interactive-mode-clear)
       (define-key cm (kbd "C-c C-o") 'haskell-compile)
       (define-key cm (kbd "C-c C-z") 'haskell-interactive-switch)
       (define-key cm (kbd "C-c c") 'haskell-process-cabal))))
