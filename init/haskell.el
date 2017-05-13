;;; -*- lexical-binding: t -*-

;; stack install apply-refact codex hasktags hlint

(defvar init--haskell-backend :intero
  "Backend to use for realtime errors, completions, etc.")

(defun init-haskell-backend-hook (keyword)
  "Convert KEYWORD symbol into a symbol beginning with 'init-'."
  (intern (concat "init-" (substring (symbol-name keyword) 1 nil))))

(defun init-haskell-backend-change (backend)
  "Switch Haskell backend.

This only affects new buffers."
  (interactive
   (let ((s (completing-read "Import: " '("dante" "intero") nil t)))
     (list (intern (concat ":" s)))))
  (when (not (eq backend init--haskell-backend))
    (remove-hook 'haskell-mode-hook (init-haskell-backend-hook init--haskell-backend))
    (add-hook 'haskell-mode-hook (init-haskell-backend-hook backend))
    (setq init--haskell-backend backend)))

(use-package cmm-mode
  :defer t)

(use-package company-cabal
  :defer t
  :init
  (progn
    (defun init-haskell-cabal ()
      (add-to-list (make-local-variable 'company-backends) #'company-cabal))

    (add-hook 'haskell-cabal-mode-hook #'init-haskell-cabal)))

(use-package dante
  :defer t
  :bind (:map dante-mode-map
              ("C-c C-i" . dante-info)
              ("C-c C-r" . dante-auto-fix)
              ("C-c C-t" . dante-type-at))
  :init
  (progn
    (defun init-dante ()
      (dante-mode)
      (flycheck-mode))

    (when (eq :dante init--haskell-backend)
      (add-hook 'haskell-mode-hook #'init-dante)))
  :config
  (progn
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    (unbind-key "C-c ." dante-mode-map)
    (unbind-key "C-c ," dante-mode-map)
    (unbind-key "C-c /" dante-mode-map)))

(use-package haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("M-g i" . haskell-navigate-imports)
              ("M-g M-i" . haskell-navigate-imports))
  :init
  (progn
    (setq haskell-compile-cabal-build-alt-command
          "cd %s && stack clean && stack build --ghc-options -ferror-spans"
          haskell-compile-cabal-build-command
          "cd %s && stack build --ghc-options -ferror-spans"
          haskell-compile-command
          "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s")
    (remove-hook 'haskell-mode-hook #'interactive-haskell-mode))
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
              ("C-c r i" . init-intero-add-import)
              ("M-." . init-intero-goto-definition))
  :init
  (progn
    (defun init-intero ()
      "Enable Intero unless visiting a cached dependency."
      (if (and buffer-file-name
               (string-match "^\\(/nix\\|.+/\\.\\(cabal\\|stack\\|stack-work\\)\\)/.+"
                             buffer-file-name))
          (progn
            (eldoc-mode -1)
            (flycheck-mode -1))
        (intero-mode)
        (setq company-dabbrev-downcase nil
              company-dabbrev-ignore-case :ignore-case
              projectile-tags-command "codex update")))

    (when (eq :intero init--haskell-backend)
      (add-hook 'haskell-mode-hook #'init-intero)))
  :config
  (progn
    (defun init-intero-insert-import ()
      "Insert a module using completing read.

The string 'import ' will be inserted as well, if missing."
      (interactive)
      (if (save-excursion (beginning-of-line) (looking-at "^import *"))
          (progn
            (end-of-line)
            (just-one-space))
        (beginning-of-line)
        (insert "import "))
      (let ((p (point)))
        (intero-get-repl-completions
         (current-buffer) "import "
         (lambda (modules)
           (save-excursion
             (goto-char p)
             (insert (completing-read "Import: " modules)))))))

    (defun init-intero-add-import ()
      "Add an import, format imports, and keep current position."
      (interactive)
      (save-excursion
        (haskell-navigate-imports)
        (open-line 1)
        (init-intero-insert-import)
        (haskell-mode-format-imports)))

    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (xref-find-definitions (xref-backend-identifier-at-point
                                  (xref-find-backend)))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
