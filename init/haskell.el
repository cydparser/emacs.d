;;; -*- lexical-binding: t -*-

;; stack install apply-refact codex hasktags hlint

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
  :diminish " Δ"
  :bind (:map dante-mode-map
              ("C-c C-i" . dante-info)
              ("C-c C-r" . dante-auto-fix)
              ("C-c C-t" . dante-type-at))
  :init (defalias 'init-dante 'dante-mode)
  :config
  (progn
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    (unbind-key "C-c ." dante-mode-map)
    (unbind-key "C-c ," dante-mode-map)
    (unbind-key "C-c /" dante-mode-map)))

(use-package haskell-mode
  :defer t
  :diminish (haskell-mode . " λ")
  :diminish (literate-haskell-mode . " ƛ")
  :bind (:map haskell-mode-map
              ("C-c C-," . init-haskell-format-imports)
              ("C-c r i" . init-haskell-format-imports)
              ("M-g i" . haskell-navigate-imports)
              ("M-g M-i" . haskell-navigate-imports))
  :commands (init-haskell-change-backend)
  :init
  (progn
    (setq haskell-font-lock-symbols t
          haskell-font-lock-symbols-alist
          '(("." "∘" haskell-font-lock-dot-is-not-composition))
          haskell-process-auto-import-loaded-modules t)
    ;; TODO: Try out
    ;; * C-j multiline
    ;; * haskell-menu
    ;; * C-`
    ;; * haskell-session-change-target
    ;; * haskell-interactive-bring
    (let* ((opt-flags '("-fdefer-type-errors"
                        "-ferror-spans"
                        "-fexternal-interpreter"
                        "-prof"))
           (exts '("NamedWildCards"
                   "PartialTypeSignatures"))
           (ext-flags (seq-map (lambda (s) (format "-X%s" s)) exts))
           (ghc-opts (list "--ghc-options"
                           (format "%s %s" (string-join opt-flags " ")
                                   (string-join ext-flags " ")))))
      (setq flycheck-ghc-args opt-flags
            flycheck-ghc-language-extensions exts
            haskell-process-args-ghci (seq-concatenate 'list opt-flags ext-flags)
            haskell-process-args-cabal-repl ghc-opts
            haskell-process-args-cabal-new-repl ghc-opts
            haskell-process-args-stack-ghci `("--no-build" "--no-load" ,ghc-opts)))

    (defvar init-haskell-backend-function 'init-interactive-haskell)
    (defalias 'init-interactive-haskell 'interactive-haskell-mode)

    (defconst init-haskell-prettify-symbols-alist
      (let ((exclude '("&&" "||")))
        (append
         '(("\\" . "λ")
           ("&&" . "∧")
           ("||" . "∨")
           ("not" . "¬")
           ("empty" . "∅")
           ("forall" . "∀")
           ("pi" . "π")
           ("undefined" . "⊥")
           ("`elem`" . "∈")
           ("`notElem`" . "∉")
           ("`member`" . "∈")
           ("`notMember`" . "∉")
           ("`isSubsetOf`" . "⊆")
           ("`isProperSubsetOf`" . "⊂")
           ("`intersection`" . "∩")
           ("`union`" . "∪"))
         (seq-remove (lambda (pair) (member (car pair) exclude))
                     init-hasklig-prettify-symbols-alist))))

    (defun init-haskell ()
      (haskell-collapse-mode)
      (setq prettify-symbols-alist init-haskell-prettify-symbols-alist)
      (prettify-symbols-mode)
      (cond ((and buffer-file-name
                  (string-match "^\\(/nix\\|.+/\\.\\(cabal\\|stack\\|stack-work\\)\\)/.+"
                                buffer-file-name))
             (eldoc-mode -1)
             (flycheck-mode -1))
            (t
             (setq-local company-dabbrev-downcase nil)
             (setq-local company-dabbrev-ignore-case :ignore-case)
             (set (make-local-variable 'projectile-tags-command) "codex update")
             (when (fboundp init-haskell-backend-function)
               (funcall init-haskell-backend-function)))))

    (add-hook 'haskell-mode-hook #'init-haskell))
  :config
  (progn
    (require 'haskell)

    (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
    ;; Disable stub keybindings.
    (unbind-key "C-c C-b" haskell-mode-map)
    (unbind-key "C-c C-i" haskell-mode-map)
    (unbind-key "C-c C-l" haskell-mode-map)
    (unbind-key "C-c C-t" haskell-mode-map)
    (unbind-key "C-c C-v" haskell-mode-map)

    (bind-keys :map interactive-haskell-mode-map
               ("C-c C-t" . haskell-mode-show-type-at)
               ("C-c r t" . init-haskell-insert-type-sig)
               ("M-." . haskell-mode-goto-loc) ;; TODO: test fallback to tags
               ("M-n" . init-haskell-goto-next-error)
               ("M-p" . init-haskell-goto-previous-error))

    (defun init-haskell-change-backend (backend)
      "Change Haskell backend for future buffers."
      (interactive (list (completing-read
                          "Import: "
                          '("dante" "interactive-haskell" "intero" " ")
                          nil t)))
      (setq init-haskell-backend-function
            (and backend
                 (not (string-blank-p backend))
                 (intern (concat "init-" backend)))))

    (defun init-haskell-goto-next-error ()
      "Go to the next Haskell or flycheck error."
      (interactive)
      (if (init--haskell-check-overlays-p)
          (haskell-goto-next-error)
        (flycheck-next-error)))

    (defun init-haskell-goto-prev-error ()
      "Go to the previous Haskell or flycheck error."
      (interactive)
      (if (init--haskell-check-overlays-p)
          (haskell-goto-prev-error)
        (flycheck-previous-error)))

    (defun init--haskell-check-overlays-p ()
      (car (haskell-check-filter-overlays
            (overlays-in (point-min) (point-max)))))

    (defun init-haskell-format-imports ()
      "Align and sort all imports."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (haskell-navigate-imports)
        (haskell-mode-format-imports)

        (while (progn (haskell-navigate-imports)
                      (looking-at "^import "))
          (haskell-sort-imports))))

    (defun init-haskell-insert-type-sig ()
      "Insert the type signature of the identifier at point."
      (interactive)
      (haskell-mode-show-type-at :insert-value))

    (defun init-haskell-process-reload-switch ()
      "Reload file and switch to the REPL."
      (interactive)
      (haskell-process-load-or-reload)
      (haskell-interactive-switch))))

(use-package haskell-snippets
  :defer t)

(use-package hlint-refactor
  :defer t
  :diminish ""
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package intero
  :defer t
  :diminish " η"
  :bind (:map intero-mode-map
              ("C-c r i" . init-intero-add-import)
              ("M-." . init-intero-goto-definition))
  :init (defalias 'init-intero 'intero-mode)
  :config
  (progn
    (defun init-intero-insert-import (callback)
      "Insert a module using completing read.

The string 'import ' will be inserted as well, if missing."
      (interactive '(nil))
      (beginning-of-line)
      (if (looking-at "^import ")
          (progn
            (end-of-line)
            (just-one-space))
        (insert "import "))
      (let ((p (point)))
        (intero-get-repl-completions
         (current-buffer) "import "
         (lambda (modules)
           (save-excursion
             (goto-char p)
             (insert (completing-read "Import: " modules))
             (if callback (funcall callback)))))))

    (defun init-intero-add-import ()
      "Add an import, format imports, and keep current position."
      (interactive)
      (save-excursion
        (haskell-navigate-imports)
        (open-line 1)
        (init-intero-insert-import #'init-haskell-format-imports)))

    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (xref-find-definitions (xref-backend-identifier-at-point
                                  (xref-find-backend)))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
