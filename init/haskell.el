;;; -*- lexical-binding: t -*-

;; stack install apply-refact codex hasktags hlint

(require 'init-hasklig)

(eval-when-compile
  (require 'cl))

(defvar init-haskell-backend-function 'init-dante)
(defvar-local init-haskell-goto-definition-function nil)

(defconst init-haskell-dev-extensions '("NamedWildCards" "PartialTypeSignatures"))
(defconst init-haskell-repl-flags
  (let* ((opt-flags '("-Wno-type-defaults"
                      "-ferror-spans"
                      "-fdefer-type-errors"
                      "-j"))
         (ext-flags (seq-map (lambda (s) (format "-X%s" s)) init-haskell-dev-extensions))
         (rts '("-A128m"))
         (rts-flags `("+RTS" ,@rts "-RTS")))
    (seq-concatenate 'list opt-flags ext-flags rts-flags)))

(defconst init-haskell-ghc-options-arg (string-join init-haskell-repl-flags " "))
(defconst init-haskell-ghc-options-list `("--ghc-options" ,init-haskell-ghc-options-arg))
(defconst init-haskell-ghc-options (concat "--ghc-options='" init-haskell-ghc-options-arg "'"))

(use-package cmm-mode)

(use-package company-cabal
  :hook (haskell-cabal-mode-hook . init-haskell-cabal)
  :init
  (progn
    (defun init-haskell-cabal ()
      (add-to-list (make-local-variable 'company-backends) #'company-cabal))))

(use-package dante
  :diminish " Δ"
  :bind (:map dante-mode-map
              ("C-c b ?" . dante-diagnose)
              ("C-c b t" . init-dante-change-target)
              ("C-c c b" . dante-eval-block)
              ("C-c C-i" . dante-info)
              ("C-c d i" . dante-info)
              ("C-c C-t" . dante-type-at)
              ("C-c d t" . dante-type-at))
  :commands (init-dante)
  :init
  (progn
    (setq dante-repl-command-line-methods-alist
          `((nix-cabal . ,(lambda (root)
                            (init-dante-repl-by-files
                             root '("dist/cabal-config-flags" "shell.nix")
                             `("nix-shell" "--run" (concat "cabal repl " (or dante-target "") " "
                                                           (init-dante-build-dir ,root "nix-cabal") " "
                                                           ,init-haskell-ghc-options)))))
            (stack     . ,(lambda (root)
                            (dante-repl-by-file
                             root '(".stack-work")
                             `("stack" "repl" dante-target ,@init-haskell-ghci-options-list))))
            (cabal-new . ,(lambda (root)
                            (dante-repl-by-file
                             root '("dist-new" "cabal.project")
                             `("cabal" "new-repl" dante-target (init-dante-build-dir ,root "cabal-new") ,init-haskell-ghc-options))))
            (cabal-old . ,(lambda (root)
                            (dante-repl-by-file
                             root '("dist")
                             `("cabal" "repl" dante-target (init-dante-build-dir ,root "cabal") ,init-haskell-ghc-options))))
            (nix-ghci  . ,(lambda (root)
                            (dante-repl-by-file
                             root '("shell.nix")
                             `("nix-shell" "--run" ,(concat "ghci -isrc:test -Wall -Wno-missing-signatures " init-haskell-ghc-options)))))
            (ghci      . ,(lambda (_) `("ghci" "-Wall" ,@init-haskell-repl-flags)))))

    (defun init-dante-check-target (target)
      (string-match-p
       "^\\([[:alpha:]][-[:alnum:]]+\\)?:\\(\\(lib\\|flib\\|exe\\|test\\|bench\\):\\)?[[:alpha:]][-[:alnum:]]*$"
       target))
    (put 'dante-target 'safe-local-variable #'init-dante-check-target))
  :config
  (progn
    ;; Dante has trouble with operators.
    (fset 'dante-ident-at-point #'haskell-ident-at-point)

    (unbind-key "C-c ." dante-mode-map)
    (unbind-key "C-c ," dante-mode-map)
    (unbind-key "C-c /" dante-mode-map)
    (unbind-key "C-c \"" dante-mode-map)

    (defun init-dante ()
      (setq-local haskell-process-show-overlays nil)
      (interactive-haskell-mode)
      (setq-local init-haskell-goto-definition-function #'init-dante-goto-definition)
      (dante-mode))

    (defun init-dante-build-dir (root backend)
      (concat "--builddir="
              (make-temp-name (concat "/tmp/dante-"
                                      backend "-"
                                      (file-name-base root) "-"))))

    (defun init-dante-change-target (target)
      "Change GHCi target to TARGET and restart, if changed."
      (interactive (list (completing-read "Choose target: "
                                          (haskell-cabal-enum-targets (haskell-process-type))
                                          nil nil nil
                                          'init-dante-cabal-targets-history)))
      (unless (string-equal target dante-target)
        (setq-local dante-target target)
        (dante-restart)
        (haskell-session-change-target target)))

    (defun init-dante-goto-definition ()
      (condition-case-unless-debug nil
          (call-interactively #'xref-find-definitions)
        ((user-error . nil))))

    (defun init-dante-repl-by-files (root paths cmd)
      (when (seq-every-p (lambda (p)
                           (file-exists-p (expand-file-name p root))) paths)
        cmd))

    (flycheck-add-next-checker 'haskell-dante '(t . haskell-hlint))
    (unbind-key "C-c ." dante-mode-map)
    (unbind-key "C-c ," dante-mode-map)
    (unbind-key "C-c /" dante-mode-map)))

(use-package haskell-mode
  :diminish ((haskell-collapse-mode . " …")
             (interactive-haskell-mode . " λ")
             ;; haskell-menu : LIST SESSION BUFFERS
             )
  :mode "\\.hs-boot\\'"
  :bind (:map haskell-mode-map
              ("C-c C-," . init-haskell-format-imports)
              ("C-c r i" . init-haskell-format-imports)
              ("M-." . init-haskell-goto-definition)
              ("M-g M-i" . haskell-navigate-imports)
              ("M-g i" . haskell-navigate-imports))
  :bind (:map interactive-haskell-mode-map
              ("C-c b t" . haskell-session-change-target))
  :commands (init-haskell-change-backend init-interactive-haskell)
  :hook (haskell-mode-hook . init-haskell)
  :init
  (progn
    ;; "-fshow-loaded-modules" ; Needed for >= ghc-8.2.2
    (setq flycheck-hlint-language-extensions init-haskell-dev-extensions)
    (setq haskell-font-lock-symbols t
          haskell-font-lock-symbols-alist '(("." "∘" haskell-font-lock-dot-is-not-composition))
          haskell-process-args-cabal-new-repl init-haskell-ghc-options-list
          haskell-process-args-cabal-repl init-haskell-ghc-options-list
          haskell-process-args-ghci (cons "-fshow-loaded-modules" init-haskell-repl-flags)
          haskell-process-args-stack-ghci `("--no-build" "--no-load" ,@init-haskell-ghci-options-list)
          haskell-process-auto-import-loaded-modules t
          haskell-process-wrapper-function 'init-haskell-process-wrapper)

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
                  (string-match
                   "\\(^\\(/nix\\|.+/\\.\\(cabal\\|stack\\|stack-work\\)\\)/.+\\)\\|\\(.+\\.hs-boot$\\)"
                   buffer-file-name))
             (eldoc-mode -1)
             (flycheck-mode -1))
            (t
             (setq-local company-dabbrev-downcase nil)
             (setq-local company-dabbrev-ignore-case :ignore-case)
             (setq-local projectile-tags-command "codex update")
             (when init-haskell-backend-function
               (funcall init-haskell-backend-function))))))
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
    (unbind-key "M-." interactive-haskell-mode-map)

    (bind-keys :map interactive-haskell-mode-map
               ("C-c C-t" . haskell-mode-show-type-at)
               ("C-c r t" . init-haskell-insert-type-sig)
               ("M-n" . init-haskell-goto-next-error)
               ("M-p" . init-haskell-goto-previous-error))

    (defun haskell-process-type ()
      "Return `haskell-process-type', or a guess if that variable is 'auto.
This function also sets the `inferior-haskell-root-dir'"
      (let ((root nil)
            (type haskell-process-type))
        (if (eq 'auto type)
            (cond
             ((and (setq root (locate-dominating-file default-directory "dist"))
                   (file-exists-p (expand-file-name "dist/cabal-config-flags" root))
                   (executable-find "cabal"))
              (setq type 'cabal-repl))
             ((and (setq root (locate-dominating-file default-directory ".stack-work"))
                   (executable-find "stack"))
              (setq type 'stack-ghci))
             ((executable-find "ghc")
              (setq root default-directory
                    type 'ghci))
             (error "Could not find any installation of GHC.")))
        (setq inferior-haskell-root-dir root)
        type))

    (defun init-interactive-haskell ()
      (setq-local init-haskell-goto-definition-function #'init-interactive-haskell-goto-definition)
      (interactive-haskell-mode))

    (defun init-haskell-change-backend (backend)
      "Change Haskell backend for future buffers."
      (interactive (list (completing-read
                          "Import: "
                          '("dante" "interactive-haskell" " ")
                          nil t)))
      (setq init-haskell-backend-function
            (and backend
                 (not (string-blank-p backend))
                 (intern (concat "init-" backend)))))

    (defun init-haskell-goto-definition ()
      "Jump to the definition of the thing at point using backend or etags."
      (interactive)
      (or (and init-haskell-goto-definition-function
               (let ((marker (point-marker)))
                 (funcall init-haskell-goto-definition-function)
                 (not (equal marker (point-marker)))))
          (cl-letf (((symbol-function 'xref-find-backend) (lambda () 'etags)))
            (call-interactively #'xref-find-definitions))))

    (defun init-haskell-goto-next-error ()
      "Go to the next Haskell or flycheck error."
      (interactive)
      (if (init--haskell-check-overlays-p)
          (haskell-goto-next-error)
        (flycheck-next-error)))

    (defun init-haskell-goto-previous-error ()
      "Go to the previous Haskell or flycheck error."
      (interactive)
      (if (init--haskell-check-overlays-p)
          (haskell-goto-prev-error)
        (flycheck-previous-error)))

    (defun init--haskell-check-overlays-p ()
      (and haskell-process-show-overlays
           (car (haskell-check-filter-overlays
                 (overlays-in (point-min) (point-max))))))

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
      (haskell-interactive-switch))

    (defun init-haskell-process-wrapper (args)
      "Executes ARGS in nix-shell."
      (list "nix-shell" "--command"
            (string-join (mapcar (lambda (a) (concat "'" a "'")) args) " ")))

    (defun init-interactive-haskell-goto-definition ()
      (condition-case-unless-debug nil
          (haskell-mode-goto-loc)
        ((error . nil))))))

(use-package haskell-snippets)

(use-package hlint-refactor
  :diminish ""
  :hook (haskell-mode-hook . hlint-refactor-mode)
  :bind (:map hlint-refactor-mode-map
              ("C-c r h b" . hlint-refactor-refactor-buffer)
              ("C-c r h h" . hlint-refactor-refactor-at-point)))
