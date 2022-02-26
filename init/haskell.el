;;; -*- lexical-binding: t -*-

(require 'init-hasklig)

(eval-when-compile
  (require 'cl-lib))

(defconst init-haskell-backends '("hls" "interactive-haskell" "none"))

(setq-default init-haskell-backend "hls")
(put 'init-haskell-backend 'safe-local-variable
     (lambda (s) (or (not s) (seq-contains-p init-haskell-backends s))))

(defconst init-haskell-dev-extensions '("NamedWildCards" "PartialTypeSignatures"))
(defconst init-haskell-dev-extension-flags (seq-map (lambda (s) (format "-X%s" s))
                                                    init-haskell-dev-extensions))
(defconst init-haskell-dev-rts-flags '("+RTS"
                                       "-A128m" ; Increase allocation area (generation 0) size
                                       "-qg"    ; Disable parallel GC
                                       "-RTS"))
(defconst init-haskell-repl-flags
  (let* ((opt-flags '("-Wno-missing-home-modules"
                      "-Wno-type-defaults"
                      "-fdefer-type-errors"
                      "-ferror-spans"
                      "-j")))
    (seq-concatenate 'list opt-flags init-haskell-dev-extension-flags init-haskell-dev-rts-flags)))

(defconst init-haskell-ghc-options-arg (mapconcat 'identity init-haskell-repl-flags " "))
(defconst init-haskell-ghc-options-list `("--ghc-options" ,init-haskell-ghc-options-arg))

(use-package cmm-mode)

(use-package company-cabal
  :hook (haskell-cabal-mode-hook . init-company-cabal)
  :init
  (progn
    (defun init-company-cabal ()
      (add-to-list (make-local-variable 'company-backends) #'company-cabal))))

(use-package haskell-cabal
  :ensure nil
  :after haskell-mode
  :hook (haskell-cabal-mode-hook . init-haskell-cabal)
  :bind (:map haskell-cabal-mode-map
              ("M-g M-b" . haskell-cabal-goto-benchmark-section)
              ("M-g M-e" . haskell-cabal-goto-executable-section)
              ("M-g M-l" . haskell-cabal-goto-library-section)
              ("M-g M-t" . haskell-cabal-goto-test-suite-section))
  :config
  (progn
    (dolist (kw '(("\\<\\([0-9.]+\\)\\>" (1 font-lock-constant-face))
                  ("\\<\\([><=^&|]+\\)\\>" (1 font-lock-builtin-face))))
      (add-to-list 'haskell-cabal-font-lock-keywords kw :append))

    (defun init-haskell-cabal ()
      (setq-local indent-line-function 'init-haskell-cabal-indent-line))

    ;; Copied from haskell-mode.
    (defun init-haskell-cabal-indent-line ()
      "Indent current line according to subsection"
      (interactive)
      (cl-case (haskell-cabal-classify-line)
        (section-data (init-haskell-cabal-indent-section-data))
        (empty (init-haskell-cabal-indent-section-data)))
      (haskell-cabal-forward-to-line-entry))

    (defun init-haskell-cabal-indent-section-data ()
      (let ((data-indent (haskell-cabal-section-data-start-column
                          (haskell-cabal-subsection)))
            (prev-indent (save-excursion
                           (next-line -1)
                           (back-to-indentation)
                           (current-column))))
        (indent-line-to (max data-indent prev-indent))))

    (defun init-haskell-cabal-subsection (f)
      (let ((plist (funcall f)))
        (save-excursion
          (haskell-cabal-beginning-of-subsection)
          (cond
           ((looking-at "\\([ \t]*\\(\\w*\\):\\)[ \t]*$")
            (plist-put plist :data-start-column (+ standard-indent (current-column))))
           (t plist)))))

    (advice-add 'haskell-cabal-subsection :around
                #'init-haskell-cabal-subsection)))

(use-package haskell-mode
  :diminish ((haskell-collapse-mode . "…")
             (interactive-haskell-mode . " λ")
             ;; haskell-menu : LIST SESSION BUFFERS
             )
  :mode (("\\.hs-boot\\'" . haskell-mode))
  :bind (:map haskell-mode-map
              ("C-c C-," . init-haskell-format-imports)
              ("C-c r i" . init-haskell-format-imports)
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
          haskell-interactive-mode-eval-mode nil
          haskell-process-args-cabal-new-repl init-haskell-ghc-options-list
          haskell-process-args-cabal-repl init-haskell-ghc-options-list
          haskell-process-args-ghci (cons "-fshow-loaded-modules" init-haskell-repl-flags)
          haskell-process-auto-import-loaded-modules t
          haskell-process-load-or-reload-prompt t
          haskell-process-log t
          haskell-process-wrapper-function 'init-haskell-process-wrapper)

    ;; "^\\([[:space:]]+\\|[^[:space:]]+.*\\)::[[:space:]]*\\<forall\\>"

    (defun init-haskell-font-lock-rankn-p (start)
      (or (eql ?\( (char-before start))
          (save-excursion
            (goto-char start)
            (re-search-backward "[(][[:space:]]*\\=" (line-beginning-position) t))))

    (defun init-haskell-font-lock-existential-p (start)
      (save-excursion
        (goto-char start)
        (re-search-backward "=[[:space:]]*\\=" (line-beginning-position) t)))

    (defun init-haskell-font-lock-universal-p (start)
      (save-excursion
        (goto-char start)
        (re-search-backward "::[[:space:]]*\\=" (line-beginning-position) t)))

    (setq haskell-font-lock-symbols-alist
          `(("." [?\s (Bc . Bc) ?∘] haskell-font-lock-dot-is-not-composition)
            ("forall" "∀ⁿ" ,(lambda (s) (or (init-haskell-font-lock-universal-p s)
                                       (init-haskell-font-lock-existential-p s) )))
            ("forall" "∀" ,(lambda (s) (or (init-haskell-font-lock-rankn-p s)
                                      (init-haskell-font-lock-existential-p s))))
            ("forall" "∃" ,(lambda (s) (or (init-haskell-font-lock-universal-p s)
                                      (init-haskell-font-lock-rankn-p s))))))

    (let ((s (point))) (or (init-haskell-font-lock-universal-p s)
                           (init-haskell-font-lock-existential-p s) ))

    (let ((s (point))) (or (init-haskell-font-lock-universal-p s)
                           (init-haskell-font-lock-rankn-p s)))

    ;; (haskell-font-lock-compose-symbol haskell-font-lock-symbols-alist)

    ;; (haskell-font-lock-symbols-keywords)
    ;; (("\\(\\.\\|forall\\)" (0 (haskell-font-lock-compose-symbol '(
    ;;                                                               ("." [32 (Bc . Bc) 8728] haskell-font-lock-dot-is-not-composition)
    ;;                                                               ("forall" "∀" (closure (t) (start) (not (init-haskell-font-lock-universal-p start))))
    ;;                                                               ("forall" "∃" init-haskell-font-lock-universal-p))) keep)))

    ;; (haskell-font-lock-keywords)
    ;; (haskell-font-lock-defaults-create)

    ;; (("\\(\\.\\|forall\\)"
    ;;   (0 (haskell-font-lock-compose-symbol
    ;;       '( ("." [32 (Bc . Bc) 8728] haskell-font-lock-dot-is-not-composition)
    ;;          ("forall" "∀" (closure (t) (start) (not (init-haskell-font-lock-universal-p start))))
    ;;          ("forall" "∃" init-haskell-font-lock-universal-p)
    ;;          )
    ;;       )
    ;;      keep)
    ;;   ))

    (setq init-haskell-prettify-symbols-alist
          (let ((exclude '("&&" "||")))
            (append
             (list
              (init-lig-rule-replace "\\" ?λ)
              (init-lig-rule-replace "undefined" ?⊥)
              (init-lig-rule-bracket "error" ?⊥)
              (init-lig-rule-center "&&" ?∧ ? )
              (init-lig-rule-center "||" ?∨ ? )
              (init-lig-rule-bracket "empty" ?∅)
              (init-lig-rule-bracket "elem" ?∈)
              (init-lig-rule-bracket "notElem" ?∉)
              (init-lig-rule-bracket "member" ?∈)
              (init-lig-rule-bracket "notMember" ?∉)
              (init-lig-rule-bracket "isSubsetOf" ?⊆)
              (init-lig-rule-bracket "isProperSubsetOf" ?⊂)
              (init-lig-rule-bracket "intersection" ?∩)
              (init-lig-rule-bracket "union" ?∪))
             (seq-remove (lambda (pair) (member (car pair) exclude))
                         init-hasklig-prettify-symbols-alist))))

    (defun init-haskell ()
      (haskell-collapse-mode)
      (setq prettify-symbols-alist init-haskell-prettify-symbols-alist)
      (prettify-symbols-mode)
      (cond ((and buffer-file-name
                  (string-match
                   "\\(^\\(/nix\\|.+/\\(\\(dist\\|dist-newstyle\\)\\|\\.\\(cabal\\|stack\\|stack-work\\)\\)\\)/.+\\)\\|\\(.+\\.hs-boot$\\)"
                   buffer-file-name))
             (setq-local eldoc-documentation-function #'haskell-doc-current-info)
             (flycheck-mode -1))
            (t
             (setq-local company-dabbrev-downcase nil)
             (setq-local company-dabbrev-ignore-case :ignore-case)
             (setq-local projectile-tags-command "codex update")
             (let ((columns 120))
               (setq-local fill-column columns)
               (setq-local whitespace-line-column columns))

             (add-hook 'hack-local-variables-hook #'init-haskell--after-locals nil :local))))

    (defun init-haskell--after-locals ()
      (cond ((and init-haskell-backend (not (string-equal init-haskell-backend "none")))
             (setq-local eldoc-documentation-function #'ignore)
             (funcall (intern (concat "init-" init-haskell-backend))))
            (t
             (setq-local eldoc-documentation-function #'haskell-doc-current-info)))))
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
               ("C-c r t" . init-haskell-insert-type-sig))

    (setq haskell-font-lock-keywords (delete "proc" haskell-font-lock-keywords))

    (defcustom haskell-process-types-alist
      '((cabal-new-repl "cabal" "dist-newstyle")
        (stack          "stack" ".stack-work")
        (cabal-sandbox  "cabal" "cabal.sandbox.config")
        (cabal-repl     "cabal" "dist")
        (ghc            "ghc"   nil))
      "TODO"
      :type '(alist :key-type symbol
                    :value-type (list (string :tag "Required executable")
                                      (choice (string   :tag "Filename")
                                              (function :tag "Filename predicate"))))
      :group 'haskell-interactive)

    (defun haskell-process-type ()
      "Return `haskell-process-type', or a guess if that variable is 'auto.
This function also sets the `inferior-haskell-root-dir'"
      (when (eq 'auto haskell-process-type)
        (unless (dolist (list haskell-process-types-alist)
                  (let ((exe  (nth 1 list))
                        (root (locate-dominating-file default-directory (nth 2 list))))
                    (when (and root (or (not exe) (executable-find exe)))
                      (return (setq inferior-haskell-root-dir root
                                    haskell-process-type (nth 0 list))))))
          (error "Unable to determine haskell-process-type")))

      (unless inferior-haskell-root-dir
        (let ((list (assoc haskell-process-type inferior-haskell-root-dir)))
          (unless list
            (error "Unrecognized haskell-process-type"))
          (let ((root (locate-dominating-file default-directory (nth 2 list))))
            (if root (setq inferior-haskell-root-dir root)
              (error "Unable to determine inferior-haskell-root-dir")))))
      haskell-process-type)

    (defun init-interactive-haskell ()
      (interactive-haskell-mode))

    (defun init-haskell-change-backend (backend)
      "Change Haskell backend for future buffers."
      (interactive (list (completing-read
                          "Import: "
                          (seq-concatenate 'list init-haskell-backends '(" "))
                          nil t)))
      (setq-default init-haskell-backend
                    (and backend
                         (not (string-blank-p backend))
                         backend)))

    (defun init--haskell-check-overlays-p ()
      (and haskell-process-show-overlays
           (car (haskell-check-filter-overlays
                 (overlays-in (point-min) (point-max))))))

    (defun init-haskell-format-imports ()
      "Align and sort all imports."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (haskell-navigate-imports-go)
        (haskell-align-imports)
        (haskell-sort-imports)

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

(use-package haskell-snippets)

(use-package hlint-refactor
  :if (executable-find "refactor")
  :diminish ""
  :hook (haskell-mode-hook . hlint-refactor-mode)
  :bind (:map hlint-refactor-mode-map
              ("C-c r h b" . hlint-refactor-refactor-buffer)
              ("C-c r h h" . hlint-refactor-refactor-at-point)))

(use-package lsp-haskell
  :commands (init-hls)
  :custom
  (lsp-haskell-formatting-provider "none")
  (lsp-haskell-plugin-import-lens-code-lens-on nil)
  (lsp-haskell-plugin-refine-imports-global-on nil)
  (lsp-haskell-server-args '("-j" "5" "-d" "-l" "/tmp/hls.log"))
  (lsp-haskell-server-wrapper-function
   (lambda (argv)
     `("nix-shell" ,(concat (lsp-haskell--get-root) "/shell.nix")
       "--run" ,(mapconcat 'identity argv " "))))
  :config
  (progn
    (defun init-hls ()
      (lsp))))
