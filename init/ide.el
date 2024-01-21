;;; -*- lexical-binding: t -*-

(require 'init-utils)
(require 'multi-xref)

(use-package company
  :demand
  :diminish ""
  :bind ("M-C-/" . company-complete)
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-backends
   '(company-capf
     company-files
     (company-dabbrev-code company-etags company-keywords)
     company-dabbrev))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-idle-delay 0.5))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :demand
  :hook (company-mode-hook . company-quickhelp-mode))

(use-package dap-mode
  :ensure nil
  :init (setq dap-breakpoints-file (expand-file-name "dap-breakpoints" init-var-directory)))

(use-package eglot
  :commands (init-eglot)
  :hook ((eglot-managed-mode-hook . init-eglot)
         (nix-mode-hook . eglot-ensure)
         (toml-mode-hook . eglot-ensure)
         (yaml-ts-mode-hook . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions))
  :init
  (progn
    (setq-default eglot-workspace-configuration
                  '(:yaml
                    (:keyOrdering :json-false
                                  :format (:enable t)))))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  :config
  (progn
    (dolist (mode '(haskell-cabal-mode haskell-mode literate-haskell-mode))
      (add-to-list 'eglot-server-programs
                   `(,mode . ("haskell-language-server-wrapper" "--lsp" "-j" "4"))))

    (add-to-list 'eglot-server-programs '(nickel-mode . ("nls")))
    (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
    (add-to-list 'eglot-server-programs '(toml-mode . ("taplo" "lsp" "stdio")))
    (defun init-eglot ()
      (add-hook 'xref-backend-functions #'multi-xref-backend -99 :local)
      (flycheck-mode 0))))

(use-package flycheck
  :demand
  :diminish ""
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :hook (after-init-hook . global-flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not
     haskell-cabal-mode
     haskell-mode
     literate-haskell-mode
     nix-mode
     rustic-mode
     toml-mode
     yaml-ts-mode
     ))
  :config
  (progn
    (defun init-flycheck-may-enable-mode (f)
      "Disallow flycheck in special buffers."
      (interactive)
      (and (not (init-special-buffer-p))
           (apply (list f))))

    (advice-add 'flycheck-may-enable-mode :around
                #'init-flycheck-may-enable-mode)

    (defun init-flycheck-list-errors ()
      (interactive)
      (flycheck-list-errors)
      (pop-to-buffer flycheck-error-list-buffer))))

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :ensure nil
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-modeline-code-actions-mode))
  :bind (:map lsp-mode-map
              ("C-c b ?" . lsp-describe-session)
              ("C-." . lsp-execute-code-action))
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-suggest-server-download nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-session-file (expand-file-name "lsp-session" init-var-directory)))

(use-package lsp-ui
  :ensure nil
  :init (setq lsp-ui-doc-alignment 'window
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'bottom
              lsp-ui-doc-use-webkit nil))

(use-package move-text
  :bind (("<M-down>" . move-text-down)
         ("<M-up>"   . move-text-up)))

(use-package project
  :ensure nil
  :hook (after-init-hook . project-forget-zombie-projects)
  :custom (project-list-file (expand-file-name "projects" init-var-directory)))

(use-package projectile
  :demand
  :diminish ""
  :hook (after-init-hook . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p !" . projectile-run-shell-command-in-root)
              ("C-c p &" . projectile-run-async-shell-command-in-root)
              ("C-c p 4 D" . projectile-dired-other-window)
              ("C-c p 4 a" . projectile-find-other-file-other-window)
              ("C-c p 4 b" . projectile-switch-to-buffer-other-window)
              ("C-c p 4 d" . projectile-find-dir-other-window)
              ("C-c p 4 f" . projectile-find-file-other-window)
              ("C-c p 4 g" . projectile-find-file-dwim-other-window)
              ("C-c p 4 t" . projectile-find-implementation-or-test-other-window)
              ("C-c p C" . projectile-configure-project)
              ("C-c p D" . projectile-dired)
              ("C-c p E" . projectile-edit-dir-locals)
              ("C-c p F" . projectile-find-file-in-known-projects)
              ("C-c p I" . projectile-ibuffer)
              ("C-c p P" . projectile-test-project)
              ("C-c p R" . projectile-regenerate-tags)
              ("C-c p S" . projectile-save-project-buffers)
              ("C-c p T" . projectile-find-test-file)
              ("C-c p V" . projectile-browse-dirty-projects)
              ("C-c p a" . projectile-find-other-file)
              ("C-c p b" . projectile-switch-to-buffer)
              ("C-c p c" . projectile-compile-project)
              ("C-c p d" . projectile-find-dir)
              ("C-c p e" . projectile-recentf)
              ("C-c p f" . projectile-find-file)
              ("C-c p g" . projectile-find-file-dwim)
              ("C-c p j" . projectile-find-tag)
              ("C-c p k" . projectile-kill-buffers)
              ("C-c p l" . projectile-find-file-in-directory)
              ("C-c p o" . projectile-multi-occur)
              ("C-c p p" . projectile-switch-project)
              ("C-c p q" . projectile-switch-open-project)
              ("C-c p r" . projectile-replace)
              ("C-c p s g" . projectile-grep)
              ("C-c p s r" . projectile-ripgrep)
              ("C-c p s s" . projectile-ag)
              ("C-c p t" . projectile-toggle-between-implementation-and-test)
              ("C-c p u" . projectile-run-project)
              ("C-c p v" . projectile-vc)
              ("C-c p x e" . projectile-run-eshell)
              ("C-c p x g" . projectile-run-gdb)
              ("C-c p x i" . projectile-run-ielm)
              ("C-c p x s" . projectile-run-shell)
              ("C-c p x t" . projectile-run-term)
              ("C-c p x v" . projectile-run-vterm)
              ("C-c p z i" . projectile-invalidate-cache)
              ("C-c p z z" . projectile-cache-current-file))
  :custom
  (projectile-cache-file (expand-file-name "projectile.cache" init-var-directory))
  (projectile-create-missing-test-files t)
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" init-var-directory))
  :init
  (progn
    (defun init-projectile-ignored-project-p (project-root)
      (string-prefix-p "/nix/store/" project-root))

    (defun init-projectile-test-suffix (project-type)
      (cond
       ((string-prefix-p "haskell-" (symbol-name project-type))
        (let ((root (projectile-project-root)))
          (cond
           ((projectile-file-exists-p (expand-file-name "test/Spec.hs" root)) "Spec")
           (t "Test"))))
       (t (projectile-test-suffix project-type))))

    (setq projectile-completion-system 'ivy
          projectile-create-missing-test-files t
          projectile-ignored-project-function #'init-projectile-ignored-project-p
          projectile-ignored-projects '("~/src/emacs.d/elpa/")
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-test-suffix-function #'init-projectile-test-suffix
          projectile-use-git-grep t)
    (setq projectile-other-file-alist
          '(;; From projectile's default list
            ("c"    . ("h"))
            ("h"    . ("c"))
            (nil    . ("lock" "gpg"))
            ("lock" . (""))
            ("gpg"  . (""))
            ;; GWT
            ("java"   . ("css" "gss" "ui.xml"))
            ("css"    . ("java"))
            ("gss"    . ("java"))
            ("ui.xml" . ("java")))))
  :config
  (progn
    (setq projectile-project-types
          (seq-filter (lambda (type) (member (car type) '(racket
                                                     rust-cargo
                                                     r
                                                     emacs-cask
                                                     maven
                                                     cmake
                                                     make
                                                     nix
                                                     )))
                      projectile-project-types))

    (defun projectile-cabal-project-p () t)

    (projectile-register-project-type 'haskell-cabal-v1 '("dist/cabal-config-flags")
                                      :compile "cabal v1-build"
                                      :test "cabal v1-test"
                                      :test-suffix "Spec")

    (projectile-register-project-type 'haskell-cabal '("dist-newstyle")
                                      :compile "cabal build"
                                      :test "cabal test"
                                      :test-suffix "Spec")))

(use-package xref
  :ensure nil
  :hook (xref-after-update-hook . outline-minor-mode)
  :custom
  (xref-auto-jump-to-first-definition t)
  (xref-search-program 'ripgrep)
  )
