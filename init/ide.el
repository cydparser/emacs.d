;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package attrap
  :after flycheck
  :bind (("C-c r f" . attrap-attrap)))

(use-package company
  :demand
  :diminish ""
  :bind ("M-C-/" . company-complete)
  :hook (after-init-hook . global-company-mode)
  :init
  (progn
    (setq company-backends '(company-capf
                             company-files
                             (company-dabbrev-code company-etags company-keywords)
                             company-dabbrev))
    (setq company-idle-delay 0.3)
    (setq-default company-dabbrev-downcase nil
                  company-dabbrev-ignore-case nil)))

(use-package company-lsp
  :hook (lsp-mode-hook . init-company-lsp)
  :init
  (progn
    (defun init-company-lsp ()
      (add-to-list (make-local-variable 'company-backends) #'company-lsp))))

(use-package company-quickhelp
  :demand
  :hook (company-mode-hook . company-quickhelp-mode))

(use-package eglot
  :config
  (progn
    (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))))

(use-package flycheck
  :demand
  :after hydra
  :diminish ""
  :bind (:map flycheck-mode-map
              ("M-n" . hydra-flycheck/flycheck-next-error)
              ("M-p" . hydra-flycheck/flycheck-previous-error))
  :hook (after-init-hook . global-flycheck-mode)
  :custom (flycheck-disabled-checkers '(haskell-ghc haskell-stack-ghc))
  :init
  (progn
    (init-when-file-exists (init-xdg-config "ruby/ruby-lint.yml")
      (setq flycheck-rubylintrc))
    (init-when-file-exists (init-xdg-config "ruby/rubocop.yml")
      (setq flycheck-rubocoprc)))
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
      (pop-to-buffer flycheck-error-list-buffer))

    (defhydra hydra-flycheck ()
      "flycheck"
      ("M-n" flycheck-next-error nil)
      ("M-p" flycheck-previous-error nil)

      ("<" flycheck-first-error "first")
      ("n" flycheck-next-error "next")
      ("p" flycheck-previous-error "previous")

      ("c" flycheck-compile "compile")
      ("v" flycheck-verify-setup "verify")

      ("d" flycheck-disable-checker "disable" :color blue)
      ("k" flycheck-clear "clear" :color blue)
      ("l" init-flycheck-list-errors "list" :color blue)
      ("w" flycheck-copy-errors-as-kill "copy" :color blue)

      ("q" nil "quit"))))

(use-package lsp-mode
  :bind (("C-c b ?" . lsp-describe-session))
  :init (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :hook (lsp-mode-hook . lsp-ui-mode)
  :init (setq lsp-ui-doc-use-webkit t))

(use-package project
  :ensure nil
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
  :bind (("M-." . init-xref-find-definitions))
  :init
  (progn
    (defun init-xref-show-definitions-function (fetcher alist)
      (let ((xrefs (funcall fetcher)))
        (cond
         ((not (cdr xrefs))
          (xref-pop-to-location (car xrefs)
                                (assoc-default 'display-action alist)))
         (t
          (xref--show-xref-buffer fetcher
                                  (cons (cons 'fetched-xrefs xrefs)
                                        alist))))
        xrefs))

    (setq xref-show-definitions-function #'init-xref-show-definitions-function)

    (defun init-xref-find-definitions ()
      (interactive)
      (let ((original-backend-functions xref-backend-functions)
            (message-log-max nil)
            (result nil))

        (unwind-protect
            (while (and (null result) (not (null xref-backend-functions)))
              (ignore-errors
                (setq result (call-interactively 'xref-find-definitions)))
              (unless result
                (setq-local xref-backend-functions
                            (let ((backend (xref-find-backend))
                                  (drop t))
                              (seq-drop-while (lambda (b) (let ((d drop))
                                                       (setq drop (eq backend b))
                                                       d))
                                              xref-backend-functions)
                              ))
                )
              result)

          (setq-local xref-backend-functions original-backend-functions))))))
