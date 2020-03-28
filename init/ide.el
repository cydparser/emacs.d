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
  :diminish ""
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :hook (after-init-hook . global-flycheck-mode)
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
      ("l" flycheck-list-errors "list" :color blue)
      ("w" flycheck-copy-errors-as-kill "copy" :color blue)

      ("q" nil "quit"))))

(use-package lsp-mode
  :bind (("C-c b ?" . lsp-describe-session))
  :init (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :hook (lsp-mode-hook . lsp-ui-mode)
  :init (setq lsp-ui-doc-use-webkit t))

(use-package projectile
  :demand
  :diminish ""
  :hook (after-init-hook . projectile-mode)
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
            ("java" . ("ui.xml"))
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

    (projectile-register-project-type 'haskell-nix-cabal-v1 '("shell.nix" "dist/cabal-config-flags")
                                      :compile "nix-shell --run 'cabal v1-build'"
                                      :test "nix-shell --run 'cabal v1-test'"
                                      :test-suffix "Spec")

    (projectile-register-project-type 'haskell-nix-cabal '("shell.nix" "dist-newstyle")
                                      :compile "hal build"
                                      :test "hal test"
                                      :test-suffix "Spec")))
