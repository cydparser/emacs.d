;;; -*- lexical-binding: t -*-

(use-package projectile
  :demand
  :diminish ""
  :init
  (progn
    (defun init-projectile-test-suffix (project-type)
      "Find default test files suffix based on PROJECT-TYPE."
      (cond ((member project-type '(haskell-cabal haskell-stack)) "Spec")
            (t (projectile-test-suffix project-type))))

    (defun init-projectile-ignored-project-p (project-root)
      (string-prefix-p "/nix/store/" project-root))

    (setq projectile-completion-system 'ivy
          projectile-create-missing-test-files t
          projectile-ignored-project-function #'init-projectile-ignored-project-p
          projectile-ignored-projects '("~/src/emacs.d/elpa/")
          projectile-mode-line nil
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-test-suffix-function #'init-projectile-test-suffix
          projectile-use-git-grep t)
    (add-hook 'after-init-hook #'projectile-mode)))
