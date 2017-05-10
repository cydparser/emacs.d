(use-package projectile
  :demand
  :diminish ""
  :init
  (progn
    (defun init-projectile-test-suffix (project-type)
      "Find default test files suffix based on PROJECT-TYPE."
      (cond ((member project-type '(haskell-cabal haskell-stack)) "Spec")
            (t (projectile-test-suffix project-type))))

    (setq projectile-completion-system 'ivy
          projectile-create-missing-test-files t
          projectile-mode-line nil
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-test-suffix-function #'init-projectile-test-suffix
          projectile-use-git-grep t)
    (make-variable-buffer-local 'projectile-tags-command)
    (add-hook 'after-init-hook #'projectile-mode)))
