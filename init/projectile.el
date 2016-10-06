(use-package projectile
  :demand
  :diminish ""
  :init
  (progn
    (defun init-projectile-test-suffix (project-type)
      "Find default test files suffix based on PROJECT-TYPE."
      (cond ((member project-type '(haskell-stack)) "Spec")
            (t (projectile-test-suffix project-type))))

    (setq projectile-mode-line nil
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-test-suffix-function #'init-projectile-test-suffix
          projectile-use-git-grep t)
    (make-variable-buffer-local 'projectile-tags-command)
    (projectile-mode)))

(use-package helm-projectile
  :demand
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
