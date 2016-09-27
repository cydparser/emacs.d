(use-package projectile
  :demand
  :diminish ""
  :init
  (progn
    (setq projectile-mode-line nil
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-use-git-grep t)
    (make-variable-buffer-local 'projectile-tags-command)
    (projectile-global-mode)))

(use-package helm-projectile
  :demand
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
