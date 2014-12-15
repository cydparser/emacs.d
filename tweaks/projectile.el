(init-package-install 'projectile)

(setq projectile-mode-line `(:eval (format " Proj[%s]" (projectile-project-name)))
      projectile-globally-ignored-files `("/log" "/target" "/tmp" "/var")
      ;; call-process uses a different path for some reason...
      projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f %s %s"))

(projectile-global-mode)
