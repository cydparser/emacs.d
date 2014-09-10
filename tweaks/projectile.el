(init-package-install 'projectile)

(setq projectile-mode-line `(:eval (format " Proj[%s]" (projectile-project-name)))
      projectile-globally-ignored-files `("/log" "/target" "/tmp" "/var"))

(projectile-global-mode)
