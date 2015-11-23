(setq projectile-mode-line nil
      projectile-globally-ignored-files `("/log" "/target" "/tmp" "/var")
      ;; call-process uses a different path for some reason...
      projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f %s %s")
      projectile-use-git-grep t)

(projectile-global-mode)
