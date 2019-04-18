;;; -*- lexical-binding: t -*-

(use-package better-shell
  :after projectile
  :bind (:map projectile-command-map
              ("x b" . better-shell-for-projectile-root)))

(use-package direnv
  :hook (after-init-hook . direnv-mode))

(use-package shell
  :ensure nil
  :bind (:map shell-mode-map
              ("SPC" . comint-magic-space)))
