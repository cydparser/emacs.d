;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package better-shell
  :after projectile
  :bind (:map projectile-command-map
              ("x b" . better-shell-for-projectile-root)))

(use-package direnv
  :if (executable-find "direnv")
  :hook (after-init-hook . direnv-mode))

(use-package shell
  :ensure nil
  :bind (:map shell-mode-map
              ("SPC" . comint-magic-space))
  :init
  (progn
    (setq comint-input-ring-separator "\n\\(: \\([0-9]+\\):\\([0-9]+\\);\\)?")

    (let ((hist (init-xdg-data "zsh/history")))
      (when (and (file-exists-p hist)
                 (not (getenv "HISTFILE")))
        (setenv "HISTFILE" hist)
        (setenv "HISTSIZE" "4096")))))
