;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package envrc
  :demand
  :if (executable-find "direnv")
  :hook (after-init-hook . envrc-global-mode))

(use-package shell
  :ensure nil
  :hook (shell-mode-hook . compilation-shell-minor-mode)
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
