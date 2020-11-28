;;; -*- lexical-binding: t -*-

(use-package git-gutter
  :demand
  :diminish ""
  :hook (after-init-hook . global-git-gutter-mode)
  :init (setq git-gutter:diff-option "-w"))

(use-package git-link
  )

(use-package magit
  :hook (git-commit-mode-hook . flyspell-mode)
  :init (setq magit-push-always-verify nil
              magit-revert-buffers t))

(use-package magit-delta
  :after magit
  :hook (magit-mode-hook . (lambda () (magit-delta-mode 1))))
