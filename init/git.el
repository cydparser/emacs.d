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
              magit-revert-buffers t)
  :custom
  (magit-module-sections-nested nil)
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules nil :append)))
