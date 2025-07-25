;;; -*- lexical-binding: t -*-

(use-package git-gutter
  :demand
  :diminish ""
  :hook (after-init-hook . global-git-gutter-mode)
  :custom
  (git-gutter:diff-option "-w"))

(use-package git-link
  )

(use-package git-timemachine
  )

(use-package magit
  :hook (git-commit-mode-hook . flyspell-mode)
  :custom
  (magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))
  (magit-module-sections-nested nil)
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules nil :append)))

(use-package magit-todos
  :after magit
  :hook (after-init-hook . magit-todos-mode))
