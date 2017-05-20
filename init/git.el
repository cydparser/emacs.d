;;; -*- lexical-binding: t -*-

(use-package git-gutter
  :demand
  :diminish ""
  :init
  (progn
    (setq git-gutter:diff-option "-w")
    (add-hook 'after-init-hook (lambda () (global-git-gutter-mode 1)))))

(use-package git-link
  :defer t)

(use-package magit
  :defer t
  :init
  (progn
    (setq magit-push-always-verify nil
          magit-revert-buffers t)
    (add-hook 'git-commit-mode-hook #'flyspell-mode)))
