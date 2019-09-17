;;; -*- lexical-binding: t -*-

(use-package color-theme-sanityinc-tomorrow
  :demand
  :init (add-hook 'after-init-hook (lambda () (load-theme 'sanityinc-tomorrow-night 'no-confirm))))

(use-package rainbow-mode
  :diminish (rainbow-mode . "🌈"))
