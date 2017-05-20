;;; -*- lexical-binding: t -*-

(use-package command-log-mode
  :defer t
  :diminish (command-log-mode . "log")
  :init
  (progn
    (setq
     command-log-mode-is-global t
     command-log-mode-key-binding-open-log "C-c l")))
