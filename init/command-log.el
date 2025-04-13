;;; -*- lexical-binding: t -*-

(use-package command-log-mode
  :diminish (command-log-mode . "log")
  :custom
  (command-log-mode-is-global t)
  (command-log-mode-key-binding-open-log "C-c l"))
