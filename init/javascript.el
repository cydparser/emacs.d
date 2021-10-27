;;; -*- lexical-binding: t -*-

(use-package js
  :ensure nil
  :init
  (progn
    (setq js-indent-level 2)))

(use-package nodejs-repl
  :if (executable-find "node")
  :bind (:map js-mode-map
              ("C-x C-e" . nodejs-repl-send-last-sexp)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)))
