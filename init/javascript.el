;;; -*- lexical-binding: t -*-

;; npm install -g coffeelint jshint

(use-package js
  :init
  (progn
    (setq js-indent-level 2)))

(use-package coffee-mode
  :pin melpa)

(use-package nodejs-repl
  :bind (:map js-mode-map
              ("C-x C-e" . nodejs-repl-send-last-sexp)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)))
