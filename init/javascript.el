;; npm install -g coffeelint jshint

(use-package js
  :defer t
  :init
  (progn
    (setq js-indent-level 2)))

(use-package coffee-mode
  :defer t
  :pin melpa)

(use-package nodejs-repl
  :defer t
  :bind (:map js-mode-map
              ("C-x C-e" . nodejs-repl-send-last-sexp)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)))
