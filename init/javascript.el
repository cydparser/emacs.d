;; npm install -g coffeelint jshint

(use-package js
  :defer t
  :init
  (progn
    (setq js-indent-level 2)))

(use-package coffee-mode
  :defer t
  :pin melpa)
