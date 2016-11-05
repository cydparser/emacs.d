;; npm install -g csslint jsonlint

(use-package haml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package scss-mode
  :defer t)

(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)))
