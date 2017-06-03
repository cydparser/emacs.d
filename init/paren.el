;;; -*- lexical-binding: t -*-

(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("{" . paredit-open-curly)
              ("}" . paredit-close-curly))
  :init
  (progn
    (setq paredit-lighter " ()"))
  :config
  (progn
    (unbind-key ";" paredit-mode-map)
    (unbind-key "M-;" paredit-mode-map)
    (unbind-key "M-?" paredit-mode-map)
    (unbind-key "\\" paredit-mode-map)))

(use-package paren
  :defer t
  :init
  (show-paren-mode))

(use-package paxedit
  :defer t)
