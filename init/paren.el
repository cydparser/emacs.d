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
  :ensure nil
  :init (add-hook 'after-init-hook #'show-paren-mode))

(use-package smartparens
  :defer t
  :diminish " $"
  :bind (:map smartparens-mode-map
              ("M-N" . sp-next-sexp)
              ("M-P" . sp-previous-sexp))
  :bind (:map smartparens-strict-mode-map
              (")" . sp-up-sexp))
  :init
  (progn
    (setq sp-base-key-bindings 'paredit)
    (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode))
  :config
  (progn
    (require 'smartparens-config)
    (with-eval-after-load "smartparens-haskell"
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "'" nil :actions nil)))))
