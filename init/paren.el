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
  :bind (("C-w" . init-sp-kill-region-or-backward-word)
         :map smartparens-mode-map
         ("M-N" . sp-next-sexp)
         ("M-P" . sp-previous-sexp)
         :map smartparens-strict-mode-map
         (")" . sp-up-sexp))
  :init
  (progn
    (setq sp-base-key-bindings 'paredit)
    (add-hook 'text-mode-hook #'turn-on-smartparens-mode)
    (add-hook 'org-mode-hook #'turn-on-smartparens-mode)
    (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode))
  :config
  (progn
    (require 'smartparens-config)

    (defun init-sp-kill-region-or-backward-word (arg)
      "Kill selected region or backward word."
      (interactive "p")
      (if (region-active-p)
          (sp-kill-region (mark) (point))
        (sp-backward-kill-word arg)))

    (with-eval-after-load "smartparens-haskell"
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "'" nil :actions nil)))))
