;;; -*- lexical-binding: t -*-

(use-package paredit
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
  :hook (after-init-hook . show-paren-mode))

(use-package smartparens
  :diminish " $"
  :bind (("C-w" . init-sp-kill-region-or-backward-word)
         :map smartparens-mode-map
         ("M-J" . sp-join-sexp)
         ("M-N" . sp-next-sexp)
         ("M-P" . sp-previous-sexp)
         :map smartparens-strict-mode-map
         (")" . sp-up-sexp))
  :hook (((org-mode-hook text-mode-hook) . turn-on-smartparens-mode)
         (prog-mode-hook . turn-on-smartparens-strict-mode))
  :init (setq sp-base-key-bindings 'paredit)
  :config
  (progn
    (require 'smartparens-config)

    (unbind-key "<M-down>" smartparens-mode-map)
    (unbind-key "<M-up>" smartparens-mode-map)

    (defun init-sp-kill-region-or-backward-word (arg)
      "Kill selected region or backward word."
      (interactive "p")
      (if (region-active-p)
          (sp-kill-region (mark) (point))
        (sp-backward-kill-word arg)))

    (with-eval-after-load "smartparens-haskell"
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "'" nil :actions nil)))))
