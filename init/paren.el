;;; -*- lexical-binding: t -*-

(use-package smartparens
  :diminish "$"
  :bind (("C-w" . init-sp-kill-region-or-backward-word)
         :map smartparens-mode-map
         ("C-S-c" . sp-convolute-sexp)
         ("C-S-i" . sp-splice-sexp)
         ("C-S-j" . sp-join-sexp)
         ("C-S-k" . sp-kill-whole-line)
         ("C-S-r" . sp-splice-sexp-killing-around)
         ("C-S-s" . sp-split-sexp)
         ("C-k" . sp-kill-hybrid-sexp)
         (")" . sp-up-sexp))
  :hook (((org-mode-hook text-mode-hook) . turn-on-smartparens-mode)
         (prog-mode-hook . turn-on-smartparens-mode))
  :init (setq sp-base-key-bindings 'paredit
              sp-no-reindent-after-kill-modes '(asm-mode
                                                haskell-mode
                                                makefile-gmake-mode
                                                nix-mode
                                                org-mode
                                                python-mode
                                                sh-mode
                                                sql-mode
                                                yaml-mode))
  :config
  (progn
    (require 'smartparens-config)
    (unbind-key "<M-down>" smartparens-mode-map)
    (unbind-key "<M-up>" smartparens-mode-map)
    (unbind-key "M-?" smartparens-mode-map)
    (unbind-key "M-S" smartparens-mode-map)
    (unbind-key "M-j" smartparens-mode-map)
    (unbind-key "M-r" smartparens-mode-map)
    (unbind-key "M-s" smartparens-mode-map)

    (defun init-sp-kill-region-or-backward-word (arg)
      "Kill selected region or backward word."
      (interactive "p")
      (if (region-active-p)
          (sp-kill-region (mark) (point))
        (sp-backward-kill-word arg)))

    (with-eval-after-load "smartparens-haskell"
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "'" nil :actions nil)))))
