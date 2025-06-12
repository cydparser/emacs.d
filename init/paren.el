;;; -*- lexical-binding: t -*-

(use-package smartparens
  :diminish "$"
  :bind (("C-w" . init-sp-kill-region-or-backward-word)
         :map smartparens-mode-map
         ("C-S-c" . sp-convolute-sexp)
         ("C-S-e" . sp-splice-sexp)
         ("C-S-j" . sp-join-sexp)
         ("C-S-k" . sp-kill-whole-line)
         ("C-S-r" . sp-splice-sexp-killing-around)
         ("C-S-s" . sp-split-sexp)
         (")" . sp-up-sexp))
  :hook (((org-mode-hook text-mode-hook) . turn-on-smartparens-mode)
         (prog-mode-hook . turn-on-smartparens-mode))
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-no-reindent-after-kill-modes
   '(asm-mode
     haskell-mode
     makefile-gmake-mode
     nix-mode
     org-mode
     python-mode
     sh-mode
     sql-mode
     yaml-ts-mode))
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

    (defalias 'sp--syntax-class-to-char 'syntax-class-to-char)

    (defun init-sp-kill-region-or-backward-word (arg)
      "Kill selected region or backward word."
      (interactive "p")
      (if (region-active-p)
          (sp-kill-region (mark) (point))
        (sp-backward-kill-word arg)))

    (with-eval-after-load "smartparens-haskell"
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "'" nil :actions nil)))

    (with-eval-after-load "smartparens-rust"
      (sp-with-modes '(rust-mode rust-ts-mode rustic-mode)
        (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                  ("* ||\n[i]" "RET")))))

    (with-eval-after-load 'typst-ts-mode
      (require 'smartparens-markdown)

      (defun init-paren-typst-math-or-raw-p (_id _action _context)
        (treesit-parent-until
         (treesit-node-at (point) 'typst)
         (lambda (node)
           (let ((type (treesit-node-type node)))
             (or (string-equal type "math")
                 (string-equal type "raw_blck")
                 (string-equal type "raw_span"))
             ))))

      ;; Modified from smartparens-markdown.
      (sp-with-modes 'typst-ts-mode
        (sp-local-pair "*" "*"
                       :unless '(init-paren-typst-math-or-raw-p sp--gfm-point-after-word-p sp-point-at-bol-p)
                       :post-handlers '(("[d1]" "SPC"))
                       :skip-match 'sp--gfm-skip-asterisk)
        (sp-local-pair "_" "_" :unless '(init-paren-typst-math-or-raw-p sp-point-after-word-p))
        (sp-local-pair "$" "$" :unless '(init-paren-typst-math-or-raw-p))
        (sp-local-pair "`'" "`'" :unless '(init-paren-typst-math-or-raw-p))
        (sp-local-pair "```" "```")
        (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                  ("* ||\n[i]" "RET")))))))
