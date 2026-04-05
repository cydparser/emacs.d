;;; -*- lexical-binding: t -*-

(use-package smartparens
  :diminish "$"
  :bind (("C-w" . init-sp-kill-region-or-backward-word)
         :map smartparens-mode-map
         ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
         ("C-M-<delete>" . sp-splice-sexp-killing-forward)
         ("C-M-SPC" . sp-mark-sexp)
         ("C-S-d" . sp-splice-sexp)
         ("C-S-e" . sp-splice-sexp)
         ("C-S-j" . sp-join-sexp)
         ("C-S-k" . sp-kill-whole-line)
         ("C-S-r" . sp-splice-sexp-killing-around)
         ("C-S-s" . sp-split-sexp)
         ;; paredit bindings
         ("C-(" . sp-backward-slurp-sexp)
         ("C-)" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-n" . sp-up-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("M-(" . sp-wrap-round)
         )
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
    (require 'init-utils)
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

    (defun init-smartparens-open-newline (&rest _ignored)
      (newline 1 :interactive)
      (forward-line -1)
      (indent-according-to-mode))

    (defun init-smartparens-open-newline-semicolon (&rest _ignored)
      (newline 1 :interactive)
      (move-end-of-line 1)
      (insert-char ?\;)
      (forward-line -1)
      (indent-according-to-mode)))

  (defun init-smartparens-add-return-posthandler (modes &optional handler openers)
    (dolist (open (or openers '("{" "(" "[")))
      (sp-local-pair modes open nil
                     :post-handlers
                     `((,(or handler 'init-smartparens-open-newline) "RET")))))

  (with-eval-after-load "smartparens-haskell"
    (sp-with-modes '(haskell-mode haskell-interactive-mode)
      (sp-local-pair "'" nil :actions nil)))

  (with-eval-after-load "smartparens"
    (init-smartparens-add-return-posthandler '(nix-mode) 'init-smartparens-open-newline-semicolon))

  (with-eval-after-load "smartparens-rust"
    (let ((modes '(rust-mode rust-ts-mode rustic-mode)))
      (init-smartparens-add-return-posthandler modes)
      (sp-with-modes modes
        (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                  ("* ||\n[i]" "RET"))))))

  (with-eval-after-load 'typst-ts-mode
    (require 'smartparens-markdown)

    (defun init-paren-typst-math-or-raw-p (_id _action _context)
      (init-treesit-first-ancestor-with-type
       [math raw_blck raw_span]
       nil
       'typst))

    ;; Modified from smartparens-markdown.
    (sp-with-modes 'typst-ts-mode
      (sp-local-pair "*" "*"
                     :unless '(init-paren-typst-math-or-raw-p sp--gfm-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC"))
                     :skip-match 'sp--gfm-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(init-paren-typst-math-or-raw-p sp-point-after-word-p))
      (sp-local-pair "$" "$" :unless '(init-paren-typst-math-or-raw-p))
      (sp-local-pair "`'" "`'" :unless '(init-paren-typst-math-or-raw-p))
      (sp-local-pair "<" ">" :unless '(init-paren-typst-math-or-raw-p))
      (sp-local-pair "```" "```")
      (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                ("* ||\n[i]" "RET"))))))
