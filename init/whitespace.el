;;; -*- lexical-binding: t -*-

(use-package whitespace
  :ensure nil
  :diminish ((whitespace-mode . "□")
             (global-whitespace-mode . "□"))
  :hook (after-init-hook . global-whitespace-mode)
  :custom
  (whitespace-global-modes ())
  (whitespace-line-column fill-column)
  (whitespace-style '(empty face lines-tail missing-newline-at-eof tabs trailing))
  :config
  (progn
    (defun init-whitespace-enable-predicate ()
      (and (derived-mode-p 'prog-mode 'text-mode)
           (not noninteractive)))

    (setq whitespace-enable-predicate #'init-whitespace-enable-predicate)))

(use-package whitespace-cleanup-mode
  :demand
  :after whitespace
  :diminish ""
  :hook (after-init-hook . global-whitespace-cleanup-mode))
