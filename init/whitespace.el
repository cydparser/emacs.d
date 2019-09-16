;;; -*- lexical-binding: t -*-

(use-package whitespace
  :demand
  :diminish ((whitespace-mode . "□")
             (global-whitespace-mode . "□"))
  :hook (after-init-hook . global-whitespace-mode)
  :init
  (progn
    (setq whitespace-global-modes ()
          whitespace-line-column fill-column
          whitespace-style '(empty face lines-tail tabs trailing)))
  :config
  (progn
    (defun init-whitespace-enable-predicate ()
      (and (derived-mode-p 'prog-mode 'text-mode)
           (not noninteractive)))

    (setq whitespace-enable-predicate #'init-whitespace-enable-predicate)))
