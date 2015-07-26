(require 'whitespace)

(setq
 whitespace-global-modes '(not erc-mode) ; disables colors
 whitespace-style '(face lines-tail)
 whitespace-line-column 100)

(setq-default
 indicate-empty-lines t
 show-trailing-whitespace t)

(global-whitespace-mode t)

(defun init-whitespace-disable ()
  (interactive)
  (whitespace-mode -1)
  (setq-local indicate-empty-lines nil)
  (setq-local show-trailing-whitespace nil))
