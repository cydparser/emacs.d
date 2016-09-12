(use-package whitespace
  :demand
  :init
  (progn
    (defun init-whitespace-disable ()
      (interactive)
      (whitespace-mode -1)
      (setq-local indicate-empty-lines nil)
      (setq-local show-trailing-whitespace nil))

    (setq whitespace-global-modes '(not erc-mode)
          whitespace-line-column 100
          whitespace-style '(face lines-tail))
    (setq-default indicate-empty-lines t
                  show-trailing-whitespace t)
    (global-whitespace-mode)))
