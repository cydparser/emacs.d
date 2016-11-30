(use-package rainbow-mode
  :defer t)

(use-package zenburn-theme
  :demand
  :init
  (progn
    (load-theme 'zenburn 'no-confirm)))
