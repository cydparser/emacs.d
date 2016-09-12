(use-package company
  :demand
  :diminish ""
  :init
  (progn
    (setq
     company-idle-delay 0.3
     company-minimum-prefix-length 2)
    (global-company-mode)))

(use-package company-quickhelp
  :demand
  :init
  (progn
    (add-hook 'company-mode-hook 'company-quickhelp-mode)))
