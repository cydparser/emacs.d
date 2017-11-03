;;; -*- lexical-binding: t -*-

(use-package company
  :demand
  :diminish ""
  :bind ("M-C-/" . company-complete)
  :init
  (progn
    (setq company-backends '(company-capf
                             company-files
                             (company-dabbrev-code company-etags company-keywords)
                             company-dabbrev))
    (setq company-idle-delay 0.3)
    (setq-default company-dabbrev-downcase nil
                  company-dabbrev-ignore-case nil)
    (add-hook 'after-init-hook #'global-company-mode)))

(use-package company-quickhelp
  :demand
  :init
  (progn
    (add-hook 'company-mode-hook #'company-quickhelp-mode)))
