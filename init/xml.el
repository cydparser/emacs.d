(use-package nxml-mode
  :defer
  :ensure nil
  :init
  (progn
    (defun init-nxml ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml))

    (add-hook 'web-mode-hook #'init-nxml)))
