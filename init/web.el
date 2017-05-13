;; npm install -g csslint jsonlint

(use-package css-mode
  :defer t
  :ensure nil
  :init
  (progn
    (defun init-css ()
      (add-to-list (make-local-variable 'company-backends) #'company-css))

    (add-hook 'web-mode-hook #'init-css)))

(use-package haml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package scss-mode
  :defer t
  :init
  (progn
    (defun init-scss ()
      (add-to-list (make-local-variable 'company-backends) #'company-css))

    (add-hook 'web-mode-hook #'init-scss)))


(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode))
  :init
  (progn
    (defun init-web ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml))

    (add-hook 'web-mode-hook #'init-web)))
