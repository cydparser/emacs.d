(use-package counsel
  :defer t
  :bind (("C-M-y" . counsel-yank-pop)
         ("C-h C-l" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package ivy
  :defer t
  :bind (("C-c z" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :init
  (progn
    (setq ivy-initial-inputs-alist nil
          ivy-re-builders-alist '((t . ivy--regex-ignore-order))
          ivy-use-virtual-buffers t)
    (add-hook 'after-init-hook (lambda () (ivy-mode 1)))))

(use-package ivy-hydra
  :defer t)

(use-package swiper
  :defer t
  :bind (("C-M-s" . swiper)))
