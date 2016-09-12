(use-package helm
  :demand
  :diminish ""
  :bind (("C-M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-s o" . helm-occur)
         ("M-x" . helm-M-x))
  :init
  (progn
    (setq helm-ff-newfile-prompt-p nil)
    (require 'helm-config)
    (helm-mode)))

(use-package helm-descbinds
  :defer t
  :init
  (progn
    (add-hook 'helm-mode-hook #'helm-descbinds-mode)))
