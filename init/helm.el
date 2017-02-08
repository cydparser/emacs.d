(use-package helm
  :demand
  :diminish ""
  :bind (("C-M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-s o" . helm-occur)
         ("M-x" . helm-M-x)
         :map helm-map
         ([tab] . helm-execute-persistent-action))
  :init
  (progn
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-ff-newfile-prompt-p nil
          helm-locate-fuzzy-match t
          helm-recentf-fuzzy-match t)
    (require 'helm-config)
    (helm-mode)))

(use-package helm-descbinds
  :defer t
  :init
  (progn
    (add-hook 'helm-mode-hook #'helm-descbinds-mode)))
