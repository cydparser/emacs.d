;;; -*- lexical-binding: t -*-

(use-package counsel
  :after projectile
  :bind (("C-M-y" . counsel-yank-pop)
         ("C-h C-l" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h a" . counsel-apropos)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("C-x r b" . counsel-bookmark)
         ("M-x" . counsel-M-x)
         ("C-M-x" . counsel-command-history)
         ("C-c p s g" . counsel-git-grep)
         ("C-c p s r" . counsel-rg))
  :config
  (progn
    (add-to-list 'counsel-rg-base-command "--hidden" :append)))

(use-package counsel-projectile
  :after projectile
  :demand
  :hook (after-init-hook . counsel-projectile-mode)
  :custom
  (counsel-projectile-remove-current-project t))

(use-package ivy
  :diminish ""
  :demand
  :bind (("C-c z" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-o" . ivy-occur)
         ("C-w" . ivy-backward-kill-word)
         :map ivy-occur-mode-map
         ("n" . init-ivy-occur-next-line)
         ("p" . init-ivy-occur-previous-line)
         ("C-o" . ivy-occur-press)
         :map ivy-occur-grep-mode-map
         ("n" . init-ivy-occur-next-line)
         ("p" . init-ivy-occur-previous-line)
         ("C-o" . ivy-occur-press))
  :hook (after-init-hook . ivy-mode)
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  :config
  (progn
    (unbind-key "S-SPC" ivy-minibuffer-map)

    (defun init-ivy-occur-next-line (&optional arg)
      "Move to next line and open item in other window."
      (interactive "p")
      (ivy-occur-next-line arg)
      (ivy-occur-press))

    (defun init-ivy-occur-previous-line (&optional arg)
      "Move to previous line and open item in other window."
      (interactive "p")
      (ivy-occur-previous-line arg)
      (ivy-occur-press))))

(use-package swiper
  :demand
  :bind (("C-M-s" . swiper)))
;; TODO add M-o option for switching to isearch
