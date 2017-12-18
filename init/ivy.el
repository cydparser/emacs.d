;;; -*- lexical-binding: t -*-

(use-package counsel
  :after projectile
  :bind (("C-M-y" . counsel-yank-pop)
         ("C-h C-l" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("C-x r b" . counsel-bookmark)
         ("M-x" . counsel-M-x)
         ("M-X" . counsel-command-history)
         :map projectile-command-map
         ("s g" . counsel-git-grep)
         ("s r" . counsel-rg)))

(use-package counsel-projectile
  :after projectile
  :hook (after-init-hook . counsel-projectile-on))

(use-package ivy
  :diminish ""
  :bind (("C-c z" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-o" . ivy-occur)
         ("C-w" . ivy-backward-kill-word)
         ("M-h" . hydra-ivy/body)
         :map ivy-occur-mode-map
         ("n" . ivy-occur-next-line)
         ("p" . ivy-occur-previous-line)
         ("C-o" . ivy-occur-press))
  :hook (after-init-hook . ivy-mode)
  :init
  (progn
    (setq ivy-initial-inputs-alist nil
          ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create
          ivy-re-builders-alist '((t . ivy--regex-ignore-order))
          ivy-use-selectable-prompt t
          ivy-use-virtual-buffers t))
  :config
  (progn
    ;; TODO, close, delete, new
    ;; (ivy-set-actions
    ;;  'ivy-switch-buffer
    ;;  '(("k" (lambda (x)
    ;;           (kill-buffer x)
    ;;           (ivy--reset-state ivy-last))
    ;;     "kill~")))
    ))

(use-package ivy-hydra
  :demand
  :after ivy
  :bind (:map hydra-ivy/keymap
              ("n" . ivy-next-line)
              ("p" . ivy-previous-line)))

(use-package swiper
  :demand
  :bind (("C-s" . swiper)))
;; TODO add M-o option for switching to isearch
