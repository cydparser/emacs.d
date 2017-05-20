;;; -*- lexical-binding: t -*-

(use-package rainbow-mode
  :defer t)

(use-package zenburn-theme
  :demand
  :init
  (progn
    (load-theme 'zenburn 'no-confirm))
  :config
  (progn
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       `(font-lock-builtin-face ((t (:foreground ,zenburn-green+3 :weight normal))))
       `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight normal))))
       `(ivy-current-match ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1 :weight normal :underline nil))))
       `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-blue-5))))
       `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-blue-4))))
       `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-blue-3))))
       `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-blue-2))))))))
