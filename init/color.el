(use-package rainbow-mode
  :defer t)

(use-package zenburn-theme
  :demand
  :init
  (progn
    (load-theme 'zenburn 'no-confirm))
  :config
  (progn
    (set-face-bold 'font-lock-keyword-face nil)
    (zenburn-with-color-variables
      (set-face-attribute 'font-lock-builtin-face nil :foreground zenburn-green+3 :bold nil)
      (set-face-attribute 'ivy-current-match nil :foreground zenburn-cyan :background zenburn-bg-1 :weight 'normal :underline nil)
      (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background zenburn-blue-5)
      (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background zenburn-blue-4)
      (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background zenburn-blue-3)
      (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background zenburn-blue-2))))
