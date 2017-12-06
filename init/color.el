;;; -*- lexical-binding: t -*-

(defconst init-color-theme 'kaolin)

(use-package kaolin-themes
  :if (eq init-color-theme 'kaolin)
  :init (add-hook 'after-init-hook (lambda () (load-theme 'kaolin-dark 'no-confirm))))

(use-package rainbow-mode
  :diminish (rainbow-mode . "🌈"))

(use-package zenburn-theme
  :commands (init-load-theme-zenburn)
  :if (eq init-color-theme 'zenburn)
  :init (add-hook 'after-init-hook 'init-load-theme-zenburn)
  :config
  (progn
    (defun init-load-theme-zenburn ()
      "Customize zenburn after calling `load-theme'.
`load-theme' evaluates the contents of zenburn-theme.el each
invocation rather than using `require', hence this hack."
      (interactive)
      (load-theme 'zenburn 'no-confirm)
      (zenburn-with-color-variables
        (custom-theme-set-faces
         'zenburn
         `(font-lock-builtin-face ((t (:foreground ,zenburn-green+3 :weight normal))))
         `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight normal))))
         `(ivy-current-match ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1 :weight normal :underline nil))))
         `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-blue-5))))
         `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-blue-4))))
         `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-blue-3))))
         `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-blue-2))))
         `(mode-line ((,class (:foreground ,zenburn-blue :background ,zenburn-bg-1 :box (:line-width -1 :style released-button))) (t :inverse-video t)))
         `(mode-line-buffer-id ((t (:foreground ,zenburn-blue+1 :weight bold))))
         `(mode-line-inactive ((t (:foreground ,zenburn-blue-3 :background ,zenburn-bg-05 :box (:line-width -1 :style released-button)))))
         `(spaceline-modified ((t (:background ,zenburn-green+3 :foreground ,zenburn-bg-2 :inherit 'mode-line))))
         `(spaceline-read-only ((t (:background ,zenburn-orange :foreground ,zenburn-bg-2 :inherit 'mode-line))))
         `(spaceline-unmodified ((t (:background ,zenburn-cyan :foreground ,zenburn-bg-2 :inherit 'mode-line)))))))))
