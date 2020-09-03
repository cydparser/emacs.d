;;; -*- lexical-binding: t -*-

(use-package all-the-icons)

(use-package color-theme-sanityinc-tomorrow
  :demand
  :init (add-hook 'after-init-hook (lambda () (load-theme 'sanityinc-tomorrow-night 'no-confirm))))

(use-package rainbow-mode
  :diminish (rainbow-mode . "🌈"))

(use-package unicode-fonts
  :hook (after-init-hook . unicode-fonts-setup)
  :custom
  (unicode-fonts-fallback-font-list '("Symbola" "Cascadia Code"))
  (unicode-fonts-ignore-overrides t)
  :config
  (progn
    (setq-default unicode-fonts-block-font-mapping
                  (seq-map (lambda (pair)
                             (list (car pair)
                                   (let ((fonts (cadr pair)))
                                     (or (seq-mapcat (lambda (font)
                                                       (cond ((string-match-p "^Quivira" font) (list "Cascadia Code"))
                                                             ((string-match-p "^\\(Cascadia\\|Noto \\|Symbola\\)" font) (list font))
                                                             (t nil))
                                                       )
                                                     fonts)
                                         fonts))))
                           unicode-fonts-block-font-mapping))))
