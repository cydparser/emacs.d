;;; -*- lexical-binding: t -*-

(use-package all-the-icons)

(use-package color-theme-sanityinc-tomorrow
  :demand
  :hook (after-init-hook . (lambda () (load-theme 'sanityinc-tomorrow-night 'no-confirm)))
  :custom-face
  (hl-todo ((t (:foreground "#cc9393" :weight semi-bold))))
  (logview-edit-filters-type-prefix ((t (:background "#de935f" :weight bold))))
  (logview-error-entry ((t (:background "#181818"))))
  (logview-highlight ((t (:background "#282a2e"))))
  (logview-information-entry ((t nil)))
  (logview-pulse ((t (:background "#373b41"))))
  (logview-trace-entry ((t nil)))
  (logview-warning-entry ((t (:background "#181818"))))
  (smerge-refined-added ((t (:inherit smerge-refined-change :background "#113311"))))
  (smerge-refined-removed ((t (:inherit smerge-refined-change :background "#331f21"))))
  (whitespace-empty ((t (:background "#191919"))))
  (whitespace-line ((t (:background "#282a2e" :foreground unspecified)))))

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

(use-package xterm-color
  :init
  (progn
    (setenv "TERM" "xterm-256color")

    (require 'compile)

    (defun init-xterm-color-compilation-filter (f proc string)
      (funcall f proc (xterm-color-filter string)))

    (advice-add 'compilation-filter
                :around #'init-xterm-color-compilation-filter)))
