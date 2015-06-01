(let ((themes (init-expand-file-name "color-themes")))
  (if (file-exists-p themes)
      (dolist (theme (directory-files themes :full "^[^.]"))
        (add-to-list 'custom-theme-load-path theme))))

(init-package-require 'color-theme)
(init-package-require 'color-theme-solarized)
(color-theme-solarized)
