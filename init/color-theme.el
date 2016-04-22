(let ((themes (init-expand-file-name "color-themes")))
  (if (file-exists-p themes)
      (dolist (theme (directory-files themes :full "^[^.]"))
        (add-to-list 'custom-theme-load-path theme))))

(load-theme 'sanityinc-solarized-light 'no-confirm)
