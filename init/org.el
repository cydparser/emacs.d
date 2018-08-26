;;; -*- lexical-binding: t -*-

(use-package org
  :init
  (progn
    (setq org-adapt-indentation nil
          org-ellipsis "…"
          org-hide-emphasis-markers t
          org-imenu-depth 3
          org-list-description-max-indent 5
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil))
  :config
  (progn
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (shell . t))))))

(use-package org-bullets
  :hook (org-mode-hook . org-bullets-mode)
  :config
  (progn
    (setcdr org-bullets-bullet-map nil)))
