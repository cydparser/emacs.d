;;; -*- lexical-binding: t -*-

(use-package org
  :defer t
  :init
  (progn
    (setq org-hide-emphasis-markers t
          org-log-done 'time
          org-src-fontify-natively t
          org-src-strip-leading-and-trailing-blank-lines t
          org-startup-truncated nil))
  :config
  (progn
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (ruby . t)
         (sh . t))))))

(use-package org-bullets
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook #'org-bullets-mode)))
