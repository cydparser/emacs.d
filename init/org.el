;;; -*- lexical-binding: t -*-

(use-package org
  :defer t
  :init
  (progn
    (setq org-hide-emphasis-markers t
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil)

    (let ((backends '(company-capf
                      company-files
                      company-dabbrev
                      company-etags
                      company-ispell)))
      (defun init-org ()
        (flyspell-mode)
        (set (make-local-variable 'company-backends) backends)))

    (add-hook 'org-mode-hook #'init-org))
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
