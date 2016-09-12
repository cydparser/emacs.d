(use-package org
  :defer t
  :init
  (progn
    (setq org-hide-emphasis-markers t
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil)
    (add-hook 'org-mode-hook #'flyspell-mode)))

(use-package org-bullets
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook #'org-bullets-mode)))
