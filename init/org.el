;; org-mode

(require 'org-bullets)
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-hide-emphasis-markers t
      org-log-done t
      org-startup-truncated nil
      org-src-fontify-natively t)

(if (file-exists-p "~/org")
    (setq org-agenda-files (directory-files "~/org" :full "^[^.]")))

(defun init-org-mode ()
  (flyspell-mode 1)
  (org-bullets-mode 1))

(add-hook 'org-mode-hook 'init-org-mode)
