(init-package-install 'twittering-mode)

;; requires gpg
(setq twittering-use-master-password t)

(defun init-twittering-mode ()
  (init-whitespace-disable))

(add-hook 'twittering-mode-hook 'init-twittering-mode)
