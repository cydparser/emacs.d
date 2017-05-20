;;; -*- lexical-binding: t -*-

(use-package erc
  :defer t
  :commands (init-erc-freenode)
  :init
  (progn
    (setq erc-hide-list '("JOIN" "MODE" "PART" "QUIT")
          erc-track-enable-keybindings nil)

    (let ((ca (init-xdg-config "ssl/local-ca.pem"))
          (pem (init-xdg-config "irc/local.pem")))
      (when (and (file-exists-p ca) (file-exists-p pem))
        (setq tls-program
              (list (concat "openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CAfile " ca " -cert " pem)
                    (concat "gnutls-cli --priority secure256 --x509cafile " ca " --x509certfile " pem)
                    "gnutls-cli --priority secure256 -p %p %h"))))

    (defun init-erc-mode ()
      (erc-spelling-mode 1))

    (add-hook 'erc-mode-hook #'init-erc-mode))
  :config
  (progn
    (defun init-erc-freenode()
      (interactive)
      (erc-tls :server "irc.freenode.net" :port 7000))))

(use-package erc-hl-nicks
  :defer t
  :init
  (progn
    (add-hook 'erc-mode-hook #'erc-hl-nicks-mode)))
