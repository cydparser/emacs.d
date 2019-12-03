;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package erc
  :ensure nil
  :hook (erc-mode-hook . init-erc-mode)
  :init
  (progn
    (setq erc-hide-list '("JOIN" "MODE" "PART" "QUIT")
          erc-track-enable-keybindings nil
          erc-track-when-inactive t)

    (let ((ca (init-xdg-config "ssl/local-ca.pem"))
          (cert (init-xdg-config "irc/local.pem")))
      (when (and (file-exists-p ca) (file-exists-p cert))
        (setq tls-program (list (concat "openssl s_client -connect %h:%p"
                                        " -ign_eof"
                                        " -CAfile " ca " -cert " cert)
                                "gnutls-cli --x509cafile %t -p %p %h"))))
    (defun init-erc-mode ()
      (erc-spelling-mode 1)
      (erc-track-mode))

    (defun init-erc-tls (&optional server port)
      (interactive '("irc.freenode.net" 7000))
      (erc-tls :server server :port port))))

(use-package erc-hl-nicks
  :hook (erc-mode-hook . erc-hl-nicks-mode))
