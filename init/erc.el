;;; -*- lexical-binding: t -*-

(use-package erc
  :defer t
  :ensure nil
  :commands (init-erc-tls)
  :init
  (progn
    (setq erc-hide-list '("JOIN" "MODE" "PART" "QUIT")
          erc-track-enable-keybindings nil)

    (let ((ca (init-xdg-config "ssl/local-ca.pem"))
          (cert (init-xdg-config "irc/local.pem")))
      (when (and (file-exists-p ca) (file-exists-p cert))
        ;; Disable the built-in gnutls: it does not use tls-program.
        (defun gnutls-available-p () nil)

        (setq tls-program (list (concat "openssl s_client -connect %h:%p"
                                        " -no_ssl2 -ign_eof"
                                        " -CAfile " ca " -cert " cert)
                                "gnutls-cli --x509cafile %t -p %p %h"))))

    (defun init-erc-mode ()
      (erc-spelling-mode 1))

    (add-hook 'erc-mode-hook #'init-erc-mode))
  :config
  (progn
    (defun init-erc-tls (&optional server port)
      (interactive '("irc.freenode.net" 7000))
      (erc-tls :server server :port port))))

(use-package erc-hl-nicks
  :defer t
  :init
  (progn
    (add-hook 'erc-mode-hook #'erc-hl-nicks-mode)))
