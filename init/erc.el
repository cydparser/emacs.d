(init-package-install 'erc-hl-nicks)

(setq
 erc-hide-list '("JOIN" "MODE" "PART" "QUIT")
 erc-track-enable-keybindings nil)

(let ((ca (init-xdg-config "ssl/local-ca.pem"))
      (pem (init-xdg-config "irc/local.pem")))
  (when (and (file-exists-p ca) (file-exists-p pem))
    (setq tls-program
          (list (concat "openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CAfile " ca " -cert " pem)
                (concat "gnutls-cli --priority secure256 --x509cafile " ca " --x509certfile " pem)
                "gnutls-cli --priority secure256 -p %p %h"))))

(defun init-erc-mode ()
  (init-whitespace-disable)
  (erc-hl-nicks-mode 1)
  (erc-spelling-mode 1))

(add-hook 'erc-mode-hook 'init-erc-mode)

(defun init-erc()
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000))
