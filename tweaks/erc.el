(init-package-install 'erc-hl-nicks)
(require 'tls)

(setq
 erc-hide-list '("JOIN" "MODE" "PART" "QUIT")
 erc-track-enable-keybindings nil)

(defun tweak-erc-mode ()
  (init-whitespace-disable)
  (erc-hl-nicks-mode 1)
  (erc-spelling-mode 1))

(add-hook 'erc-mode-hook 'tweak-erc-mode)

(defun init-erc()
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000 :nick "cydparser" :password erc-password))
