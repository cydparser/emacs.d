;;; -*- lexical-binding: t -*-

(use-package whitespace
  :demand
  :diminish ((whitespace-mode . "□")
             (global-whitespace-mode . "□"))
  :hook (after-init-hook . global-whitespace-mode)
  :init
  (progn
    (setq whitespace-global-modes '(not
                                    compilation-mode
                                    csv-mode
                                    erc-mode
                                    magit-status-mode
                                    twittering-mode)
          whitespace-line-column fill-column
          whitespace-style '(empty face lines-tail tabs trailing))))
