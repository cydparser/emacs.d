(use-package whitespace
  :demand
  :diminish ((whitespace-mode . "")
             (global-whitespace-mode . ""))
  :init
  (progn
    (setq whitespace-global-modes '(not compilation-mode erc-mode twittering-mode)
          whitespace-line-column fill-column
          whitespace-style '(empty face lines-tail trailing))
    (global-whitespace-mode)))
