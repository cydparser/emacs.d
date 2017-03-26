(use-package widget-edit
  :defer t
  :ensure nil
  :bind (:map widget-keymap
              ("n" . next-line)
              ("p" . previous-line)
              ("q" . init-kill-buffer-current)))
