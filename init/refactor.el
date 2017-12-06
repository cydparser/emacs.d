(use-package iedit
  :diminish "🙾"
  :bind (("C-;" . iedit-mode)
         :map esc-map
         ("C-;" . iedit-execute-last-modification)
         :map iedit-mode-keymap
         ("C-o" . iedit-toggle-unmatched-lines-visible)
         :map isearch-mode-map
         ("C-;" . iedit-mode-from-isearch))
  :init (setq iedit-toggle-key-default nil))
