(use-package ispell
  :defer t
  :init
  (progn
    (setq ispell-dictionary "en_US"
          ispell-program-name (executable-find "hunspell")
          ispell-silently-savep t)
    (init-when-file-exists (init-xdg-config (concat "hunspell/" ispell-dictionary))
      (setq ispell-personal-dictionary))))

(use-package flyspell
  :defer t
  :bind (:map flyspell-mode-map
              ("C-M-." . init-flyspell-save-word))
  :config
  (progn
    (defun init-flyspell-save-word ()
      "Add word at point to dictionary.
Source: http://stackoverflow.com/a/22116480/1231408"
      (interactive)
      (let ((cursor-location (point))
            (word (flyspell-get-word)))
        (if (consp word)
            (flyspell-do-correct 'save nil (car word)
                                 cursor-location (cadr word) (caddr word)
                                 cursor-location))))))
