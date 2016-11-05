(use-package ispell
  :defer t
  :init
  (progn
    (setq ispell-silently-savep t)
    (let ((pdict (init-xdg-config "ispell/words")))
      (if (file-exists-p pdict)
          (setq ispell-personal-dictionary pdict)))))

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
