(setq ispell-silently-savep t)

(let ((pdict (init-xdg-config "ispell/words")))
  (if (file-exists-p pdict)
      (setq ispell-personal-dictionary pdict)))

;; http://stackoverflow.com/a/22116480/1231408
(defun init-flyspell-save-word ()
  "Add word at point to dictionary."
  (interactive)
  (let ((cursor-location (point))
        (word (flyspell-get-word)))
    (if (consp word)
        (flyspell-do-correct 'save nil (car word) cursor-location (cadr word) (caddr word) cursor-location))))

(with-eval-after-load 'flyspell
  (let ((map flyspell-mode-map))
    (define-key map flyspell-auto-correct-binding nil)
    (define-key map (kbd "C-M-.") 'init-flyspell-save-word))
