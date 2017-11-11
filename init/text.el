;;; -*- lexical-binding: t -*-

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
  :diminish "_"
  :config
  (progn
    (defun init-flyspell-save-word ()
      "Add word at point to dictionary.
Source: http://stackoverflow.com/a/22116480/1231408"
      (interactive)
      (let ((cursor-location (point))
            (word-start-end (flyspell-get-word)))
        (if (consp word-start-end)
            (flyspell-do-correct 'save nil (nth 0 word-start-end)
                                 cursor-location (nth 1 word-start-end) (nth 2 word-start-end)
                                 cursor-location))))))

(use-package flyspell-correct-ivy
  :defer t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-." . flyspell-correct-word-generic)
              ("C-c r s" . flyspell-correct-word-generic)))

(use-package text-mode
  :defer t
  :ensure nil
  :init
  (progn
    (let ((backends '(company-capf
                      company-files
                      company-dabbrev
                      company-etags
                      company-ispell)))
      (defun init-text ()
        (flyspell-mode)
        (set (make-local-variable 'company-backends) backends)))

    (add-hook 'text-mode-hook #'init-text)))
