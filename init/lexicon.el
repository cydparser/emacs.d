;;; -*- lexical-binding: t -*-

(use-package sdcv
  :bind (("C-c d" . init-define-word)
         :map sdcv-mode-map
         ("<RET>" . sdcv-search-pointer))
  :commands (init-define-word)
  :config
  (progn
    (defun init-define-word (input)
      (interactive "P")
      (if input
          (sdcv-search-input)
        (sdcv-search-pointer)))))

(use-package synosaurus
  :if (executable-find "wn")
  :diminish ""
  :bind (("C-c C-s l" . synosaurus-lookup)
         ("C-c C-s r" . synosaurus-choose-and-replace)
         ("C-c C-s i" . synosaurus-choose-and-insert))
  :init
  (progn
    (setq synosaurus-choose-method 'default
          synosaurus-prefix "C-c C-s C-s")))
