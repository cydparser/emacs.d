;;; -*- lexical-binding: t -*-

(use-package sdcv
  :if (executable-find "sdcv")
  :bind (("C-c d" . init-define-word)
         :map sdcv-mode-map
         ("<RET>" . sdcv-search-pointer))
  :commands (init-define-word)
  :init
  (progn
    (defvar sdcv-mode-font-lock-keywords
      '(
        ;; Dictionary name
        ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
        ;; Search word
        ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
        ;; Serial number
        ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
        ;; Type name
        ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
        ;; Phonetic symbol
        ("^[^[:space:]]+[[:space:]]+[\\]\\([^[\\]*\\)[\\]" . (1 font-lock-string-face)) ; new
        ("^\\/\\([^>]*\\)\\/$" . (1 font-lock-string-face))
        ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
        )))
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
