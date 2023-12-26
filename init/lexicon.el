;;; -*- lexical-binding: t -*-

(use-package sdcv
  :if (executable-find "sdcv")
  :after popup
  :bind (("C-c d" . sdcv-search-pointer)
         ("C-c i d" . sdcv-search-pointer+)
         :map sdcv-mode-map
         ("<RET>" . sdcv-search-pointer))
  :commands (init-define-word)
  :init
  (progn
    (setq
     sdcv-mode-font-lock-keywords
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
       ("^[^[:space:]]+[[:space:]]+[\\]\\([^\\]+\\)[\\]" . (1 font-lock-string-face)) ; new
       ("^\\/\\([^>]*\\)\\/$" . (1 font-lock-string-face))
       ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
       )))
  :config
  (progn
    (define-derived-mode sdcv-mode nil "sdcv"
      (setq font-lock-defaults '(sdcv-mode-font-lock-keywords :keywords-only)) ; modified
      (setq buffer-read-only t)
      (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

    ))

(use-package synosaurus
  :if (executable-find "wn")
  :diminish ""
  :hook ((text-mode-hook . synosaurus-mode)
         (prog-mode-hook . synosaurus-mode))
  :custom
  (synosaurus-choose-method 'default)
  (synosaurus-prefix (kbd "C-c l s")))
