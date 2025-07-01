;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package markdown-mode)

(use-package nxml-mode
  :ensure nil
  :hook (web-mode-hook . init-nxml)
  :init
  (progn
    (defun init-nxml ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml)))
  :config
  (progn
    (unbind-key "C-c C-f" nxml-mode-map)))

(use-package tex
  :ensure nil
  :custom
  (TeX-auto-save t)
  (TeX-newline-function 'newline-and-indent)
  (TeX-parse-self t))

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git" :rev :newest)
  :hook (typst-ts-mode-hook . eglot-ensure)
  :bind (:map typst-ts-mode-map
              ("C-c c c" . typst-ts-compile)
              ("C-c c p" . typst-ts-compile-and-preview)
              ("C-c c w" . typst-ts-watch-mode))
  :custom
  (typst-ts-enable-raw-blocks-highlight t)
  :config
  (progn
    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))

      (lsp-register-client (make-lsp-client
                            :new-connection (lsp-stdio-connection '("tinymist" "lsp"))
                            :activation-fn (lsp-activate-on "typst")
                            :server-id 'tinymist)))))
