;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package ispell
  :ensure nil
  :init
  (progn
    (setq ispell-dictionary "en_US"
          ispell-program-name (executable-find "hunspell")
          ispell-silently-savep t)
    (init-when-file-exists (init-xdg-config (concat "hunspell/" ispell-dictionary))
      (setq ispell-personal-dictionary))))

(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map
              ("C->" . init-flyspell-save-word))
  :diminish "⍶"
  :config
  (progn
    (unbind-key "C-;" flyspell-mode-map)

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
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-." . flyspell-correct-wrapper)
              ("C-c r s" . flyspell-correct-wrapper)))

(use-package text-mode
  :ensure nil
  :hook (text-mode-hook . init-text)
  :init
  (progn
    (let ((backends '(company-capf
                      company-files
                      company-dabbrev
                      company-etags
                      company-ispell))
          (not-flyspell-modes
           '(haskell-cabal-mode
             yaml-ts-mode)))
      (defun init-text ()
        (unless (memq major-mode not-flyspell-modes)
          (flyspell-mode))
        (set (make-local-variable 'company-backends) backends)))))

(use-package pdf-tools
  :ensure nil
  :hook (after-init-hook . pdf-tools-install))

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git" :rev :newest)
  :hook (typst-ts-mode-hook . lsp-deferred)
  :config
  (progn
    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))

      (lsp-register-client (make-lsp-client
                            :new-connection (lsp-stdio-connection '("tinymist" "lsp"))
                            :activation-fn (lsp-activate-on "typst")
                            :server-id 'tinymist)))))
