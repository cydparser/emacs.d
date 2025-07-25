;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package apheleia
  :bind (("C-c r f" . apheleia-format-buffer)
         ("C-c C-f" . apheleia-format-buffer))
  :config
  (progn
    (setq apheleia-formatters
          (seq-concatenate
           'list
           '((alejandra . ("alejandra" "-"))
             (cabal-fmt . ("cabal-fmt"))
             (cabal-gild . ("cabal-gild"))
             (shfmt . ("shfmt" "-filename" filepath "--indent=2" "--binary-next-line" "--case-indent"
                       "--language-dialect" (cl-case (bound-and-true-p sh-shell)
                                              (sh "posix")
                                              (t "bash"))
                       "-"))
             (stylish-haskell . ("stylish-haskell"))
             (taplo . ("taplo" "format" "--stdin-filepath" filepath "-"))
             )
           apheleia-formatters))

    (setq apheleia-mode-alist
          (seq-concatenate
           'list
           '((haskell-cabal-mode . cabal-gild)
             (toml-ts-mode . taplo)
             )
           apheleia-mode-alist))))

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit--quit)))

(use-package multiple-cursors
  :bind (("<C-S-down>" . mc/mark-next-lines)
         ("<C-S-up>" . mc/mark-previous-lines)
         ("<M-down-mouse-1>" . mc/toggle-cursor-on-click))
  :custom
  (mc/list-file (expand-file-name "multiple-cursors.el" init-config-directory)))

(use-package reformatter
  :disabled
  :commands (init-format-buffer)
  :config
  (progn
    (defun init-format-buffer ()
      (interactive)
      (let* ((region-active (region-active-p))
             (f (intern (concat
                         (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))
                         "-format-" (if region-active "region" "buffer")))))
        (if (functionp f)
            (call-interactively f)
          (message "Missing formatter %s" f))))

    (reformatter-define bash-ts-format
      :program "shfmt"
      :args '("-language-dialect" "bash"
              "--indent" "2"
              "--binary-next-line"
              "--case-indent"
              "--space-redirects"))

    (reformatter-define haskell-format
      :program "stylish-haskell")

    (reformatter-define haskell-format
      :program "fourmolu"
      :args (list "--stdin-input-file" buffer-file-name))

    (with-eval-after-load "haskell-cabal"
      (reformatter-define haskell-cabal-format
        :program "cabal-fmt"
        :args (list buffer-file-name)
        :stdin nil
        ))

    (reformatter-define nxml-format
      :program "xmllint"
      :args '("--format" "-"))

    (reformatter-define web-format
      :program "xmllint"
      :args '("--html" "--format" "-"))
    )

  (reformatter-define yaml-ts-format
    :program "yamlfmt"
    :args '("-in"))
  )
