;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit--quit)))

(use-package multiple-cursors
  :bind (("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)
         ("<M-down-mouse-1>" . mc/toggle-cursor-on-click))
  :custom
  (mc/list-file (expand-file-name "multiple-cursors.el" init-config-directory)))

(use-package reformatter
  :commands (init-format-buffer)
  :bind (("C-c r f" . init-format-buffer)
         ("C-c C-f" . init-format-buffer))
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

    (reformatter-define haskell-format
      :program "stylish-haskell")

    (reformatter-define haskell-format
      :program "fourmolu"
      :args (list "--stdin-input-file" input-file))

    (with-eval-after-load "haskell-cabal"
      (reformatter-define haskell-cabal-format
        :program "cabal-fmt"))

    (with-eval-after-load "nix-format"
      (reformatter-define nix-format
        :program "alejandra"
        :args '("-")))

    (reformatter-define nxml-format
      :program "xmllint"
      :args '("--format" "-"))

    (reformatter-define web-format
      :program "xmllint"
      :args '("--html" "--format" "-"))
    ))
