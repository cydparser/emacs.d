(defalias 'ms 'magit-status)

(setq magit-push-always-verify nil
      magit-revert-buffers t)

(with-eval-after-load 'git-rebase
  (let ((map git-rebase-mode-map))
    (define-key map (kbd "q") 'with-editor-finish)))
