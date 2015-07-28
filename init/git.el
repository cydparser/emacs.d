(with-eval-after-load 'git-rebase
  (let ((map git-rebase-mode-map))
    (define-key map (kbd "q") 'with-editor-finish)))
