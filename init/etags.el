(use-package etags
  :bind (("M-." . init-goto-tag))
  :init
  (progn
    (setq tags-revert-without-query t))
  :config
  (progn
    (defun init-goto-tag ()
      "Jump to the definition."
      (interactive)
      (find-tag (find-tag-default)))))
