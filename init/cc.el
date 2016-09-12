(use-package cc-mode
  :defer t
  :init
  (progn
    (defun init-java ()
      (setq c-basic-offset 4
            tab-stop-list (number-sequence 4 120 4)
            tab-width 4))

    (add-hook 'java-mode-hook #'init-java)))
