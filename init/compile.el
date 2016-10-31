(use-package compile
  :defer t
  :init
  (progn
    (setq compilation-scroll-output 'first-error)

    (defun init-compile-colorize ()
      "Colorize compilation."
      (let ((inhibit-read-only t))
        (goto-char compilation-filter-start)
        (move-beginning-of-line nil)
        (ansi-color-apply-on-region (point) (point-max))))

    (add-hook 'compilation-filter-hook #'init-compile-colorize)))
