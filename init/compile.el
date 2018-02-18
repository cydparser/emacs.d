;;; -*- lexical-binding: t -*-

(use-package compile
  :ensure nil
  :bind (:map compilation-mode-map
              ("n" . next-error)
              ("p" . previous-error))
  :hook (compilation-filter-hook . init-compilation-colorize)
  :init
  (progn
    (setq compilation-scroll-output 'first-error)

    (defun init-compilation-colorize ()
      "Colorize compilation."
      (let ((inhibit-read-only t))
        (goto-char compilation-filter-start)
        (move-beginning-of-line nil)
        (ansi-color-apply-on-region (point) (point-max))))))
