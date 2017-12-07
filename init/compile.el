;;; -*- lexical-binding: t -*-

(use-package compile
  :ensure nil
  :hook (compilation-filter-hook . init-compilation-colorize)
  :init (setq compilation-scroll-output 'first-error)
  :init
  (progn
    (defun init-compilation-colorize ()
      "Colorize compilation."
      (let ((inhibit-read-only t))
        (goto-char compilation-filter-start)
        (move-beginning-of-line nil)
        (ansi-color-apply-on-region (point) (point-max))))))
