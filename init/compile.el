(use-package compile
  :defer t
  :init
  (progn
    (defun init-compile-colorize ()
      "Colorize compilation.
http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html"
      (let ((inhibit-read-only t)
            (end (point)))
        (save-excursion
          (goto-char compilation-filter-start)
          (move-beginning-of-line nil)
          (ansi-color-apply-on-region (point) end))))

    (add-hook 'compilation-filter-hook #'init-compile-colorize)))
