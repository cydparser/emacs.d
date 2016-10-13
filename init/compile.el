(use-package compile
  :defer t
  :init
  (progn
    (defun init-compile-colorize ()
      "Colorize compilation.
http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html"
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))

    (add-hook 'compilation-filter-hook #'init-compile-colorize)))
