(require 'yasnippet)
(require 'haskell-snippets)

(setq yas-snippet-dirs `(,(init-expand-file-name "snippets")
                         ,(init-expand-file-name "local-snippets")))

(dolist (dir yas-snippet-dirs)
  (make-directory dir t)
  (yas-load-directory dir))

(setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

(yas-global-mode 1)

(defun yasutils/uncapitalize (cap)
  (concat (downcase (substring cap 0 1))
          (substring cap 1)))
