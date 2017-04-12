(use-package repl-toggle
  :defer t
  :diminish ""
  :bind (:map repl-toggle-mode-map
              ("C-c r" . rtog/toggle-repl))
  :init
  (progn
    (setq rtog/goto-buffer-fun #'pop-to-buffer
          rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
                                 (js-mode . nodejs-repl)
                                 (ruby-mode . inf-ruby)))
    (add-hook 'prog-mode-hook #'rtog/activate)))
