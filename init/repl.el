;;; -*- lexical-binding: t -*-

(use-package repl-toggle
  :diminish ""
  :bind (:map repl-toggle-mode-map
              ("C-c t r" . rtog/toggle-repl))
  :hook (prog-mode-hook . rtog/activate)
  :init (setq rtog/goto-buffer-fun #'pop-to-buffer
              rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
                                     (js-mode . nodejs-repl)
                                     (ruby-mode . inf-ruby))))
