;;; -*- lexical-binding: t -*-

;; gem install rubocop ruby-lint

(use-package inf-ruby
  :if (executable-find "irb")
  :hook (ruby-mode-hook . inf-ruby-minor-mode))

(use-package ruby-mode
  :ensure nil
  :init (setq ruby-align-to-stmt-keywords t
              ruby-deep-indent-paren nil
              ruby-use-smie nil)
  :config
  (progn
    (defun init-rspec-file-p ()
      "Indicates whether the current buffer is an rspec file."
      (let ((bname buffer-file-name))
        (and bname (rspec-spec-file-p bname))))))
