;;; -*- lexical-binding: t -*-

;; gem install rubocop ruby-lint

(use-package inf-ruby
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :defer t
  :init
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-use-smie nil)
    (init-when-file-exists (init-xdg-config "ruby/ruby-lint.yml") (setq flycheck-rubylintrc))
    (init-when-file-exists (init-xdg-config "ruby/rubocop.yml") (setq flycheck-rubocoprc)))
  :config
  (progn
    (defun init-rspec-file-p ()
      "Indicates whether the current buffer is an rspec file."
      (let ((bname buffer-file-name))
        (and bname (rspec-spec-file-p bname))))))

(use-package ruby-block
  :defer t
  :diminish ""
  :commands (ruby-block-mode)
  :init
  (progn
    (setq ruby-block-highlight-toggle 'overlay)
    (add-hook 'ruby-mode-hook #'ruby-block-mode)))
