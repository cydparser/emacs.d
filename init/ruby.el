(init-package-install 'projectile-rails)
(init-package-install 'ruby-block)

(add-to-list 'auto-mode-alist '("Buildfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(defconst rspec-spec-file-name-re "\\(_\\|-\\)[j]?spec\\.rb\\'"
  "The regex to identify spec files: /([_-][j]?spec.rb$/")

(defmacro init-rspec-file-p ()
  (and buffer-file-name
       (rspec-spec-file-p buffer-file-name)))

(autoload 'run-ruby "inf-ruby")
(autoload 'ruby-block-mode "ruby-block")

(defun init-ruby-mode ()
  (setq ruby-block-highlight-toggle 'overlay
        ruby-use-smie nil
        ruby-deep-indent-paren nil)
  (projectile-rails-mode)
  (ruby-block-mode t))

(add-hook 'ruby-mode-hook 'init-ruby-mode)
