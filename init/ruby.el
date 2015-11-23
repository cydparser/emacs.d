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

(setq ruby-block-highlight-toggle 'overlay
      ruby-deep-indent-paren nil
      ruby-use-smie nil)

(defun init-ruby-mode ()
  (projectile-rails-mode)
  (ruby-block-mode t))

(add-hook 'ruby-mode-hook 'init-ruby-mode)
