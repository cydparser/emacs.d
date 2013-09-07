;; ruby-mode config

(add-to-list 'auto-mode-alist '("Buildfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")

(defun tweak-ruby-mode ()
  (when (package-installed-p 'ruby-block)
        (require 'ruby-block)
        (ruby-block-mode t)
        (setq ruby-block-highlight-toggle 'overlay))
  (subword-mode 1)
  (setq ruby-deep-indent-paren nil))

(add-hook 'ruby-mode-hook 'tweak-ruby-mode)
