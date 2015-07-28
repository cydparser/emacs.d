;; c/c++: cppcheck
;; coffeescript: coffeelint (npm)
;; css: csslint (npm)
;; javascript: jshint (npm)
;; json: jsonlint (npm)
;; python: pylint (pip)
;; ruby: rubocop ruby-lint (gem)

(let ((f (init-xdg-config "ruby/ruby-lint.yml")))
  (when (file-exists-p f)
    (setq flycheck-rubylintrc f)))

(let ((f (init-xdg-config "ruby/rubocop.yml")))
  (when (file-exists-p f)
    (setq flycheck-rubocoprc f)))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (let ((map flycheck-mode-map))
    (define-key map (kbd "M-n") 'flycheck-next-error)
    (define-key map (kbd "M-p") 'flycheck-previous-error)))

(add-hook 'after-init-hook #'global-flycheck-mode)
