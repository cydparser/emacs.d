;; c/c++: cppcheck
;; coffeescript: coffeelint (npm)
;; css: csslint (npm)
;; javascript: jshint (npm)
;; json: jsonlint (npm)
;; python: pylint (pip)
;; ruby: rubocop ruby-lint (gem)

(init-package-install 'flycheck)

(let ((f (init-xdg-config "ruby/ruby-lint.yml")))
  (when (file-exists-p f)
    (setq flycheck-rubylintrc f)))

(let ((f (init-xdg-config "ruby/rubocop.yml")))
  (when (file-exists-p f)
    (setq flycheck-rubocoprc f)))

(add-hook 'after-init-hook #'global-flycheck-mode)
