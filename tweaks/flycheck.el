;; c/c++: cppcheck
;; coffeescript: coffeelint (npm)
;; css: csslint (npm)
;; javascript: jshint (npm)
;; json: jsonlint (npm)
;; python: pylint (pip)
;; ruby: rubocop ruby-lint (gem)

(init-package-install 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
