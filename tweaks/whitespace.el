(require 'whitespace)

(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 100)

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(global-whitespace-mode t)
