;;; Initialize package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)

(package-initialize)

;;; Initialize use-package

(setq use-package-always-defer t
      use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;;; Initialize Remaining

(load (expand-file-name "base.el" user-emacs-directory))

;; Load all init/*.el files.
(dolist (file (directory-files (expand-file-name "init" user-emacs-directory) :full "\\.el$"))
  (load file))

;; (dolist (n '("color-theme"
;;              "command-log"
;;              "company"
;;              "dired"
;;              "docker"
;;              "emacs-lisp"
;;              "erc"
;;              "etags"
;;              "flycheck"
;;              "git"
;;              "haskell"
;;              "helm"
;;              "javascript"
;;              "mac"
;;              "org"
;;              "paren"
;;              "projectile"
;;              "ruby"
;;              "sh"
;;              "term"
;;              "web"
;;              "whitespace"
;;              "yaml"
;;              "yasnippet"))
;;   (load (expand-file-name (concat "init/" n ".el") user-emacs-directory)))

;; Summon the daemon.
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Enable Disabled Features

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
