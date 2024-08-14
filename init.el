;;; -*- lexical-binding: t -*-

;;; Initialize package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa" . 10))
      package-menu-use-current-if-no-marks nil
      package-native-compile (native-comp-available-p))

;;; Initialize use-package

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-hook-name-suffix nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(use-package diminish :demand)

;;; Initialize Remaining

;; Add lib/ to load path.
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

;; Add Nix-installed emacs package path.
(let ((path (expand-file-name "~/.nix-profile/share/emacs/site-lisp/")))
  (when (file-exists-p path)
    (add-to-list 'load-path path)))

(condition-case-unless-debug err
    (progn
      (load (expand-file-name "base.el" user-emacs-directory))

      ;; Load all init/*.el files.
      (dolist (file (directory-files (expand-file-name "init" user-emacs-directory)
                                     :full "\\.el$"))
        (load file)))
  (display-warning
   'init
   (format "Failed loading init config: %s" (error-message-string err))
   :error))

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
