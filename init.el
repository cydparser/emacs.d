;;; packages

(require 'package)

(dolist (a '(("gnu" . "http://elpa.gnu.org/packages/")
             ("ELPA" . "http://tromey.com/elpa/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives a t))

(setq package-enable-at-startup nil)
(package-initialize)

;;; utilities

(defmacro init-expand-file-name (relative-path)
  (expand-file-name relative-path user-emacs-directory))

(defun init-tab-width (x)
  (setq tab-width x
        tab-stop-list (number-sequence x 25 x)))

(defun init-package-install (pname)
  (unless (package-installed-p pname)
    (init-package-refresh)
    (package-install pname)))

(defvar init-package-fresh nil)

(defun init-package-refresh ()
  (when (null init-package-fresh)
    (package-refresh-contents)
    (setq init-package-fresh t)))

(defun init-package-require (pname)
  (init-package-install pname)
  (require pname))

(setq user-cache-directory (init-expand-file-name "cache"))
(setq user-tweaks-directory (init-expand-file-name "tweaks"))
(setq user-packages-directory (init-expand-file-name "packages"))

;;; emacs files

(let ((default-directory user-packages-directory))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; store autosaves and backups in emacs.d/cache
(let ((adir (expand-file-name "autosaves/" user-cache-directory))
      (bdir (expand-file-name "backups/" user-cache-directory)))
  (make-directory adir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat adir "\\1") t)))
  (make-directory bdir t)
  (setq backup-directory-alist `((".*" . ,bdir))))

;; store generated custom settings in separate file
(setq custom-file (init-expand-file-name "custom.el"))
(load custom-file)

;;; tweaks

(dolist (file (directory-files (init-expand-file-name "tweaks") :full "\\.el$"))
  (load file))

;; summon daemon
(server-start)

;;; enable disabled features

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
