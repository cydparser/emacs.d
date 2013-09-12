;;;; ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(require 'package)
(package-initialize)

;;;; Init Utilities
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

;; http://emacsblog.org/2007/01/29/maximize-on-startup-part-1/
(defun fix-window-size ()
  (interactive)
  (set-frame-size (selected-frame) 100 30)
  (set-frame-position (selected-frame) 0 0))

(setq user-cache-directory (init-expand-file-name "cache"))
(setq user-tweaks-directory (init-expand-file-name "tweaks"))
(setq user-packages-directory (init-expand-file-name "packages"))

(add-to-list 'exec-path "/usr/local/bin")

;; disable audible and visual bell
(setq ring-bell-function (lambda ()))
(setq echo-keystrokes 0.125)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 25 4))
(setq-default fill-column 100)

;; enable syntax highlighting
(global-font-lock-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(global-set-key (kbd "C-c C-SPC") 'delete-trailing-whitespace)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;;;; Themes
(let ((themes (init-expand-file-name "color-themes")))
  (make-directory themes t)
  (dolist (theme (directory-files themes :full "^[^.]"))
    (add-to-list 'custom-theme-load-path theme)))

(init-package-install 'color-theme)
(init-package-require 'color-theme-solarized)
(load-theme 'solarized-dark t)

(let ((default-directory (init-expand-file-name "packages/")))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; store autosaves and backups in emacs.d/cache
(let ((dir (expand-file-name "autosaves/" user-cache-directory)))
  (make-directory dir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat dir "\\1") t))))

(let ((dir (expand-file-name "backups/" user-cache-directory)))
  (make-directory dir t)
  (setq backup-directory-alist `((".*" . ,dir))))

;; keep generated custom settines in separate file
(setq custom-file (init-expand-file-name "custom.el"))
(load custom-file)

(dolist (file (directory-files (init-expand-file-name "tweaks") :full "\\.el$"))
  (load file))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
