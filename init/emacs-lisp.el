;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package aggressive-indent
  :diminish "⃕")

(use-package flycheck-elsa
  :after flycheck
  :hook (emacs-lisp-mode-hook . init-flycheck-elsa)
  :init
  (progn
    (require 'project)

    (defun init-flycheck-elsa ()
      (let ((pr (project-current)))
        (when (and pr (file-exists-p (expand-file-name ".cask" (project-root pr))))
          (flycheck-elsa-setup))))))

(use-package font-lock-studio)

(use-package lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode-hook . init-emacs-lisp-mode)
  :init
  (progn
    (defun init-emacs-lisp-mode ()
      (setq mode-name "elisp")
      (add-to-list (make-local-variable 'company-backends) #'company-elisp)
      (unless (init-special-buffer-p)
        (aggressive-indent-mode)
        (rainbow-delimiters-mode)))))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package pcre2el
  :hook (emacs-lisp-mode-hook . rxt-mode))

(use-package rainbow-delimiters)
