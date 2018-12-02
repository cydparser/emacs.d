;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package attrap
  :after flycheck
  :bind (("C-c r f" . attrap-attrap)))

(use-package flycheck
  :demand
  :diminish ""
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :hook (after-init-hook . global-flycheck-mode)
  :init
  (progn
    (init-when-file-exists (init-xdg-config "ruby/ruby-lint.yml")
      (setq flycheck-rubylintrc))
    (init-when-file-exists (init-xdg-config "ruby/rubocop.yml")
      (setq flycheck-rubocoprc)))
  :config
  (progn
    (defun init-flycheck-may-enable-mode (f)
      "Disallow flycheck in special buffers."
      (interactive)
      (and (not (init-special-buffer-p))
           (apply (list f))))

    (advice-add 'flycheck-may-enable-mode :around
                #'init-flycheck-may-enable-mode)))

(use-package projectile
  :demand
  :diminish ""
  :hook (after-init-hook . projectile-mode)
  :init
  (progn
    (defun init-projectile-ignored-project-p (project-root)
      (string-prefix-p "/nix/store/" project-root))

    (setq projectile-completion-system 'ivy
          projectile-create-missing-test-files t
          projectile-ignored-project-function #'init-projectile-ignored-project-p
          projectile-ignored-projects '("~/src/emacs.d/elpa/")
          projectile-keymap-prefix (kbd "C-c p")
          projectile-mode-line nil
          ;; `call-process` uses a different path.
          projectile-tags-command (concat "PATH=" (getenv "PATH") " ctags -Re -f \"%s\" %s")
          projectile-use-git-grep t)))
