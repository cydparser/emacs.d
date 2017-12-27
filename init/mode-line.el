;;; -*- lexical-binding: t -*-

(use-package fancy-battery
  :demand
  :hook (after-init-hook . fancy-battery-mode)
  :init (setq fancy-battery-show-percentage t))

(use-package spaceline
  :demand
  :after projectile
  :hook (after-init-hook . init-spaceline)
  :init
  (progn
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified
          spaceline-minor-modes-separator " ")

    (require 'spaceline-config)

    (defun init-spaceline ()
      (spaceline-compile
        "init"
        `(((buffer-modified
            projectile-root)
           :face highlight-face
           :priority 0)
          (projectile-dir :priority 9)
          (buffer-id :priority 5)
          (major-mode :priority 7)
          (minor-modes :priority 6 :when active))
        '((line-column :priority 5)
          ((flycheck-error flycheck-warning flycheck-info)
           :when active
           :priority 1)
          (process :priority 6 :when active)
          (erc-track :when active)
          (global :when active)
          input-method
          (org-clock :when active)
          ((buffer-size
            buffer-encoding-abbrev)
           :separator " | "
           :priority 8)
          (battery :priority 2 :when active)))
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-init))))))
  :config
  (progn
    (spaceline-define-segment projectile-dir
      "The relative project  subdirectory."
      (when (and buffer-file-name (projectile-project-p)
                 (file-regular-p buffer-file-name))
        (file-name-directory (file-relative-name buffer-file-name
                                                 (projectile-project-root)))))))
