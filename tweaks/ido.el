(init-package-require 'flx-ido)

(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-use-faces nil
      ido-everywhere t
      ido-ignore-extensions t
      ido-save-directory-list-file (expand-file-name "ido.last" user-var-directory))

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
