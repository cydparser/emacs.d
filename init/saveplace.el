(use-package saveplace
  :demand
  :init
  (progn
    (setq save-place-file (expand-file-name "places" init-var-directory))
    (save-place-mode)))
