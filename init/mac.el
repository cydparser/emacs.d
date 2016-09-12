(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (progn
    (exec-path-from-shell-initialize)))

