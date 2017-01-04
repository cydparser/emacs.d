(defconst init-os-win (and (memq window-system '(pc w32)) t)
  "True when running on Windows.")

(use-package powershell
  :defer t
  :if init-os-win)
