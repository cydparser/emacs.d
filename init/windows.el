;;; -*- lexical-binding: t -*-

(defconst init-os-win (memq window-system '(pc w32))
  "True when running on Windows.")

(when init-os-win
  (setq explicit-bash-args '("--noediting" "--login" "-i")
        explicit-shell-file-name "C:/msys64/usr/bin/bash.exe"
        shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(use-package powershell
  :if init-os-win)
