;;; disable prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(global-subword-mode 1)

(defun align-comments ()
  (align-regexp region-beginning region-end REGEXP))

(setq mode-compile-always-save-buffer-p t)
