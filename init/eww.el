(setq browse-url-browser-function 'eww-browse-url)

(defun init-eww-mode ()
  (init-whitespace-disable))

(add-hook 'eww-mode-hook 'init-eww-mode)
