(setq browse-url-browser-function 'eww-browse-url)

(defun tweak-eww-mode ()
  (init-whitespace-disable))

(add-hook 'eww-mode-hook 'tweak-eww-mode)
