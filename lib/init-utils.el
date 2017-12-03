;;; -*- lexical-binding: t -*-

;;; Utilities

(defconst init-var-directory
  (expand-file-name "var" user-emacs-directory)
  "Directory for various files created by Emacs.")

(defconst init-xdg-config-home
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config"))
  "XDG config home directory.")

(defconst init-xdg-data-home
  (or (getenv "$XDG_DATA_HOME") (expand-file-name "~/.local/share"))
  "XDG data home directory.")

(defun init-special-buffer-p ()
  "Checks if current buffer is special."
  (string-prefix-p "*" (buffer-name)))

(defun init-xdg-config (path)
  "Convert relative PATH to absolute using XDG config home for the parent directory."
  (expand-file-name path init-xdg-config-home))

(defun init-xdg-data (path)
  "Convert relative PATH to absolute using XDG data home for the parent directory."
  (expand-file-name path init-xdg-data-home))

(defmacro init-when-file-exists (path sexp)
  "Evaluates (append SEXP (PATH)) if file exists."
  (declare (indent defun))
  `(let ((file ,path))
     (when (file-exists-p file)
       (,@sexp file))))

(provide 'init-utils)
