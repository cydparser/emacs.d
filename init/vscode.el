;;; -*- lexical-binding: t -*-

(require 'misc)

(defun init-scroll-down-line ()
  (interactive)
  (scroll-down 1))

(defun init-scroll-up-line ()
  (interactive)
  (scroll-up 1))

(bind-key "<C-down>" #'init-scroll-up-line)
(bind-key "<C-up>" #'init-scroll-down-line)

(bind-key "<M-S-down-mouse-1>" #'mouse-drag-region-rectangle)

(defun init-duplicate-line-down (&optional n)
  (interactive "p")
  (let ((duplicate-line-final-position 1))
    (duplicate-line n)))

(bind-key "<M-S-down>" #'init-duplicate-line-down)
(bind-key "<M-S-up>"   #'duplicate-line)
