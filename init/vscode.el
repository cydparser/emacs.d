;;; -*- lexical-binding: t -*-

(defun init-scroll-down-line ()
  (interactive)
  (scroll-down 1))

(defun init-scroll-up-line ()
  (interactive)
  (scroll-up 1))

(bind-key "<C-down>" #'init-scroll-up-line)
(bind-key "<C-up>" #'init-scroll-down-line)

(bind-key "<M-S-down-mouse-1>" #'mouse-drag-region-rectangle)
