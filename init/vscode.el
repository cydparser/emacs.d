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

(defun init-copy-line (&optional up)
  (interactive)
  (let ((p (point))
        (orig-goal-column goal-column)
        (line (string-trim-right (thing-at-point 'line t) "[\n\r]+")))
    (cond
     ((and (eq p (buffer-end 1)) (eq p (point-at-bol)))
      (if up (open-line 1) (newline 1))
      )
     (up
      (beginning-of-line)
      (open-line 1)
      (insert line)
      (goto-char p))
     (t
      (setq goal-column (current-column))
      (end-of-line)
      (newline 1)
      (insert line)
      (goto-char p)
      (next-line 1)
      (setq goal-column orig-goal-column)))))

(defun init-copy-line-up ()
  (interactive)
  (init-copy-line :up))

(bind-key "<C-M-S-down>" #'init-copy-line)
(bind-key "<C-M-S-up>"   #'init-copy-line-up)
