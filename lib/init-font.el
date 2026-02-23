;;; -*- lexical-binding: t -*-

(defun init-font-lig-rule-overlay (replace lig)
  (let ((len (string-width replace))
        (rule (list ?\s '(Br . Bl))))
    (dotimes (_n (- len 2))
      (nconc rule (list ?\s '(Br . Bl))))
    (nconc rule (list ?\s '(Br . Br) lig))
    (cons replace rule)))

(defconst init-font-lig-padding ?˙)

(defun init-font-lig-rule-pad (replace lig &optional padding)
  (let ((len (string-width replace))
        (pad (or padding init-font-lig-padding)))
    (let ((rule (list ?\s '(Bl . Bl) lig)))
      (dotimes (_n (- len 1))
        (nconc rule (list '(Br . Bl) pad)))
      (cons replace rule))))

(defun init-font-lig-rule-replace (replace char)
  (cons replace (vector ?\s '(Br . Br) char)))

(defun init-font-lig-rule-center (replace char &optional padding)
  (let* ((len (string-width replace))
         (half (/ len 2))
         (pad (or padding init-font-lig-padding))
         (rule (list pad)))
    (if (< len 2)
        (init-font-lig-rule-replace replace char)
      (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))
      (cond ((cl-oddp len)
             (nconc rule (list '(Br . Bl) ?\s '(Br . Br) char))
             (dotimes (_ half) (nconc rule (list '(Br . Bl) pad))))
            (t
             (nconc rule (list '(Br . Bc) char '(Br . Bc) pad))
             (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))))
      (cons replace rule))))

(defun init-font-lig-rule-bracket (replace char &optional padding left-bracket right-bracket)
  (let* ((len (string-width replace))
         (half (/ len 2))
         (pad (or padding ?\s))
         (lb (or left-bracket ?⸢))
         (rb (or right-bracket ?⸥))
         (rule (list pad)))
    (cond ((< len 2)
           (init-font-lig-rule-replace replace char))
          (t
           (nconc rule (list '(Bl . Bl) lb))
           (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))
           (cond ((cl-oddp len)
                  (nconc rule (list '(Br . Bl) ?\s '(Br . Br) char))
                  (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad))))
                 (t
                  (nconc rule (list '(Br . Bc) char '(Br . Bc) pad))
                  (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))))
           (nconc rule (list '(Br . Bl) rb))
           (cons replace rule)))))

(defun init-font-for-buffer (&rest families)
  (let* ((font-family-list (font-family-list))
         (family (seq-find (lambda (f)
                           (seq-contains-p font-family-list f)) families)))
    (if family
        (buffer-face-set :family family)
      (warn "Missing fonts %s" families))))

(provide 'init-font)
