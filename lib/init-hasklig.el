;;; -*- lexical-binding: t -*-

(defun init-lig-rule-overlay (replace lig)
  (let ((len (string-width replace))
        (rule (list ?\s '(Br . Bl))))
    (dotimes (_n (- len 2))
      (nconc rule (list ?\s '(Br . Bl))))
    (nconc rule (list ?\s '(Br . Br) lig))
    (cons replace rule)))

(defconst init-lig-padding ?˙)

(defun init-lig-rule-pad (replace lig &optional padding)
  (let ((len (string-width replace))
        (pad (or padding init-lig-padding)))
    (let ((rule (list ?\s '(Bl . Bl) lig)))
      (dotimes (_n (- len 1))
        (nconc rule (list '(Br . Bl) pad)))
      (cons replace rule))))

(defun init-lig-rule-replace (replace char)
  (cons replace (vector ?\s '(Br . Br) char)))

(defun init-lig-rule-center (replace char &optional padding)
  (let* ((len (string-width replace))
         (half (/ len 2))
         (pad (or padding init-lig-padding))
         (rule (list pad)))
    (if (< len 2)
        (init-lig-rule-replace replace char)
      (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))
      (cond ((cl-oddp len)
             (nconc rule (list '(Br . Bl) ?\s '(Br . Br) char))
             (dotimes (_ half) (nconc rule (list '(Br . Bl) pad))))
            (t
             (nconc rule (list '(Br . Bc) char '(Br . Bc) pad))
             (dotimes (_ (- half 1)) (nconc rule (list '(Br . Bl) pad)))))
      (cons replace rule))))

(defun init-lig-rule-bracket (replace char &optional padding left-bracket right-bracket)
  (let* ((len (string-width replace))
         (half (/ len 2))
         (pad (or padding ?\s))
         (lb (or left-bracket ?⸢))
         (rb (or right-bracket ?⸥))
         (rule (list pad)))
    (cond ((< len 2)
           (init-lig-rule-replace replace char))
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

(defconst init-hasklig-prettify-symbols-alist
  (when (or (member "Hasklug Nerd Font" (font-family-list)) (member "Hasklig" (font-family-list)))
    (let ((codepoint #Xe100))
      (mapcar
       (lambda (replace)
         (let ((rule (init-lig-rule-overlay replace (decode-char 'ucs codepoint))))
           (setq codepoint (1+ codepoint))
           rule))
       '("&&"  "***" "*>"  "\\\\" "||"  "|>"  "::"
         "=="  "===" "==>" "=>"   "=<<" "!!"  ">>"
         ">>=" ">>>" ">>-" ">-"   "->"  "-<"  "-<<"
         "<*"  "<*>" "<|"  "<|>"  "<$>" "<>"  "<-"
         "<<"  "<<<" "<+>" ".."   "..." "++"  "+++"
         "/="  ":::" ">=>" "->>"  "<=>" "<=<" "<->")))))

(defconst init-hasklig-prettify-symbols-common-alist
  (let ((keep '("&&" "||" "==" "===" "=>" "->" "<<" ">>"
                ">>>" ".." "..." "++" "<=>")))
    (append
     (list
      (init-lig-rule-replace "lambda" ?λ)
      (init-lig-rule-center "not" ?￢)
      (init-lig-rule-center "pi" ?π))
     (seq-filter (lambda (pair)
                   (member (car pair) keep))
                 init-hasklig-prettify-symbols-alist))))

(provide 'init-hasklig)
