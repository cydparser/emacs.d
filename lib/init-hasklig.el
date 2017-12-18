;;; -*- lexical-binding: t -*-

(let ((hasklig-present (member "Hasklig" (font-family-list))))
  (defconst init-hasklig-prettify-symbols-alist
    (when hasklig-present
      (let ((codepoint #Xe100))
        (mapcar (lambda (lig)
                  (let ((pair (cons lig (string ?\t codepoint))))
                    (setq codepoint (1+ codepoint))
                    pair))
                '("&&"  "***" "*>"  "\\\\" "||"  "|>"  "::"
                  "=="  "===" "==>" "=>"   "=<<" "!!"  ">>"
                  ">>=" ">>>" ">>-" ">-"   "->"  "-<"  "-<<"
                  "<*"  "<*>" "<|"  "<|>"  "<$>" "<>"  "<-"
                  "<<"  "<<<" "<+>" ".."   "..." "++"  "+++"
                  "/="  ":::" ">=>" "->>"  "<=>" "<=<" "<->")))))

  (defconst init-hasklig-prettify-symbols-common-alist
    (when hasklig-present
      (let ((keep '("&&" "||" "==" "===" "=>" "->" "<<" ">>" ">>>"
                    ".." "..." "++" "<=>")))
        (append
         '(("!=" . "	")
           ("lambda" . "λ")
           ("not" . "¬"))
         (seq-filter (lambda (pair)
                       (member (car pair) keep))
                     init-hasklig-prettify-symbols-alist))))))

(provide 'init-hasklig)
