;;; -*- lexical-binding: t -*-

(defmacro init-patch (package broken-version &rest body)
  "Apply BODY until PACKAGE is past BROKEN-VERSION."
  (declare (indent defun))
  `(let ((desc (alist-get ,package package-alist)))
     (cond
      ((null desc)
       (message "Package not found: %s" ,package))
      ((version-list-<= ,broken-version (package-desc-version (car desc)))
       (with-eval-after-load (symbol-name ,package)
         ,@body))
      (t (message "Package %s may no longer be broken" ,package)))))
