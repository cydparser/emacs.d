;;; -*- lexical-binding: t -*-

(require 'xref)

(defun multi-xref-backend ()
  "multi xref backend."
  'multi)

(cl-defmethod xref-backend-definitions ((_backend (eql multi)) id)
  (multi-xref-try-until-success 'xref-backend-definitions id))

(cl-defmethod xref-backend-references ((_backend (eql multi)) identifier)
  (multi-xref-try-until-success 'xref-backend-references identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql multi)) pattern)
  (multi-xref-try-until-success 'xref-backend-apropos pattern))

(defun multi-xref-try-until-success (method arg)
  (let ((functions xref-backend-functions)
        (ignore '(multi))
        (xrefs nil))
    (while (and (null xrefs) (not (null functions)))
      (let ((f (car functions)))
        (cond
         ((functionp f)
          (let ((backend (funcall f)))
            (or (and backend (not (member backend ignore))
                     (setq xrefs (ignore-errors
                                   (funcall method backend arg))))
                (setq functions (cdr functions)
                      ignore (cons backend ignore)))))
         ((eq t f)
          (setq functions (default-value 'xref-backend-functions)))
         (t
          (lwarn '(multi-xref) :error "Encountered an invalid hook in xref-backend-functions: %s" f)
          (setq functions (cdr functions))))))
    xrefs))

(provide 'multi-xref)
