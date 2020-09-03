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

(with-eval-after-load "counsel"
  (defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
    (interactive)
    (let ((counsel-ag-base-command counsel-rg-base-command)
          (counsel--grep-tool-look-around nil)) ; Removed --pcre2
      (counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                  :caller 'counsel-rg))))

(with-eval-after-load "dante" ; init-patch 'dante '(20190629 652)
  (defun dante-schedule-next (buffer)
    "If no sub-session is running, run the next queued sub-session for BUFFER, if any.
Note that sub-sessions are not interleaved."
    (lcr-scheduler)
    (with-current-buffer buffer
      (if lcr-process-callback (force-mode-line-update t)
        (let ((req (pop dante-queue)))
          (when req (funcall req buffer)))))))
