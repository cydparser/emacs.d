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
  (unless (functionp 'dante--async-type-at)
    (defun dante-type-at (insert)
      "Get the type of the thing or selection at point.
When the universal argument INSERT is non-nil, insert the type in the buffer."
      (interactive "P")
      (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
        (lcr-cps-let ((_load_messages (dante-async-load-current-buffer nil))
                      (ty (dante--async-type-at tap)))
                     (if ty
                         (if insert (save-excursion (goto-char (line-beginning-position))
                                                    (insert ty "\n"))
                           (message "%s" ty))
                       (message "Unable to obtain the type")))))

    (lcr-def dante--async-type-at (ghc-subexp)
             "Asynchronously get the fontified type of GHC-SUBEXP.
The result will be nil when there are errors."
             (let ((ty (lcr-call dante-async-call (concat ":type-at " ghc-subexp))))
               (unless (s-match "^\\(<interactive>\\|Couldn't guess that module name\\)" ty)
                 (dante-fontify-expression ty))))

    (defun dante-idle-function ()
      (when (and dante-mode ;; don't start GHCi if dante is not on.
                 (dante-buffer-p) ;; buffer exists
                 (with-current-buffer (dante-buffer-p)
                   (not (eq dante-state 'dead))) ;; GHCi alive?
                 (not lcr-process-callback)) ;; Is GHCi idle?
        (let ((tap (dante--ghc-subexp (dante-thing-at-point))))
          (unless (or (nth 4 (syntax-ppss)) (nth 3 (syntax-ppss)) (s-blank? tap)) ;; not in a comment or string
            (setq-local dante-idle-point (point))
            (lcr-cps-let ((_load_messages (dante-async-load-current-buffer t))
                          (ty (dante--async-type-at tap)))
                         (when (and ty
                                    (let ((cur-msg (current-message)))
                                      (or (not cur-msg)
                                          (string-match-p (concat "^Wrote " (buffer-file-name)) cur-msg)
                                          (and dante-last-valid-idle-type-message
                                               (string-match-p dante-last-valid-idle-type-message cur-msg))))
                                    ;; echo area is free, or the buffer was just saved from having triggered a check, or the queue had many requests for idle display and is displaying the last fulfilled idle type request
                                    (eq (point) dante-idle-point)) ;; cursor did not move
                           (setq dante-last-valid-idle-type-message (s-collapse-whitespace ty))
                           (message "%s" dante-last-valid-idle-type-message)))))))))
