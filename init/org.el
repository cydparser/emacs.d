;;; -*- lexical-binding: t -*-

(use-package org
  :hook (org-mode-hook . init-org)
  :init
  (progn
    (setq org-adapt-indentation nil
          org-ellipsis "…"
          org-hide-emphasis-markers t
          org-imenu-depth 3
          org-list-description-max-indent 5
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil))
  :config
  (progn
    (defconst init-org-prettify-symbols-alist
      '(("[ ]" . "☐")
        ("[X]" . "☑")

        ("#+BEGIN_EXAMPLE" . "‘")
        ("#+END_EXAMPLE"   . "’")

        ("#+BEGIN_EXPORT" . "«")
        ("#+END_EXPORT"   . "»")

        ("#+BEGIN_SRC" . "(")
        ("#+begin_src" . "(")
        ("#+END_SRC"   . ")")
        ("#+end_src"   . ")")


        ("#+BEGIN_QUOTE" . "“")
        ("#+END_QUOTE"   . "”")

        ("#+RESULTS:"  . "≅")
        ("#+PROPERTY:" . "≔")))

    (add-to-list 'org-modules 'org-tempo t)

    (defun init-org ()
      (setq-local prettify-symbols-alist init-org-prettify-symbols-alist)
      (prettify-symbols-mode))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)))))

(use-package org-bullets
  :hook (org-mode-hook . org-bullets-mode)
  :config
  (progn
    (setcdr org-bullets-bullet-map nil)))
