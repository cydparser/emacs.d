;;; -*- lexical-binding: t -*-

(use-package org
  :hook (org-mode-hook . init-org)
  :custom
  (org-adapt-indentation nil)
  (org-ellipsis "…")
  (org-hide-emphasis-markers t)
  (org-imenu-depth 3)
  (org-list-description-max-indent 5)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-startup-truncated nil)
  :config
  (progn
    (unbind-key "<M-S-down>" org-mode-map)
    (unbind-key "<M-S-up>" org-mode-map)
    (unbind-key "<M-down>" org-mode-map)
    (unbind-key "<M-up>" org-mode-map)
    (unbind-key "<C-S-down>" org-mode-map)
    (unbind-key "<C-S-up>" org-mode-map)

    (defconst init-org-prettify-symbols-alist
      '(("[ ]" . "☐")
        ("[X]" . "☑")

        ("#+BEGIN_EXAMPLE" . "‘")
        ("#+begin_example" . "‘")
        ("#+END_EXAMPLE"   . "’")
        ("#+end_example"   . "’")

        ("#+BEGIN_EXPORT" . "«")
        ("#+begin_export" . "«")
        ("#+END_EXPORT"   . "»")
        ("#+end_export"   . "»")

        ("#+BEGIN_SRC" . "(")
        ("#+begin_src" . "(")
        ("#+END_SRC"   . ")")
        ("#+end_src"   . ")")

        ("#+BEGIN_QUOTE" . "“")
        ("#+begin_quote" . "“")
        ("#+END_QUOTE"   . "”")
        ("#+end_quote"   . "”")

        ("#+RESULTS:"  . "≅")
        ("#+results:"  . "≅")
        ("#+PROPERTY:" . "≔")
        ("#+property:" . "≔")
        ))

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

(use-package org-sticky-header
  :hook (org-mode-hook . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'reversed))
