;;; -*- lexical-binding: t -*-

(require 'init-utils)

;;; Commands

(defun init-delete-excess-whitespace (&optional n)
  "Deletes excess spaces and newlines.
When on an non-empty line, delete one or N spaces (see
`just-one-space')."
  (interactive "*p")
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*$"))
      (delete-blank-lines)
    (just-one-space n)))

(defun init-kill-buffer-current ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun init-kill-region-or-backward-word (arg)
  "Kill selected region or backward word."
  (interactive "p")
  (if (region-active-p)
      (kill-region (mark) (point) 'region)
    (backward-kill-word arg)))

(defun init-sexp-downcase (&optional arg)
  "Convert the sexp to lower case.
ARG determines the direction and number of sexps."
  (interactive "*P")
  (mark-sexp arg)
  (downcase-region (region-beginning) (region-end)))

(defun init-sexp-upcase (&optional arg)
  "Convert the sexp to upper case.
ARG determines the direction and number of sexps."
  (interactive "*P")
  (mark-sexp arg)
  (upcase-region (region-beginning) (region-end)))

;;; Global Configuration

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(make-directory init-config-directory :parents)

;; Store auto-saves and backups in emacs.d/var.
(let ((adir (expand-file-name "autosaves/" init-var-directory))
      (ldir (expand-file-name "auto-save-list/" init-var-directory))
      (bdir (expand-file-name "backups/" init-var-directory)))
  (make-directory adir t)
  (make-directory bdir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat adir "\\1") t))
        auto-save-list-file-prefix (concat ldir "/saves-")
        backup-directory-alist `((".*" . ,bdir))))

;; Disable lock files.
(setq create-lockfiles nil)

;; Split windows horizontally.
(setq split-height-threshold nil)

;; Simplify prompts.
(fset 'yes-or-no-p #'y-or-n-p)

;; Reduce noise.
(setq auto-revert-mode-text ""
      confirm-kill-processes nil
      confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function #'ignore
      xref-prompt-for-identifier nil)

;; Prevent accidental closure.
(setq confirm-kill-emacs #'y-or-n-p)

;; Display column number in modeline.
(setq column-number-mode t)

;; Collect garbage less frequently.
(setq gc-cons-threshold 104857600)

;; Delete the trailing newline.
(setq kill-whole-line t)

;; Adjust indentation and line wrapping.
(let ((spaces 2)
      (max-line-length 80))
  (setq-default fill-column max-line-length
                indent-tabs-mode nil
                standard-indent spaces
                tab-width spaces))

(defalias 'ar #'align-regexp)
(defalias 'rs #'replace-string)
(defalias 'sb #'speedbar)
(defalias 'sl #'sort-lines)

(define-key input-decode-map [(control ?I)] (kbd "<backtab>"))

(bind-key "C-S-C" #'kill-ring-save)
(bind-key "C-S-V" #'yank)
(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-h C-f" #'find-function-at-point)
(bind-key "C-h C-k" #'find-function-on-key)
(bind-key "C-h C-v" #'find-variable-at-point)
(bind-key "C-w" #'init-kill-region-or-backward-word)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'init-kill-buffer-current)
(bind-key "M-/" #'hippie-expand)
(bind-key "M-L" #'init-sexp-downcase)
(bind-key "M-R" #'repeat)
(bind-key "M-SPC" #'init-delete-excess-whitespace)
(bind-key "M-U" #'init-sexp-upcase)

(delete-selection-mode 1)

(global-subword-mode 1)
(diminish 'subword-mode "")

(when (string-equal (seq-take emacs-version 2) "25")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional _param)
       (list start end))))

;;; Packages

(use-package align
  :ensure nil
  :config
  (progn
    (dolist (mode '(haskell-mode ruby-mode))
      (add-to-list 'align-open-comment-modes mode))

    (dolist (tr '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                  (haskell-assignment  . "\\(\\s-+\\)\\(=\\|:=\\|.=\\)\\s-+")
                  (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))
      (add-to-list 'align-rules-list
                   `(,(car tr) (regexp . ,(cdr tr))
                     (modes . '(haskell-mode literate-haskell-mode)))))))

(use-package async
  :demand
  :hook (after-init-hook . async-bytecomp-package-mode)
  :init (setq async-bytecomp-allowed-packages '(all)))

(use-package autorevert
  :ensure nil
  :init (setq auto-revert-verbose nil
              auto-revert-tail-mode-text " ⭛"))

(use-package bookmark
  :ensure nil
  :init (setq bookmark-default-file (expand-file-name "bookmarks" init-var-directory)
              bookmark-save-flag 1))

(use-package diff-mode
  :ensure nil
  :config
  (progn
    (unbind-key "o" diff-mode-shared-map)))

(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode . ""))

(use-package etags
  :ensure nil
  :init (setq tags-add-tables nil
              tags-revert-without-query t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("DICPATH" "PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(use-package flymake
  :ensure nil
  :after hydra
  :bind (:map flymake-mode-map
              ("M-n" . hydra-flymake/flymake-goto-next-error)
              ("M-p" . hydra-flymake/flymake-goto-prev-error))
  :config
  (progn
    (defhydra hydra-flymake ()
      "flymake"
      ("M-n" flymake-goto-next-error nil)
      ("M-p" flymake-goto-prev-error nil)

      ("n" flymake-goto-next-error "next")
      ("p" flymake-goto-prev-error "previous")

      ("c" flymake-proc-compile "compile")

      ("d" (seq-map #'flymake--disable-backend (flymake-running-backends)) "disable" :color blue)

      ("q" nil "quit"))))

(use-package hippie-exp
  :ensure nil
  :init (setq hippie-expand-try-functions-list (seq-difference hippie-expand-try-functions-list
                                                               '(try-expand-line try-expand-list))))

(use-package hydra
  :demand)

(use-package smerge-mode
  :ensure nil
  :after hydra
  :bind (:map smerge-mode-map
              ("C-c m n" . hydra-smerge/smerge-next)
              ("C-c m p" . hydra-smerge/smerge-prev)
              ("C-c m l" . hydra-smerge/smerge-keep-lower)
              ("C-c m u" . hydra-smerge/smerge-keep-upper))
  :config
  (progn
    (defhydra hydra-smerge ()
      "smerge"
      ("n" smerge-next "next")
      ("p" smerge-prev "prev")
      ("e" smerge-ediff "ediff")
      ("r" smerge-refine "refine")
      ("a" smerge-keep-all "keep-all")
      ("c" smerge-keep-current "keep-current")
      ("l" smerge-keep-lower "keep-lower")
      ("u" smerge-keep-upper "keep-upper")
      ("w" save-buffer "done" :color blue)
      ("q" nil "quit"))))

(use-package mwheel
  :ensure nil
  :init (setq mouse-wheel-flip-direction t
              mouse-wheel-tilt-scroll t))

(use-package newcomment
  :ensure nil
  :bind ("M-;" . comment-line))

(use-package rect
  :ensure nil
  :after hydra
  :bind ("C-x SPC" . hydra-rectangle/rectangle-mark-mode)
  :config
  (progn
    (defhydra hydra-rectangle (:color pink :post (deactivate-mark))
      "rect"
      ("m" rectangle-mark-mode nil)

      ("b" rectangle-backward-char "backward")
      ("f" rectangle-forward-char "forward")
      ("n" rectangle-next-line "next")
      ("p" rectangle-previous-line "previous")
      ("e" rectangle-exchange-point-and-mark "exchange")

      ("0" (rectangle-number-lines (region-beginning) (region-end) 0) "0" :color blue)
      ("1" rectangle-number-lines "1" :color blue)
      ("s" string-rectangle "string" :color blue)
      ("o" open-rectangle "open" :color blue)
      ("c" clear-rectangle "clear" :color blue)
      ("d" delete-rectangle "delete" :color blue)

      ("r" copy-rectangle-to-register "to-register" :color blue)
      ("k" kill-rectangle "kill" :color blue)
      ("y" yank-rectangle "yank" :color blue)
      ("w" copy-rectangle-as-kill "XXX" :color blue)

      ("q" nil "quit"))))
