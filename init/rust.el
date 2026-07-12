;;; -*- lexical-binding: t -*-

(use-package rust-mode
  :bind (:map rust-mode-map
              (":" . init-rust-insert-colon)
              ("M-<return>" . eglot-x-on-enter)
              ("C-c C-f" . rust-format-buffer))
  :custom
  (rust-mode-treesitter-derive t)
  :init
  (progn
    (with-eval-after-load 'projectile
      (defun init-projectile-rust-related-files-fn (file)
        (let ((file-name (file-name-nondirectory file))
              (dir (file-name-directory file)))
          (cond
           ((string-equal "tests.rs" file-name)
            (list :impl (if (string-equal "src" (file-name-nondirectory (directory-file-name dir)))
                            (file-name-concat dir "lib.rs")
                          (replace-regexp-in-string "[/]?$" ".rs" dir))))
           ((string-equal "lib.rs" file-name)
            (list :test (file-name-concat dir "tests.rs")))
           (t
            (list :test (replace-regexp-in-string "[.]rs$" "/tests.rs" file))))))

      (projectile-update-project-type
       'rust-cargo
       :src-dir "src"
       :test-dir "src"
       :test-suffix "/tests"
       :related-files-fn #'init-projectile-rust-related-files-fn))
    )
  :config
  (progn
    (require 'init-utils)

    (defun init-rust-insert-colon ()
      "Insert a colon unless the previous character is a colon or the following
character is a space or colon"
      (interactive)
      (let ((prev-char (char-before)))
        (insert-char ?:)
        (unless (or (char-equal ?: prev-char)
                    (char-equal ?{ prev-char)
                    (bound-and-true-p multiple-cursors-mode)
                    (init-treesit-first-ancestor-with-type
                     [token_tree_pattern string_content] :include-node 'rust)
                    ;; Tree-sitter produces error nodes for some partial patterns like `($tt:)'.
                    (let ((p (point)))
                      (re-search-backward "[)] *=>" (line-beginning-position) :noerror)
                      (let ((match (re-search-forward "[$][[:alnum:]]+:" p t)))
                        (goto-char p)
                        match)))
          (let ((char (read-key nil :disable)))
            (when char
              (cond ((not
                      (and (characterp char)
                           (<= ?\s char)
                           (or (>= ?z char)
                               ;; The only modifier pressed is shift.
                               (= 0 (logand char
                                            ;; Codes extracted from `event-modifiers'.
                                            (logior ?\M-\0 ; meta
                                                    ?\C-\0 ; ctrl
                                                    ?\H-\0 ; hyper
                                                    ?\s-\0 ; super
                                                    ?\A-\0 ; alt
                                                    ))))))
                     (setq unread-command-events (list char)))
                    ((or (char-equal ?: char)
                         (char-equal ?\s char))
                     (self-insert-command 1 char))
                    (t
                     (insert-char ?:)
                     (self-insert-command 1 char))))))))

    (with-eval-after-load 'compile
      (require 'rust-compile)

      ;; thread 'tests::test_div_ceil2' (1769198) panicked at bitfield/impl/src/lib.rs:80:9:
      (add-to-list 'compilation-error-regexp-alist-alist
                   (cons 'rustc-thread-panicked
                         (cons (concat "^thread [^ ]+ [^ ]+ panicked at " rustc-compilation-location) '(2 3 4 0 1))))

      (add-to-list 'compilation-error-regexp-alist-alist
                   (cons 'rustc-arrow
                         (cons (concat "^ +--> " rustc-compilation-location) '(2 3 4 0 1)))))
    ))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  :config
  (progn
    (remove-hook 'flycheck-mode-hook 'rustic-flycheck-setup)
    (remove-hook 'rustic-mode-hook 'flycheck-mode)
    (remove-hook 'rustic-mode-hook 'flymake-mode-off)

    (defun init-rustic-mode ()
      "Disable LSP for /nix and .cargo directories."
      (when (and buffer-file-name
                 (string-match "\\(^/nix/\\|/\\(target\\|\\.cargo\\)/\\)" buffer-file-name))
        (setq-local rustic-lsp-client nil)))

    (add-hook 'rustic-mode-hook #'init-rustic-mode -100)

    (with-eval-after-load 'smartparens-rust
      (defun init-smartparens-rust-single-quote-p (_id action _context)
        (and
         (eq action 'insert)
         (or
          (when-let ((char (char-after)))
            (or
             (= ?> char)
             (= ?\" char)))
          (when-let ((char (char-before (point))))
            (or
             (= ?, char)
             (= ?< char)))
          (when-let ((char (char-before (- (point) 1))))
            (= ?& char))
          (when-let ((node (treesit-node-at (point) 'rust)))
            (let ((type (treesit-node-type node)))
              (seq-some (lambda (name) (string-equal name type))
                        [">"
                         "doc_comment"
                         "line_comment"
                         "string_content"
                         "type_identifier"
                         ])))
          (init-treesit-first-ancestor-with-type
           [type_arguments type_parameters] :include-node 'rust))))

      (let ((modes '(rust-mode rust-ts-mode rustic-mode)))
        (init-smartparens-add-return-posthandler modes)
        (sp-with-modes modes
          (sp-local-pair "'" "'"
                         :unless '((:add init-smartparens-rust-single-quote-p) (:rem sp-in-comment-p sp-in-string-quotes-p))
                         :post-handlers'(:rem sp-escape-quotes-after-insert))

          (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                                    ("||\n" "RET"))))))
    ))
