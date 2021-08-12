;;; -*- lexical-binding: t -*-

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("<M-S-right>" . er/expand-region)
         ("<M-S-left>" . er/contract-region)))
