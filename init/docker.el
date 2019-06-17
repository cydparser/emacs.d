;;; -*- lexical-binding: t -*-

(use-package docker
  :if (executable-find "docker"))

(use-package dockerfile-mode
  :mode "\\.docker\\'")
