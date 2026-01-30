;;; -*- lexical-binding: t -*-

(custom-set-variables
 ;; Disable GC during init.
 '(gc-cons-threshold most-positive-fixnum)

 ;; Reduce noise.
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)

 ;; Disable mouse UI modes.
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)

 '(default-frame-alist
   '((background-color . "#000000")
     (foreground-color . "#c5c8c6")
     (font . "FiraCode Nerd Font-14"))))
