;;; -*- lexical-binding: t; -*-

(setq user-emacs-directory "/home/scarlett/Development/_Emacs/dotemacs/")
(setq custom-file (expand-file-name ".emacs" user-emacs-directory))

;; (setq custom-file (concat user-emacs-directory "custom.el"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

(let ((file-name-handler-alist nil))
  ;; Load the remainder of the configuration from the Org configuration file.
  (org-babel-load-file (concat user-emacs-directory "configuration.org")))

;;(when (file-exists-p custom-file)
;;  (load custom-file))
(put 'upcase-region 'disabled nil)
