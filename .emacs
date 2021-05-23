;;      _       _                                   
;;     | |     | |                                  
;;   __| | ___ | |_    ___ _ __ ___   __ _  ___ ___ 
;;  / _` |/ _ \| __|  / _ \ '_ ` _ \ / _` |/ __/ __|
;; | (_| | (_) | |_  |  __/ | | | | | (_| | (__\__ \
;;  \__,_|\___/ \__|  \___|_| |_| |_|\__,_|\___|___/
;;
;;
;;
;; SECTION :: Setup Packages!
;;
(require 'package)
;; :: Add Melpa Archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; :: Setup TIDAL
(add-to-list 'load-path "~/Development/_Tidal/_SOURCE/Tidal")
(require 'haskell-mode)
(require 'tidal)

;; :: Load up local packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; :: Load up some other packages
(require 'org)
(require 'org-bullets)
(require 'htmlize)
;;(require 'web-mode)
(require 'haskell-unicode-input-method)

;; :: Initialize packages
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ========================================
;; SECTION :: Theme
(load-theme 'zenburn t)

;; =========================================
;; SECITON :: Usability Tweaks
;; :: Show line numbbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(global-visual-line-mode t) 

;; :: Enable active buffer switch with arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings)

;; :: Setup hotkeys for windows scrolling other window
(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

;; :: Enable handy keybind for new line
(defun newline-without-break-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<M-RET>") 'newline-without-break-of-line)

;; :: Disable splash screen
(setq inhibit-splash-screen t)

;; :: Enable transient mark mode
(transient-mark-mode 1)

;; :: Show a clock
(display-time)

;; :: Enable org fancy bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; :: Enable EditorConfig Mode
(editorconfig-mode 1)

;; ========================================
;; SECTION :: Desktop Mode
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/" "~" "."))

;; =========================================
;; SECTION :: Org Mode

;; :: Make org mode work with files ending in .
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; :: Set hook on task completion: ask for note
(setq org-log-done 'note)

;; :: Set default todo file
(setq org-agenda-files (list "~/org/todo/misc.org"))

;; :: Set agenda key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; =========================================
;; SECITON :: Web Mode

;; :: Set web mode hooks (PHP, HTML)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; =========================================
;; SECTION :: Haskell Mode

;; pretty symbols for haskell
;;(load "~/.emacs.d/lisp/emacs-rc-pretty-lambda.el")

;; :: Set Pretty Haskell Symbols
(add-hook 'haskell-mode-hook
	  (lambda () (set-input-method "haskell-unicode")))

;; ==========================================
;; SECTION :: The Wasteland
;;            Functions not working, or experimental

;; :: Dunno?? -*- mode: elisp -*-

;; :: Ctl+Shift+Return new line
(global-set-key (kbd "<C-S-RET>") (lambda ()
                       (interactive)
                       (beginning-of-line)
                       (newline-and-indent)
                       (previous-line)))

;; hack for annoying mini buffer thing
;; see: http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Insert new line below current line
;; and move cursor to new line
;; it will also indent newline
;;(global-set-key (kbd "<C-return>") (lambda ()
  ;;                 (interactive)
    ;;               (end-of-line)
      ;;             (newline-and-indent)))



;; Set indents to be 2 by default
;;(setq-default indent-tabs-mode t)
;;(setq-default tab-width 2)
;;(setq indent-line-function 'insert-tab)

;; https://www.emacswiki.org/emacs/DesktopMultipleSaveFiles

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default)))
 '(fci-rule-color "#444a73")
 '(jdee-db-active-breakpoint-face-colors (cons "#161a2a" "#82aaff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#161a2a" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#161a2a" "#444a73"))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#ff757f")
 '(org-agenda-files (quote ("~/org/todo/misc.org")))
 '(package-selected-packages
   (quote
    (web-narrow-mode magit ox-epub htmlize web-mode-edit-element editorconfig web-mode zenburn-theme haskell-mode use-package tron-legacy-theme spacemacs-theme doom-themes cyberpunk-theme)))
 '(pdf-view-midnight-colors (cons "#c8d3f5" "#212337"))
 '(rustic-ansi-faces
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(vc-annotate-background "#212337")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7dd85")
    (cons 60 "#ebd27e")
    (cons 80 "#ffc777")
    (cons 100 "#ffb76e")
    (cons 120 "#ffa866")
    (cons 140 "#ff995e")
    (cons 160 "#ea9993")
    (cons 180 "#d599c9")
    (cons 200 "#c099ff")
    (cons 220 "#d58dd4")
    (cons 240 "#ea81a9")
    (cons 260 "#ff757f")
    (cons 280 "#d06a7c")
    (cons 300 "#a15f79")
    (cons 320 "#725476")
    (cons 340 "#444a73")
    (cons 360 "#444a73")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
