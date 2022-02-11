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
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; :: Setup TIDAL
;;(add-to-list 'load-path "~/Development/_Tidal/_SOURCE/Tidal")
;;(require 'haskell-mode)
;;(require 'tidal)

;; :: Load up local packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

;; :: Load up some other packages
(require 'org)
(require 'org-bullets)
(require 'htmlize)
(require 'web-mode)
(require 'haskell-unicode-input-method)
(require 'setup-ligatures)

;; (require 'eaf)
;; (require 'eaf-file-sender)
;; (require 'eaf-terminal)
;; (require 'eaf-browser)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-file-manager)
;; (require 'eaf-file-browser)
;; (require 'eaf-image-viewer)
;; (require 'eaf-rss-reader)
;; (require 'eaf-demo)
;; (require 'eaf-music-player)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-jupyter)
;; (require 'eaf-system-monitor)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-vue-demo)
;; (require 'eaf-org-previewer)
;; (require 'eaf-camera)
;; (require 'eaf-airshare)

;; :: Initialize packages
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; :: Setup autosave directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; ========================================
;; SECTION :: Theme
(load-theme 'doom-1337 t)

(set-face-attribute 'menu nil
                    :inverse-video nil
                    :background "black"
                    :foreground "black"
                    :bold t)

(set-face-attribute 'line-number nil
                    :inverse-video nil
                    :background "#161616"
                    :foreground "#444444"
                    :bold nil)

(set-face-attribute 'line-number-current-line nil
                    :inverse-video nil
                    :background "pink"
                    :foreground "#161616"
                    :bold nil)

(set-face-attribute 'mode-line nil
                    :background "#333333")

(set-face-attribute 'mode-line-inactive nil
                    :foreground "pink"
                    :background "#333333")

(set-face-attribute 'cursor nil
                    :background "pink")

(set-face-background 'vertical-border "#161616")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(setq vertical-scroll-bar nil)

;; set Iosevka font only if it available
(defun rag-set-face (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic)
      (progn
        (when (member "Iosevka" (font-family-list))
          (progn
            (set-frame-font "Iosevka" nil t))))))
(add-hook 'after-make-frame-functions #'rag-set-face)

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 110)

;;  

;;set frame font when running emacs normally
(when (member "Iosevka" (font-family-list))
  (progn
    (set-frame-font "Iosevka" nil t)))

(set-face-attribute 'fringe nil :background nil)
;; =========================================
;; SECITON :: Usability Tweaks
;; :: Show line numbbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(global-visual-line-mode t) 

;; :: Hide the menubar, scrollbar, and toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; :: Enable Smart Mode Line
;;(setq sml/theme 'dark)
(setq sml/override-theme nil)
(sml/setup)

;; :: Enable All The Icons
;; Note: this doesn't work!
(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

;; :: Highlight current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
                     :background "#333333")

;; :: Enable Autodim
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode nil))))

;; :: Enable Golden Ration screen resizing
(golden-ratio-mode 0)
(setq golden-ratio-auto-scale t)

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
(setq display-time-string-forms
      '((propertize (concat day "/" dayname "/" monthname " - " 12-hours ":" minutes "" am-pm))))
(display-time)

;; :: Enable org fancy bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; :: Enable EditorConfig Mode
;;(editorconfig-mode 1)

;; =======================================
;; SECTION :: Helm Mode
(helm-mode 1)
(setq-default helm-M-x-fuzzy-match t)
(global-set-key "\C-x\C-m" 'helm-M-x)
(global-set-key "\C-c\C-m" 'helm-M-x)

;; Might want to play around with these functions later
;; (define-key evil-ex-map "b " 'helm-mini)
;; (define-key evil-ex-map "e" 'helm-find-files)

;; (require 'helm-projectile)
;; (define-key evil-ex-map "g" 'helm-projectile-grep)
;; (define-key evil-ex-map "f" 'helm-projectile-find-file)

;; =======================================
;; SECTION :: Magit Mode

(remove-hook 'server-switch-hook 'magit-commit-diff)
(setq magit-refresh-status-buffer nil)

;; ========================================
;; SECTION :: Python Mode
(defun my-restart-python-console ()
  "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
  (setq python-shell-completion-native-enable -1)
  (interactive)
  (if (get-buffer "*Python*")
      (let ((kill-buffer-query-functions nil)) (kill-buffer "*Python*")))
  (run-python)
  (python-shell-send-buffer))

(global-set-key (kbd "C-c C-x C-c") 'my-restart-python-console)

;; ========================================
;; SECTION :: Desktop Mode
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/" "~" "."))

;; =========================================
;; SECTION :: Org Mode

;; :: Make org mode work with files ending in .
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; :: Set hook on task completion: ask for note
;;(setq org-log-done 'note)

;; :: Set default todo file
(setq org-agenda-files (list "~/Windows/Notes/org/todo/default.org" "~/org/todo/misc.org"))

;; :: Set Todo keywords
;;(setq org-todo-keywords
;;      '((sequence "TODO" "WAIT" "VERIFY" "|" "DONE" "DELEGATED")))

(setq org-todo-keyword-faces
 '(("IN-PROGRESS" . "orange") ("WAIT" . "yellow") ("CANCELED" . "red") ("DO" . "green"))
 )

;; Set timestamp on TODO create
;;(setq org-treat-insert-todo-heading-as-state-change t)

(defun my/log-todo-creation-date (&rest ignore)
  "Log TODO creation time in the property drawer under the key 'CREATED'."
  (when (and (org-get-todo-state)
             (not (org-entry-get nil "CREATED")))
    (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))

(advice-add 'org-insert-todo-heading :after #'my/log-todo-creation-date)
(advice-add 'org-insert-todo-heading-respect-content :after #'my/log-todo-creation-date)
(advice-add 'org-insert-todo-subheading :after #'my/log-todo-creation-date)

(add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)

;; Setup agenda command to show completed tasks
;; https://emacs.stackexchange.com/questions/52994/org-mode-agenda-show-list-of-tasks-done-in-the-past-and-not-those-clocked

(setq org-agenda-custom-commands
      '(("W" "Weekly review with archives"
         agenda ""
         ((org-agenda-start-day "-6d")
          (org-agenda-span 14)
          (org-agenda-start-on-weekday 1)
          (org-agenda-archives-mode t)
          (org-agenda-start-with-log-mode t)
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))
        ("R" "Weekly review"
         agenda ""
         ((org-agenda-start-day "-6d")
          (org-agenda-span 14)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-log-mode '(closed))
          (org-agenda-archives-mode t)
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))
        ("n" "Agenda and all TODOs"
         agenda ""
         ((alltodo "")))
        ))

;; :: Set fancy todo states
;;(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "<img draggable="false" role="img" class="emoji" alt="✔" src="https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/2714.svg"> DONE(d)")
;;(sequence "⚑ WAITING(w)" "|")
;;(sequence "|" "✘ CANCELED(c)")))

;; :: Set Color coding for @ contexts
;; https://stackoverflow.com/questions/40876294/color-tags-based-on-regex-emacs-org-mode/40918994#40918994

;; :: Set agenda key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; =========================================
;; SECITON :: Web Mode

;; :: Set web mode hooks (PHP, HTML)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(setq geben-dbgp-default-port 90)
(setq geben-dbgp-default-host "127.0.0.1")

;;(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)
;; Debug a simple PHP script.
;; Change the session key my-php-54 to any session key text you like
(defun my-php-debug ()
  "Run current PHP script for debugging with geben"
  (interactive)
  (call-interactively 'geben)
  (shell-command (concat "XDEBUG_CONFIG='idekey=my-php-54' /usr/bin/php "
                         (buffer-file-name) " &"))
  )

(global-set-key [f5] 'my-php-debug)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
;;  (setq web-mode-markup-indent-offset 2)
;;  (setq web-mode-code-indent-offset 2)
;;  (setq web-mode-css-indent-offset 2)
;;  (setq web-mode-comment-style 2)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

;; Remove tab stops
(setq-default indent-tabs-mode nil)

;; Highlight current column
(setq web-mode-enable-current-column-highlight t)

;; Snippets
(setq web-mode-extra-snippets
      '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
        ("php" . (("bif" . "@if (|) \n@else\n @endif")
                  ("!" . "{!! $| !!}")
                  ("bforeach" . "@foreach ($|) \n\n @endforeach")
                  ("div" . "<div class=\"|\">\n</div>")
                  ("uselog" . "use Illuminate\\Support\\Facades\\Log;")
                  ("publicfunction" . "/**\n*\n*\n*/\npublic function |()\n{\n}\n")
                  ("debug" . "<?php error_log(__LINE__); ?>")))
        ))

;; =========================================
;; SECTION :: Haskell Mode

;; pretty symbols for haskell
;;(load "~/.emacs.d/lisp/emacs-rc-pretty-lambda.el")

;; :: Set Pretty Haskell Symbols
(add-hook 'haskell-mode-hook
	  (lambda () (set-input-method "haskell-unicode")))


;;
;; SECTION :: Custom helper functions
;; from numlocked on yCombinator
(defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))


(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")
(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'. 
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
 '(auto-dim-other-buffers-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (doom-1337)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4133d2d6553fe5af2ce3f24b7267af475b5e839069ba0e5c80416aa28913e89a" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "f302eb9c73ead648aecdc1236952b1ceb02a3e7fcd064073fb391c840ef84bca" "1f50a7274cd56f28713e1694600ec7b8f2fd1c7d2ef38c5e7378a26931605409" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "5aef652e40fa5f111e78997285f6e4c892112da0c2f919eb663baaa330a8521f" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "716f0a8a9370912d9e6659948c2cb139c164b57ef5fda0f337f0f77d47fe9073" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "cf9414f229f6df728eb2a5a9420d760673cca404fee9910551caf9c91cff3bfa" "d7ee1fdb09a671a968b2a751746e5b3f5f26ac1fd475d95d094ee1e4ce446d58" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default)))
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-use-mail-icon nil)
 '(exwm-floating-border-color "#1e1e3f")
 '(fci-rule-color "#444a73")
 '(fringe-mode 0 nil (fringe))
 '(helm-completion-style (quote helm))
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(highlight-tail-colors
   ((("#2e3c4c" "#2e3c4c" "green")
     . 0)
    (("#42305a" "#42305a" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#161a2a" "#82aaff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#161a2a" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#161a2a" "#444a73"))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "pink")
 '(org-agenda-files
   (quote
    ("~/Windows/Notes/org/todo/issues.org" "~/Development/csv_to_org/todo.org" "~/Windows/Notes/org/todo/web_content.org" "~/Windows/Notes/org/todo/daily.org_archive" "~/Windows/Notes/org/todo/sage.org" "~/Windows/Notes/org/scrum/scrum.org" "~/Windows/Notes/org/todo/daily.org" "~/Windows/Notes/org/todo/default.org")))
 '(package-selected-packages
   (quote
    (rainbow-delimiters smart-mode-line all-the-icons-ibuffer all-the-icons-dired all-the-icons helm auto-dim-other-buffers golden-ratio geben yaml-mode exec-path-from-shell elpy markdown-mode melancholy-theme web-narrow-mode magit ox-epub htmlize editorconfig zenburn-theme haskell-mode use-package tron-legacy-theme spacemacs-theme doom-themes cyberpunk-theme)))
 '(pdf-view-midnight-colors (cons "#c8d3f5" "#212337"))
 '(rustic-ansi-faces
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(sml/show-frame-identification nil)
 '(sml/theme (quote dark))
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
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#161616"))))
 '(custom-comment ((t (:background "#282828" :foreground "#d4d4d4" :slant italic :family "Victor Mono"))))
 '(font-lock-comment-face ((t (:foreground "#6D6D6D" :slant italic :width extra-condensed :family "Victor Mono"))))
 '(font-lock-keyword-face ((t (:foreground "light slate blue" :weight bold))))
 '(font-lock-string-face ((t (:background "#181818" :foreground "LightGoldenrod1"))))
 '(helm-buffer-not-saved ((t (:foreground "color-118"))))
 '(helm-buffer-saved-out ((t (:background "black" :foreground "color-118"))))
 '(hl-line ((t (:background "#333333"))))
 '(italic ((t (:slant italic :family "Victor Mono"))))
 '(line-number ((t (:inherit default :background "#0a0a0a" :foreground "#333333" :strike-through nil :underline nil :slant normal :weight bold :height 0.8))))
 '(line-number-current-line ((t (:inherit default :background "pink" :foreground "black" :strike-through nil :underline nil :slant normal :weight bold :height 0.8))))
 '(minibuffer-prompt ((t (:background "black" :foreground "indian red" :inverse-video nil))))
 '(mode-line ((t (:background "#0a0a0a" :foreground "gray60" :inverse-video nil :box nil :overline "pink"))))
 '(mode-line-inactive ((t (:background "#0a0a0a" :foreground "gray60" :inverse-video nil :box nil))))
 '(sml/filename ((t (:inherit sml/global :foreground "pink" :weight bold))))
 '(sml/git ((t (:foreground "goldenrod1"))))
 '(sml/global ((t (:foreground "#888888" :inverse-video nil))))
 '(sml/line-number ((t (:inherit sml/modes :weight extra-bold :height 0.8))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#777777"))))
 '(sml/time ((t (:inherit nil :distant-foreground "black" :foreground "honeydew" :weight extra-bold :height 0.9 :width ultra-condensed :family "Iosevka"))))
 '(sml/vc ((t (:inherit sml/git :distant-foreground "#d4d4d4" :inverse-video nil :overline t :weight bold :height 1.0))))
 '(vertical-border ((t nil))))
