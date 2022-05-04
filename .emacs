;;      _       _                                   
;;     | |     | |                                  
;;   __| | ___ | |_    ___ _ __ ___   __ _  ___ ___ 
;;  / _` |/ _ \| __|  / _ \ '_ ` _ \ / _` |/ __/ __|
;; | (_| | (_) | |_  |  __/ | | | | | (_| | (__\__ \
;;  \__,_|\___/ \__|  \___|_| |_| |_|\__,_|\___|___/
;;
;;
;;
;; ========================================
;; SECTION :: Setup Packages!
(require 'package)
;; :: Add Melpa, Elpa, and local package locations.
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; Local packages are located in a directory called "lisp" which is located in same directory as this file.
;;(message (concat (expand-file-name (locate-dominating-file buffer-file-name ".emacs")) "lisp/"))
(add-to-list 'load-path "/home/scarlett/Development/_Emacs/dotemacs/lisp/")
;;(add-to-list 'load-path "/home/scarlett/.emacs.d/lisp/")
;; Require the local packages.
(require 'awesome-tray)
(require 'htmlize)
(require 'web-mode)
(require 'haskell-unicode-input-method)
(require 'package)
(require 'org)
(require 'org-bullets)
(require 'frame-cmds)
(require 'buffer-move)
;; :: Initialize packages.
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; ========================================
;; SECTION ::
;; :: Setup autosave directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
;; ========================================
;; SECTION :: Appearance
;; --------------------------------------
;; SUB SECTION :: Theme
;; :: My tweaks are based on the DOOM-1337 theme, which I quite like by itself. 
;; :: I decided to personalize it a little bit... and those customizations
;; :: are added at the end of this file.
(load-theme 'doom-1337 t)
;; :: Use Iosevka as the default font. Victor Mono is also used (for comments), but that's
;; :: configured in `customize-face`.
(set-face-attribute 'default nil
                    :family "Iosevka Fixed SS05"                   
                    :height 110)

(solaire-global-mode 1)
;; --------------------------------------
;; SUB SECTION :: Awesome Tray Mode
;; :: Enable Awesome tray mode, which is used to move the time display
;; :: to the mini-buffer.
(awesome-tray-mode -1)
;; :: This is my preferred way to see the date
(defun awesome-tray-module-date-info()
  (format-time-string "%a, %b %-d %H:%M"))
;; :: Override awesome-tray's other modules--just display time.
(setq awesome-tray-active-modules
      '("date"))
;; --------------------------------------
;; SUB SECTION :: Miscellaneous tweaks
;; :: Disable splash screen
(setq inhibit-splash-screen t)
;; :: Hide the menubar, scrollbar, toolbar, and modeline
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(toggle-frame-fullscreen)
;; :: Get rid of the scroll bar
(setq vertical-scroll-bar nil)
;; :: Show the fringe on the side of the frame
(fringe-mode 1)
(set-face-attribute 'fringe nil :background nil)
;; :: Show Git differences in the fringe
(global-diff-hl-mode 1)
;; :: Highlight the active parentheses
(show-paren-mode 1)
;; :: Show line numbbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; :: Highlight current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
                     :background "#333333")
;; :: Remove the time display
(display-time-mode -1)
;; :: Enable transient mark mode
(transient-mark-mode 1)
;; :: Enable telephone-line-mode, which makes the mode bar fancy.
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
(telephone-line-mode 1)
;; :: Make delimiters raindow colors
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(rainbow-delimiters-mode 1)
;; :: Add ligatures
(use-package ligature
  :load-path "/home/scarlett/Development/_Emacs/dotemacs/lisp/"
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
;;(setq split-height-threshold nil)
;;(setq split-width-threshold 80)

;; =======================================
;; SECTION :: Helm Mode
(helm-mode 1)
(setq-default helm-M-x-fuzzy-match t)
(global-set-key "\M-x" 'helm-M-x)
(global-set-key "\C-c\C-m" 'helm-M-x)
(global-set-key "\C-x\C-f" 'helm-find-files) ;; replace emacs default finder
(global-set-key (kbd "C-x b") 'helm-buffers-list)
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
;; ========================================
;; SECTION :: Desktop Mode
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/" "~" "."))
;; =========================================
;; SECTION :: Org Mode
;; Troubleshootin': C-u M-x org-reload, if 'wrong number of arguments'
;; :: Enable org fancy bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; :: Make org mode work with files ending in .
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq truncate-lines nil)
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

;; :: ORG ROAM
;;(setq org-roam-directory (file-truename "~/org-roam"))
;;(org-roam-db-autosync-mode)
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
(defun my-web-mode-hook ()
  "Hooks for Web mode."
;;  (setq web-mode-markup-indent-offset 2)
;;  (setq web-mode-code-indent-offset 2)
;;  (setq web-mode-css-indent-offset 2)
;;  (setq web-mode-comment-style 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'php-mode-hook 'flymake-php-load)
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
;; =========================================
;; SECTION :: Luamode Mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 3)
(setq lua-electric-flag nil)
(setq lua-indent-nested-block-content-align nil)
(defun lua-abbrev-mode-off () (abbrev-mode 0))
(add-hook 'lua-mode-hook 'lua-abbrev-mode-off)
(setq save-abbrevs nil)   ;; is this still needed?
;; ==========================================
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

;; ==========================================
;; SECTION :: Party-zone
;; :: Make the cursor blink through some colors.
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
;; SECTION :: Custom key bindings
;; :: Setup hotkeys for moving to top/bottom of buffer.
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)
;; :: Enable active buffer switch with arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings)
;; :: Setup hotkeys for windows scrolling other window.
(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
;; :: Enable handy keybind for new line.
(defun newline-without-break-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<M-RET>") 'newline-without-break-of-line)
;; :: Setup hotkeys for Org-agenda mode.
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; :: Setup hotkeys to start python console.
(global-set-key (kbd "C-c C-x C-c") 'my-restart-python-console)
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

;; :: Show a clock
;; (setq display-time-string-forms
;;       '((propertize (concat day "/" dayname "/" monthname " - " 12-hours ":" minutes "" am-pm))))
;; (display-time-mode -1)

;; (global-visual-line-mode -1)

;; (set-face-background 'vertical-border "#161616")
;; (set-face-foreground 'vertical-border (face-background 'vertical-border))

;; (defun rag-set-face (frame)
;;   "Configure faces on frame creation"
;;   (select-frame frame)
;;   (if (display-graphic)
;;       (progn
;;         (when (member "Iosevka" (font-family-list))
;;           (progn
;;             (set-frame-font "Iosevka" nil t))))))
;; (add-hook 'after-make-frame-functions #'rag-set-face)

;;set frame font when running emacs normally
;; (when (member "Iosevka" (font-family-list))
;;   (progn
;;     (set-frame-font "Iosevka" nil t)))

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

;; :: Enable EditorConfig Mode
;;(editorconfig-mode 1)

;; https://www.emacswiki.org/emacs/DesktopMultipleSaveFiles

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Windows/Notes/org/todo/daily.org" "~/Windows/Notes/org/todo/default.org")))
 '(package-selected-packages
   (quote
    (flymake-jshint typescript-mode org-roam edit-indirect flymake-php flymake-phpcs package-lint-flymake diff-hl git-gutter-fringe+ solaire-mode rainbow-delimiters telephone-line helm-mode-manager use-package sml-modeline smart-mode-line lua-mode golden-ratio doom-themes all-the-icons)))
 '(sml/show-frame-identification t))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#191919" :foreground "honeydew" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 113 :width normal :foundry "UKWN" :family "Iosevka Fixed SS05"))))
 '(auto-dim-other-buffers-face ((t (:background "#161616"))))
 '(custom-comment ((t (:background "#282828" :foreground "#d4d4d4" :slant italic :family "Victor Mono"))))
 '(font-lock-builtin-face ((t (:foreground "spring green"))))
 '(font-lock-comment-face ((t (:foreground "hot pink" :slant italic :width extra-condensed :family "Victor Mono"))))
 '(font-lock-constant-face ((t (:foreground "salmon"))))
 '(font-lock-function-name-face ((t (:foreground "turquoise" :weight ultra-bold))))
 '(font-lock-keyword-face ((t (:foreground "light slate blue" :weight bold))))
 '(font-lock-string-face ((t (:background "#121212" :foreground "LightGoldenrod1" :slant italic :width ultra-condensed :family "Victor Mono"))))
 '(font-lock-variable-name-face ((t (:foreground "green yellow"))))
 '(fringe ((t (:inherit default :foreground "#4b474c"))))
 '(helm-buffer-not-saved ((t (:foreground "color-118"))))
 '(helm-buffer-saved-out ((t (:background "black" :foreground "color-118"))))
 '(hl-line ((t (:background "#333333"))))
 '(italic ((t (:slant italic :family "Victor Mono"))))
 '(line-number ((t (:inherit default :background "#0a0a0a" :foreground "#333333" :strike-through nil :underline nil :slant normal :weight bold :height 0.8))))
 '(line-number-current-line ((t (:inherit default :background "aquamarine" :foreground "medium slate blue" :strike-through nil :underline nil :slant normal :weight bold :height 0.8))))
 '(markdown-header-face ((t (:inherit bold :foreground "green yellow" :height 1.1))))
 '(minibuffer-prompt ((t (:background "#161616" :foreground "LightGoldenrod1" :inverse-video nil))))
 '(mode-line ((t (:background "medium slate blue" :foreground "pink" :box nil :height 1.0))))
 '(mode-line-inactive ((t (:background "#0a0a0a" :foreground "gray60" :inverse-video nil :box nil :height 1.0))))
 '(sml/col-number ((t (:inherit sml/global :height 1.0))))
 '(sml/filename ((t (:inherit sml/global :foreground "pink" :weight bold))))
 '(sml/git ((t (:foreground "goldenrod1"))))
 '(sml/global ((t (:foreground "DarkSlateGray4" :inverse-video nil))))
 '(sml/line-number ((t (:inherit sml/modes :weight extra-bold :height 0.8))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#777777"))))
 '(sml/time ((t (:inherit nil :distant-foreground "black" :foreground "#999999" :weight extra-bold :height 0.9 :width ultra-condensed :family "Iosevka Fixed SS05"))))
 '(sml/vc ((t (:inherit sml/git :distant-foreground "#d4d4d4" :inverse-video nil :overline t :weight bold :height 1.0))))
 '(solaire-default-face ((t (:inherit default :background "#0a0a0a"))))
 '(telephone-line-accent-active ((t (:background "aquamarine" :foreground "medium slate blue" :weight bold :width ultra-condensed :family "Iosevka"))))
 '(telephone-line-accent-inactive ((t (:background "#0a0a0a" :foreground "#d4d4d4"))))
 '(vertical-border ((t nil)))
 '(web-mode-keyword-face ((t (:foreground "medium slate blue"))))
 '(web-mode-string-face ((t (:inherit font-lock-string-face :foreground "#FBE3BF")))))
