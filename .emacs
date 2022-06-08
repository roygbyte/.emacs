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
;;(require 'org)
;;(require 'ligature)
(require 'org-bullets)
(require 'frame-cmds)
(require 'buffer-move)
(require 'dired+)
(require 'smartparens-config)
(require 'all-the-icons)
(require 'org-modern)
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
                    :family "Iosevka Fixed"
                    :height 110)

(solaire-global-mode 1)


;; Tab bar ? What is this
(add-to-list 'tab-bar-format 'tab-bar-format-align-right t)
(add-to-list 'tab-bar-format 'tab-bar-format-global t)
(display-time-mode)
;;(add-to-list 'tab-bar-format 'display-time-mode t)
(tab-bar-mode 1)
;; --------------------------------------
;; SUB SECTION :: Awesome Tray Mode
;; :: Enable Awesome tray mode, which is used to move the time display
;; :: to the mini-buffer.
;; (awesome-tray-mode 1)
;; ;; :: This is my preferred way to see the date
;; (defun awesome-tray-module-date-info()
;;   (format-time-string "%a, %b %-d %H:%M"))
;; ;; :: Override awesome-tray's other modules--just display time.
;; (setq awesome-tray-active-modules
;;       '("date"))
;; --------------------------------------
;; SUB SECTION :: Miscellaneous tweaks
;; :: Disable splash screen
(setq inhibit-splash-screen t)
;; :: Hide the menubar, scrollbar, toolbar, and modeline
(toggle-frame-fullscreen)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; :: Get rid of the scroll bar
(setq vertical-scroll-bar nil)
;; :: Show the fringe on the side of the frame
(setq-default left-margin-width nil right-margin-width nil)
(set-window-buffer nil (current-buffer))
(setq fringes-outside-margins t)
(window-divider-mode)

(fringe-mode 1)
(set-fringe-style '(12 . 12))
(set-face-attribute 'fringe nil :background nil)
;; :: Show Git differences in the fringe
(global-diff-hl-mode 1)
(diff-hl-margin-mode)
(diff-hl-flydiff-mode)
;; :: Highlight the active parentheses
(show-paren-mode 1)
;; :: Show line numbbers
;;(when (version<= "26.0.50" emacs-version )
;;  (global-display-line-numbers-mode))
;; :: Highlight current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
                     :background "#333333")
;; :: Remove the time display
;;(display-time-mode -1)
;; :: Enable transient mark mode
(transient-mark-mode 1)
;; :: Enable icons in dired mode
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
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
;; SECTION :: Elisp mode

;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                   (smartparens-mode)
;;                                   (smartparens-strict-mode t)
;;                                   )

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
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-org-modern-mode 1)
(setq org-modern-todo-faces
      (quote (("DO" :background "green"
               :foreground "black")
              ("WAIT" :background "yellow"
               :foreground "black"))))

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
;;(setq org-todo-keyword-faces
;; '(("IN-PROGRESS" . "orange") ("WAIT" . "yellow") ("CANCELED" . "red") ("DO" . "green"))
;; )

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

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
;;(setq org-roam-directory (file-truename "~/Dropbox/org"))

;; (use-package org-roam
;;   :custom
;;   (org-roam-directory "~/Dropbox/org/")
;;   
;;   (org-roam-capture-templates
;;    '(("d" "default" plain
;;       #'org-roam-capture--get-point "%?"
;;       :target (file+head "pages/${slug}.org"
;;                          "#+title: ${title}\n")
;;       :unnarrowed t))))
;; (org-roam-setup)

;; ==================================
;; Org-roam mode!

(defun roygbyte/org-roam-tia--entry ()
  (prog1 (concat "* Today I Achieved: %? \n"
          ":PROPERTIES:\n"
          ;; Add file tags
          ":END:\n")))

          ;; (concat ":ID: " (org-id-new) "\n")

(org-roam-db-autosync-mode t)
(use-package org-roam 
    :ensure t 
    :init 
    (setq org-roam-v2-ack t) 
    :custom 
    (setq org-roam-directory "~/Dropbox/org")
    (setq org-roam-dailies-directory "journals/")
    (setq org-roam-completion-system 'helm)
    (setq org-roam-complete-everywhere t)
    (setq org-roam-node-display-template ;; Rollback org-roam find behavior to show tags
          (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (setq org-roam-capture-templates
      '(
        ("d" "default" plain "%?"
         :target (file+head "pages/${slug}.org" ;; add .gpg for encryption!
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("e" "encrypted" plain "%?"
         :target (file+head "pages/${slug}.org.gpg" ;; add .gpg for encryption!
                            "#+title: ${title}\n")
         :unnarrowed t)))
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry "* %<%I:%M %p>: %?"
             :if-new (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))
            ("t" "tia!" entry "* Today I Achieved: %? \n:PROPERTIES:\n:END:\n"
             :if-new (file+head "%<%Y_%m_%d>.org" (concat "#+title: %<%Y-%m-%d>\n" roygbyte/org-roam-tia--entry)))))
    )

;; So the issue with my template is that the template string passed to the ... template (?) needs to be a file

(org-roam-setup)

;;(setq org-roam-dailies-directory "journals/")

;; Add tags to the org roam find
;; https://emacs.stackexchange.com/questions/70552/tag-column-for-filetag-in-org-roam-node-list-is-is-gone/70560#


;; (defmacro go-roam-find-file-project-fn (project)
;;   "Define a function to find an `org-roam' file within the given PROJECT."
;;   (let* ((fn-name (intern (concat "go-roam-find-" (replace-regexp-in-string " +" "-" project))))
;;          (docstring (concat "Find an `org-roam' file for: " project)))
;;     `(defun ,fn-name (&optional completions filter-nf no-confirm)
;;        ,docstring
;;        (interactive)
;;        (org-roam-find-file (concat ,project " ") completions filter-nf no-confirm))))



;; (go-roam-find-file-project-fn "thel-sector")
;; (go-roam-find-file-project-fn "ardu")
;; (go-roam-find-file-project-fn "permanent bibliographies")
;; (go-roam-find-file-project-fn "permanent cards")
;; (go-roam-find-file-project-fn "hesburgh-libraries")
;; (go-roam-find-file-project-fn "samvera")

;; (defvar jnf-find-file-in-roam-project--title (with-octicon "book" "Find File in Roam Project" 1 -0.05))
;; (pretty-hydra-define jnf-find-file-in-roam-project (:foreign-keys warn :title jnf-find-file-in-roam-project--title :quit-key "q")
;;   (
;;    "Permanent"
;;    (("b" go-roam-find-permanent-bibliographies "Bibliography")
;;     ("c" go-roam-find-permanent-cards "Card"))
;;    "RPGs"
;;    (("a" go-roam-find-ardu "Ardu, World of")
;;     ("t" go-roam-find-thel-sector "Thel Sector"))
;;    "Work"
;;    (("h" go-roam-find-hesburgh-libraries "Hesburgh Libraries")
;;     ("s" go-roam-find-samvera "Samvera"))

;;    ))
;; (global-set-key (kbd "M-1") 'jnf-find-file-in-roam-project/body)

;; Hydra config
(require 'hydra)
(require 'pretty-hydra)

;; https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325
(defun with-octicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))


;;(org-roam-find-file (concat ,project " ") completions filter-nf no-confirm)

(defvar org-roam-launcher--title (with-octicon "book" "Dun Org-roamin'" 1 -0.05))
(pretty-hydra-define org-roam-launcher (:color pink :title org-roam-launcher--title :separator "━" :quit-key "q")
   ("Capture and find"
    (("c c" org-roam-capture "capture node")
     ("c d" org-roam-dailies-capture-today "capture today")
     ("f" org-roam-node-find "find")
     ("i" org-roam-node-insert "insert"))
    "Tags"
    (("t a" org-roam-tag-add "add")
     ("t r" org-roam-tag-remove "remove"))
    "Dailies"
    (("d t" org-roam-dailies-goto-today "goto today")
     ("d y" org-roam-dailies-goto-yesterday "goto yesterday"))
   ))
(global-set-key (kbd "C-c r") 'org-roam-launcher/body)

;; https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra-define
(defvar jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))

(pretty-hydra-define jp-toggles
  (:color amaranth :quit-key "q" :title jp-toggles--title)
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (;;("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p)
    ("z" zone "zone" :toggle t)
    ("F" toggle-frame-fullscreen "fullsreen" :toggle t))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
(global-set-key (kbd "C-c t") 'jp-toggles/body)

;; This is the hydra we can call upon when feeling bored, tired, understimulated, etc.
;; It will include a few activities that may be helpful (?) as stones stepping towards
;; recognition of said boredom. Maybe a 15 minute timer?
;; - Artist mode to create a new drawing in a folder
;; - 
(defhydra hydra-bored (:color blue)
   "Launch"
   ("1" (browse-url "http://www.reddit.com/r/emacs/") "reddit" :column "More focus-mana")
   ("2" org-roam-capture 1 "write" :column "More focus-mana")
   ("3" (browse-url "http://www.emacswiki.org/") "emacswiki" :column "Less focus-mana")
   ("q" nil "cancel"))
(global-set-key (kbd "M-2") 'hydra-bored/body)

(pretty-hydra-define hydra-tab-bar (:color blue :quit-key "q")
   ("Keeping tabs"
    (("b" tab-previous "Previous tab")
     ("f" tab-next "Next tab")
     ("r" tab-bar-rename-tab "Rename tab")
     ("n" tab-bar-new-tab "New tab")
     ("m" switch-buffer-to-other-tab "Switch buffer to other tab"))))
(global-set-key (kbd "C-x t") 'hydra-tab-bar/body)

;; ==========================================
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
(setq display-fill-column-indicator-character ".")
(setq fci-dash-pattern 0.25)
(setq fci-rule-use-dashes t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
;;  (setq web-mode-markup-indent-offset 2)
;;  (setq web-mode-code-indent-offset 2)
;;  (setq web-mode-css-indent-offset 2)
  ;;  (setq web-mode-comment-style 2)
  (display-fill-column-indicator-mode)
  (flymake-php-load)
  )
(add-hook 'web-mode-hook #'my-web-mode-hook)
;;(add-hook 'php-mode-hook #'flymake-php-load)
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
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))
;; (windmove-default-keybindings)
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<down>") 'windmove-down)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)

;; :: Setup hotkeys for windows scrolling other window.
;;(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
;;(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
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
