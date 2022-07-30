(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-indent-nested-block-content-align nil)
;; (setq lua-indent-level 2)
;; (setq lua-electric-flag t)
;; (setq lua-indent-nested-block-content-align t)
;; (defun lua-abbrev-mode-off () (abbrev-mode t))
;; (add-hook 'lua-mode-hook 'lua-abbrev-mode-off)
;; (setq save-abbrevs nil)   ;; is this still needed?
