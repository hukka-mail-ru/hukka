
; BASIC OPTIONS
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "Cyrillic-KOI8")
 '(default-input-method "cyrillic-jcuken")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-tip-of-the-day nil)
 '(ecb-tip-of-the-day-file "off (nil)")
 '(global-font-lock-mode t nil (font-lock))
 '(scroll-all-mode nil nil (scroll-all)))

 
; COLORS
(custom-set-faces

 '(bold ((t (:height 120 :family "adobe-courier-bold"))))
 '(cursor ((t nil)))
 '(custom-button-pressed-face ((t (:background "white" :foreground "black"))))

 '(custom-comment-face ((((class grayscale color) (background dark)) (:background "green"))))
 '(custom-comment-tag-face ((((class color) (background dark)) (:foreground "green"))))
 '(font-lock-builtin-face ((t (:foreground "darkgreen" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "blue" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "black" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "blue" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "purple" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(font-lock-string-face ((t (:foreground "darkgreen"))))
 '(font-lock-type-face ((t (:foreground "black" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod" :weight bold))))
 '(fringe ((((class color) (background dark)) (:background "w"))))
 '(header-line ((((type tty)) (:foreground "green"))))
 '(isearch ((((type tty pc) (class color)) (:background "white" :foreground "black"))))
 '(isearch-lazy-highlight-face ((((type tty pc) (class color)) (:background "turquoise3" :foreground 
 "black"))))
 '(menu ((((type tty)) (:background "white" :foreground "black"))))
 '(mode-line ((t (:background "grey" :foreground "black"))))
 '(region ((t (:background "grey"))))
 '(show-paren-match-face ((((class color)) (:background "turquoise" :foreground "black"))))
 '(tool-bar ((t (:background "grey" :foreground "black"))))
 '(widget-field-face ((((type tty)) (:background "yellow3" :foreground "black"))))
 '(widget-inactive-face ((((class grayscale color) (background light)) (:foreground "grey")))))






;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;


 (defun copy-to-register-5 () 
  (interactive)
  (copy-to-register 5 (mark) (point)))

(defun insert-register-5 () 
  (interactive)
  (insert-register 5))

;; THIN CURSOR  (packet "emacs-goodies-el" needed)
(bar-cursor-mode 't)

;; ETAGS DIRECTORY
(setq tags-table-list (list (getenv "SRCROOT")))

;; PHP MODE
(load-file "~/emacs/php-mode.el")


;; SHOW DIFFERENCE AS 2 WINDOWS: LEFT AND RIGHT
(setq ediff-split-window-function 'split-window-horizontally)

;; DON'T CREATE BACKUP FILES
(setq make-backup-files nil)

;; ENABLE MOUSE WHEEL
(mouse-wheel-mode 1)


;; SMOOTH SCROLLING
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

;; MATCH LEFT AND RIGTH PARENTHESIS
(show-paren-mode)
 
; TEXT SELECTION
(transient-mark-mode 't)

;; Shift-arrows a la windows...
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))


;; INDENTS STYLE
(add-hook 'c-mode-common-hook
(function
(lambda ()
(c-set-style "stroustrup")
(c-set-offset 'inline-open ' 0)
(c-set-offset 'block-open ' -)
(c-set-offset 'innamespace ' -)
(setq indent-tabs-mode nil)
(setq tab-width 4))))


;; Shell script mode for .conf
;;(setq auto-mode-alist (cons '("\\.conf$" . apache-mode)))
(add-to-list 'auto-mode-alist '("\\.conf" . sgml-mode))


;; ;; ;;;;;;;; HOTKEYS ;;;;;;;;;;;;;;;;;;;;;;;

 (global-set-key (kbd "<f36>") 'copy-to-register-5) ;; means Ctrl+F12
 (global-set-key (kbd "<f24>") 'insert-register-5)  ;; means Shift+F12


;; EXIT
(global-set-key (kbd "\C-x") 'save-buffers-kill-emacs)

;; SAVE FILE
(global-set-key (kbd "<f2>") 'save-buffer)

;; OPEN FILE
(global-set-key (kbd "<f3>") 'find-file)

;; COMMENT REGION
(global-set-key (kbd "<f7>") 'comment-region)

;; UNCOMMENT REGION
(global-set-key (kbd "<f8>") 'uncomment-region)

; SWITCH BUFFERS
  (global-set-key (kbd "\C-b") 'buffer-menu)

;; OPEN .EMACS 
  (defun Open-conf ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs")))
  (global-set-key (kbd "\C-e") 'Open-conf)

;; OPEN TEMPLATE FILE
  (defun Open-template ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/template/template.cpp")))
  (global-set-key (kbd "\C-t") 'Open-template)



;; COMPILATION
   (setq compilation-scroll-output 1)

   (global-set-key (kbd "<f9>") 'user-save-and-make-all)
   (global-set-key (kbd "<f11>") 'user-make-clean)


    (defun user-save-and-make-all ()
     "save and call compile as make all"
     (interactive)
     (save-buffer)
     (compile "make all")
     (message "make all executed!"))

     (defun user-make-clean ()
     "save and call compile as make all"
     (interactive)
     (compile "make clean")
     (message "make clean executed!"))


										      
;; GOTO LINE NUMBER...
(global-set-key (kbd "\C-l") 'goto-line)
(global-set-key (kbd "\e\el") 'goto-line)

;; UNDO Ctrl+Z
(global-set-key (kbd "\C-z") 'undo)

;; GOTO MINIBUFER
(global-set-key (kbd "\e\ex") 'execute-extended-command)
(global-set-key [C-return] 'execute-extended-command)


;; Crl+Y erase line
(global-set-key (kbd "\C-y") 'kill-line)

;; Select all
(global-set-key (kbd "\C-a") 'mark-whole-buffer)

;; CUT
(global-set-key [C-delete] 'kill-region)


;; HISTORY NAVIGATION Ctrl+Shift+ <- ->
(global-set-key [C-S-left] 'ecb-nav-goto-previous)
(global-set-key [C-S-right] 'ecb-nav-goto-next)

;; Depending on the system any of the following could be home.
(global-set-key [home] 'beginning-of-line)  ;;; Home = Beginning of line
(global-set-key [kp-home] 'beginning-of-line);; Home = Beginning of line
(global-set-key "\e[1~" 'beginning-of-line) ;;; Home = Beginning of line

;; Depending on the system any of the following could be end
(global-set-key [kp-end] 'end-of-line)      ;;; End = End of line
(global-set-key [end] 'end-of-line)         ;;; End = End of line
(global-set-key [select] 'end-of-line)      ;;; End = End of line

;; TAB (only in c++ because tab makes the autocompletion in shell and minibuffer)
  (defun my-c-mode-hook ()
  (local-set-key [tab] 'c-indent-line-or-region)
  (setq indent-tabs-mode nil))
  (add-hook 'c++-mode-hook 'my-c-mode-hook )

;; TAB in XML
  (defun my-sgml-mode-hook ()
  (local-set-key [tab] 'self-insert-command))
  (add-hook 'sgml-mode-hook 'my-sgml-mode-hook )


;;============ ECB, ETAGS ============================

;; ECB
;;(add-to-list 'load-path "c:/Programme/Emacs/ecb-2.32/") 
;;(require 'ecb)
;;(global-set-key (kbd "\e\et") 'ecb-toggle-ecb-windows)
;;(global-set-key (kbd "\e\ee") 'ecb-activate)
;;(global-set-key (kbd "\e\ed") 'ecb-deactivate)

;; Cedet
;;(setq semantic-load-turn-useful-things-on t)
;;(load-file "/usr/share/emacs/site-lisp/cedet-common/cedet.el")
;;(global-set-key (kbd "\e\ec") 'semantic-ia-complete-symbol)
;; don't drop shit
(setq semanticdb-default-save-directory "~/emacs") 

;; ETAGS
;;(global-set-key (kbd "\e\ec") 'complete-symbol)
;;(global-set-key (kbd "\e\ev") 'visit-tags-table)

;; session
;;(require 'session)

;; FIND ALL TAGS in project
(global-set-key (kbd "<f1>") 'tags-apropos)


;; ============= BUFFERS  ========================

;; Close buffer - F12
(defun My-kill-buffer ()
(interactive)
(kill-buffer (current-buffer)))
(global-set-key [f12]            'My-kill-buffer)

;;
(defun My-buffer-up ()
(interactive)
(other-window -1))
;;
(defun My-buffer-down ()
(interactive)
(other-window 1))
;;
(global-set-key [f6]     'other-window)
(global-set-key [C-a]   'other-window)


;; ONLY ONE WINDOW
(global-set-key [f5]     'delete-other-windows)


;;========== ESHELL ==================

;; REMAIN ONLY SHELL
;;
(defun View-only-shell ()
(interactive)
(delete-other-windows)
(eshell))

;; VIEW ESHELL
(global-set-key [f4] 'View-only-shell)

(global-set-key "\C-f" 'isearch-repeat-forward) 

;; ESHELL HISTORY LIKE IN ZSH
(defun my-eshell-mode-hook ()
  (local-set-key [up] 'eshell-previous-input)
  (local-set-key [down] 'eshell-next-input)
  )
(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)



