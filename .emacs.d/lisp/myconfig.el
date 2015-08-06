;;; myconfig.el --- vanilla emacs config
;;; commentary:
;;; keybinds and modes available on default emacs >= 24
;;; code:

(load "myfunctions.el")

;; config

;; set backup dir (/tmp/emacs{uid})
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(menu-bar-mode -1) ;; No menu bar
(setq inhibit-startup-message t) ;; no startup message
(setq initial-scratch-message ";; Scratch buffer\n") ;; Scratch buffer message

(column-number-mode t)					; print column number
(line-number-mode t)					; print line number

(display-time-mode 1)					; display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq scroll-margin 10)					; pre scroll
(setq scroll-conservatively 1000)		; keep prescrolling ?

(global-auto-revert-mode t)				; auto update changed files

(defalias 'yes-or-no-p 'y-or-n-p)		; always (y or n) instead of (yes or no)

;; keybinds to functions from myfunctions.el
(global-set-key (kbd "C-c p") 'region-execute-python)
(global-set-key (kbd "C-c s") 'region-as-python-string)

(global-set-key (kbd "C-c d") 'duplicate-line-or-region) ; explicit name ;)
(global-set-key (kbd "C-c c") 'copy-line)		 ; copy line
(global-set-key (kbd "C-c k") 'kill-whole-line)	; kill whole line

(global-set-key (kbd "C-q") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-c r") 'reload-dotemacs-file) ; reload emacs config

(global-set-key (kbd "C-c C-g") 'search-google) ; preform a google search of region/input

;; keybinds to emacs functions
(global-set-key (kbd "C-o") 'other-window) ; faster windows switching

(global-set-key (kbd "C-x C-d") (lambda() "open dired ." (interactive) (dired "."))) ; emacs . for file navigation

(global-set-key (kbd "C-c C-t") 'eshell) ; start terminal
(global-set-key (kbd "C-c C-s") 'eshell) ; start shell

(global-set-key (kbd "M-i") (lambda () "insert tab" (interactive) (insert-tab)))

(global-set-key (kbd "C-x g") 'goto-line)

(global-set-key (kbd "C-c m") 'compile)

(global-set-key (kbd "C-c e") 'whitespace-mode) ; 'cat -e' like
(global-set-key (kbd "C-c w") 'whitespace-cleanup-region) ; remove trailing whitespaces in region

(global-set-key (kbd "M-m") 'mark-sexp)	; mark balanced expression

;; resize hotkeys
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

(global-set-key (kbd "M-2") 'enlarge-window)
(global-set-key (kbd "M-8") 'shrink-window)
(global-set-key (kbd "M-6") 'enlarge-window-horizontally)
(global-set-key (kbd "M-4") 'shrink-window-horizontally)

;; configs and keybinds from modes
(require 'cc-mode)
(require 'semantic)
(require 'semantic/ia)
(global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-mode 1)						 ;
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)	; update DB wen idle
(semantic-add-system-include "/nfs/zfs-student-5/users/2014/ntibi/.brew/Cellar/boost/1.58.0/include/")
(global-set-key (kbd "C-x j") 'semantic-complete-jump) ; jump to local symbol
(global-set-key (kbd "C-c j") 'senator-go-to-up-reference) ; jump to definition
(global-semantic-show-parser-state-mode)
;; (global-semantic-highlight-edits-mode)
(global-semantic-mru-bookmark-mode)

(require 'linum)						; get line number
(global-linum-mode)
(setq linum-format "%4d \u2502 ")


(require 'hideshow)						; factorize functions {...}
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key [f5] 'hs-hide-all)
(global-set-key [f6] 'hs-show-all)
(global-set-key (kbd "C-x x") 'hs-toggle-hiding)
(global-set-key (kbd "C-v") 'hs-toggle-hiding)


(require 'paren)						; show matching parenthese
(show-paren-mode 1)						; ON
(setq show-paren-delay 0)				; delay


(require 'mouse)
(xterm-mouse-mode t)					; mouse on mofo
(global-set-key (kbd "<mouse-2>") 'nil)
(global-set-key (kbd "<mouse-3>") 'nil)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)


;; (require 'zone)							; kind of screen saver
;; (zone-when-idle 60)						; after 60s


; this is not vi(m)
(defconst wq "You mean C-x C-c ?")
(defconst qq "You mean C-x C-c ?")
(defconst w "You mean C-x C-s ?")
(defconst q! "You mean C-x C-c ?")
(defconst wq! "You mean C-x C-c ?")
(defconst qw! "You mean C-x C-c ?")

(provide 'myconfig)
;;; myconfig.el ends here
