77;10103;0c;;; myconfig.el --- vanilla emacs config
;;; commentary:
;;; code:

(menu-bar-mode -1) ;; No menu bar
(setq inhibit-startup-message t) ;; no startup message
(setq initial-scratch-message ";; Scratch buffer\n") ;; Scratch buffer message

(column-number-mode t)					; print column number
(line-number-mode t)					; print line number


(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)


(setq scroll-margin 10)					; pre scroll
(setq scroll-conservatively 1000)		; keep prescrolling ?


;; resize hotkeys
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

(require 'linum)						; get line number
(add-hook 'prog-mode-hook 'global-linum-mode)
(setq linum-format "%4d \u2502 ")
(global-set-key (kbd "C-x l") 'global-linum-mode) ; toggle line number


(require 'hideshow)						; factorize functions {...}
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key [f5] 'hs-hide-all)
(global-set-key [f6] 'hs-show-all)
(global-set-key (kbd "C-x x") 'hs-toggle-hiding)



(require 'paren)						; show matching parenthese
;(show-paren-mode 1)						; ON
(setq show-paren-delay 0)				; delay


(require 'mouse)
(xterm-mouse-mode t)					; mouse on mofo
(global-set-key (kbd "<mouse-2>") 'nil)
(global-set-key (kbd "<mouse-3>") 'nil)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)



(require 'zone)							; kind of screen saver
(zone-when-idle 60)						; after 60s


; set backup dir (/tmp/emacs{uid})
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)


; this is not vi(m)
(defconst wq "You mean C-x C-c ?")
(defconst qq "You mean C-x C-c ?")
(defconst w "You mean C-x C-s ?")
(defconst q! "You mean C-x C-c ?")
(defconst wq! "You mean C-x C-c ?")
(defconst qw! "You mean C-x C-c ?")


(provide 'myconfig)
;;; myconfig.el ends here
