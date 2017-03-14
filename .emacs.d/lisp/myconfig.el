;;; myconfig.el --- vanilla emacs config
;;; commentary:
;;; keybinds and modes available on default emacs >= 24
;;; code:

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(defconst user user-real-login-name)

(setq use-dialog-box nil)

(menu-bar-mode -1)						; No menu bar
(setq inhibit-startup-message t)		; no startup message
(setq initial-scratch-message ";; Scratch buffer\n") ; Scratch buffer message

(setq enable-recursive-minibuffers nil)

(setq shift-select-mode t)				; handy selection with shift

(setq history-delete-duplicates t)

(setq ad-redefinition-action (quote accept))

(column-number-mode t)					; print column number
(line-number-mode t)					; print line number

(set-default 'truncate-lines t)			; truncate long lines with a $

(setq scroll-margin 10)					; pre scroll
(setq scroll-step 1)					; smooth scroll (1 line by 1)
(setq scroll-conservatively 1000)		; keep prescrolling ?
(setq hscroll-margin 20)				; pre hscroll
(setq hscroll-step 1)					; smooth hscroll (1 column by 1)

(global-auto-revert-mode t)				; auto update changed files


(electric-pair-mode)					; autopair "" {} () ...

;; TODO: delay modeline loading
(add-hook 'prog-mode-hook 'set-mode-line) ; set my mode line in the new programming buffers

(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t
				  tab-width 4
				  python-indent-offset 4)))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.myzshrc\\'" . sh-mode))

(defalias 'yes-or-no-p 'y-or-n-p)		; always (y or n) instead of (yes or no)

;;;; keybinds to functions from myfunctions.el


(global-set-key (kbd "C-c C-s") 'sudo-save)

(global-set-key (kbd "C-c C-e") 'region-execute-python)
(global-set-key (kbd "C-c s") 'region-as-python-string)

(global-set-key (kbd "C-x y") 'xpaste)	; xclipboard paste

(global-set-key (kbd "C-x n") 'switch-to-new-buffer) ; 

(global-set-key (kbd "C-c /") 'temp-buffer) ; switch between current buffer and *scratch* buffer


(global-set-key (kbd "C-c r") 'reload-dotemacs-file) ; reload emacs config

(global-set-key (kbd "C-c g") 'search-google) ; preform a google search of region/input

(global-set-key (kbd "M-Y") 'yank-pop-forward)


;;;; keybinds to emacs functions


(global-set-key (kbd "C-c C-g") 'keyboard-quit) ; quit when misstyped beggining of command
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "C-l") 'recenter)	; less messy than recenter-top-bottom

(global-set-key (kbd "C-o") 'other-window) ; faster windows switching
(global-set-key (kbd "C-x o") (rep 'other-window)) ; activate rep on default window switching

(global-set-key (kbd "C-x C-d") (lambda() "open dired ." (interactive) (dired "."))) ; emacs . for file navigation

(global-set-key (kbd "C-x 5 t") 'term) ; start terminal
(global-set-key (kbd "C-x 5 s") 'eshell) ; start elisp shell

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "M-i") (lambda () "insert tab" (interactive) (insert-tab)))
(global-set-key (kbd "<backtab>") (lambda () "insert tab" (interactive) (insert-tab)))

(global-set-key (kbd "C-x g") 'goto-line) ; goto specified line
(global-set-key (kbd "M-,") 'beginning-of-buffer) ; faster than "M-<"
(global-set-key (kbd "M-.") 'end-of-buffer)		  ; and "M->"

(global-set-key (kbd "C-c m") 'compile)

(global-set-key (kbd "C-c w") 'whitespace-cleanup-region) ; remove trailing whitespaces in region

(global-set-key (kbd "M-m") 'mark-sexp)	; mark balanced expression
(global-set-key (kbd "M-k") 'kill-sexp)	; kill balanced expression

(global-set-key (kbd "M-r") 'isearch-forward-regexp)

(global-set-key (kbd "<f12>") (lambda() (interactive) (switch-to-buffer (get-buffer-create "*scratch*")))) ; go to scratch buffer

(global-set-key (kbd "C-M-j") (lambda() (interactive) (set-mark-command 1))) ; jump to the last marked point
(global-set-key (kbd "M-RET") (lambda() (interactive) (set-mark-command 1))) ; jump to the last marked point

(global-set-key (kbd "M-'") 'repeat)	; repeat command faster

(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-c C-r") 'string-insert-rectangle)

(global-set-key (kbd "M-p") 'scroll-down-command) ; scroll-up Oo
(global-set-key (kbd "M-n") 'scroll-up-command)	  ; scroll down oO

(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)

(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-<left>") 'left-word)

(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "<ESC> <right>") 'forward-sexp)
(global-set-key (kbd "<ESC> <left>") 'backward-sexp)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "<ESC> <up>") 'move-text-up)
(global-set-key (kbd "<ESC> <down>") 'move-text-down)

(global-set-key (kbd "C-v") 'quoted-insert) ; like in the shell


;;;; resize hotkeys

(global-set-key (kbd "C-c <right>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'enlarge-window-horizontally))))
(global-set-key (kbd "C-c <left>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'shrink-window-horizontally))))
(global-set-key (kbd "C-c <down>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'enlarge-window))))
(global-set-key (kbd "C-c <up>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'shrink-window))))

(provide 'myconfig)
;;; myconfig.el ends here
