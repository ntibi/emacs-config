;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:


; installed packages
  ;; anaconda-mode
  ;; company
  ;; company-anaconda
  ;; company-c-headers
  ;; company-irony
  ;; cyberpunk-theme
  ;; dash
  ;; elpy
  ;; epl
  ;; f
  ;; find-file-in-pr
  ;; flycheck
  ;; flycheck-clangc
  ;; flycheck-irony
  ;; flycheck-ocaml
  ;; flycheck-pos-tip
  ;; flycheck-tip
  ;; flymake-easy
  ;; flymake-python
  ;; flymake-shell
  ;; function-args
  ;; fuzzy
  ;; highlight-inden
  ;; highlight-paren
  ;; highlight-thing
  ;; irony
  ;; json-rpc
  ;; let-alist
  ;; linum-relative
  ;; merlin
  ;; monokai-theme
  ;; mouse
  ;; multiple-cursors
  ;; neotree
  ;; pastels-on-dark
  ;; pkg-info
  ;; popup
  ;; pyvenv
  ;; rainbow-delimiters
  ;; rainbow-identif
  ;; rainbow-mode
  ;; s
  ;; strings
  ;; swiper
  ;; tab-group
  ;; tabbar
  ;; undo-tree
  ;; yasnippet
  ;; zenburn-theme

(require 'flycheck)								   ; flycheck
(require 'flycheck-clangcheck)					   ; clangcheck
(add-hook 'after-init-hook #'global-flycheck-mode) ; flycheck ON
(set 'flycheck-clang-include-path (list "./" "./includes/" "../includes/" "./include/" "../include/" "/nfs/zfs-student-5/users/2014/ntibi/.brew/Cellar/boost/1.58.0/include/"))
(setq flycheck-clangcheck-analyze t)
(setq flycheck-check-syntax-automatically '(mode-enabled save)) ; check at save
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))) ; --std=c++11
;; (add-hook 'c-mode-common-hook  (lambda () DO STUFF ))

(require 'cc-mode)
(require 'semantic)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)	; update DB wen idle
(semantic-add-system-include "/nfs/zfs-student-5/users/2014/ntibi/.brew/Cellar/boost/1.58.0/include/")
(global-set-key (kbd "C-x j") 'semantic-complete-jump) ; jump to local symbol


(require 'company)						; company auto complete
(add-hook 'after-init-hook 'global-company-mode) ; company auto-compete ON
(company-semantic 1)							 ; company with semantic backend
(global-set-key (kbd "M-/") 'company-complete)	  ; launch ac
(global-set-key (kbd "M-.") 'company-show-doc-buffer) ; show doc
(global-set-key (kbd "M-,") 'company-show-location)	  ; show source
(add-to-list 'completion-styles 'initials t)		  ; initials auto complete
(add-to-list 'completion-styles 'semantic)
(add-to-list 'company-backends 'company-c-headers)	  ; headers auto completion


(require 'function-args)
(fa-config-default)


(require 'company-clang)
;; (set 'company-clang-arguments (list (concat "-I" (file-name-directory load-file-name) "./") (concat "-I" (file-name-directory load-file-name) "/includes/") (concat "-I" (file-name-directory load-file-name) "../includes/")))

;; (require 'company-c-headers)
;; (set 'company-c-headers-path-user (list "./" "./includes/" "../includes/"))


(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook (lambda () "" (interactive) (add-to-list 'company-backends 'company-anaconda)))
(add-hook 'python-mode-hook (lambda () "" (interactive) (pyenv-mode)))


(require 'yasnippet)							 ; yet another snippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; snippets path
(add-hook 'after-init-hook (lambda () "" (interactive) (yas-global-mode 1))) ; enable yas
(defvar company-mode/enable-yas t)				 ; snippets completion in company
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


(require 'helm)
(helm-mode 1)
(eval-after-load 'company
  '(progn
	 (define-key company-mode-map (kbd "C-/") 'helm-company)
	      (define-key company-active-map (kbd "C-/") 'helm-company)))
(define-key global-map [remap find-file] 'helm-find-files) ; helm custom find-file
(define-key global-map [remap occur] 'helm-occur) ; use helm occur (dunno what occur is)
(define-key global-map [remap list-buffers] 'helm-buffers-list) ; use helm buffer-list
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev) ; use helm dabbrev
(global-set-key (kbd "M-x") 'helm-M-x)	; use custom minibuffer
(helm-autoresize-mode 1)				; shrink minibuffer if possible


(require 'highlight-thing)				; highlight current line/word
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(setq highlight-thing-what-thing 'word)	; underline word
(setq highlight-thing-delay-seconds 0.1)
(custom-set-faces '(hi-yellow ((t (:underline t)))))


(require 'rainbow-mode)					; colorize hex codes
(add-hook 'prog-mode-hook 'rainbow-mode)
(require 'rainbow-identifiers)			; different variables color
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(require 'highlight-parentheses)		; highlight surrounding parentheses
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(require 'rainbow-delimiters)			; parentheses color according to depth
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(require 'neotree)						; neo tree
(global-set-key (kbd "C-x a a") 'neotree-toggle) ; open neo tree
(global-set-key [f8] 'neotree-toggle)			 ; same


(require 'undo-tree)					; undo tree
(global-undo-tree-mode)					; set undo-tree as default undo (C-x u)


(require 'tabbar)						; tabbar mode
(require 'tab-group)					; organize tabs in groups
(tabbar-mode t)							; ON
(load "tabbar-tweek.el")				; nice tabbar config
(global-set-key (kbd "C-x <left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x p") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <down>") 'tabbar-backward-group)
(global-set-key (kbd "C-x <up>") 'tabbar-forward-group)
(setq tabbar-use-images nil)			; faster ?



(require 'multiple-cursors)				; multiple cursors
(global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click) ; ctrl clic to add cursor


(provide 'myconfig)
;;; packages-config.el ends here
