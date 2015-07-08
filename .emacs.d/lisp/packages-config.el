;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:


; installed packages
  ;; auto-complete
  ;; caml
  ;; company
  ;; cyberpunk-theme
  ;; dash
  ;; epl
  ;; firecode-theme
  ;; flycheck
  ;; flycheck-clangc...
  ;; flycheck-ocaml
  ;; flymake-easy
  ;; flymake-python-...
  ;; flymake-shell
  ;; highlight-paren...
  ;; highlight-thing
  ;; let-alist
  ;; linum-relative
  ;; merlin
  ;; molokai-theme
  ;; monokai-theme
  ;; mouse+
  ;; multiple-cursors
  ;; neotree
  ;; pastels-on-dark...
  ;; pkg-info
  ;; popup
  ;; popup-complete
  ;; rainbow-delimiters
  ;; rainbow-identif...
  ;; rainbow-mode
  ;; strings
  ;; tab-group
  ;; tabbar
  ;; tabbar-ruler
  ;; tuareg
  ;; undo-tree
  ;; yasnippet
  ;; zenburn-theme


(load-theme 'monokai t)					; default theme

(require 'flycheck)								   ; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode) ; flycheck ON


(require 'company)						; company auto complete
(add-hook 'after-init-hook 'global-company-mode) ; company auto-compete ON
(add-to-list 'company-backends 'company-anaconda) ; anaconda mode for python ac
(add-hook 'python-mode-hook 'anaconda-mode)		  ; python auto complete
(global-set-key (kbd "M-/") 'company-complete)	  ; launch ac
(global-set-key (kbd "M-.") 'company-show-doc-buffer) ;show doc
(global-set-key (kbd "M-,") 'company-show-location)	  ; show source
(add-to-list 'completion-styles 'initials t)		  ; initials auto complete
(add-to-list 'company-backends 'company-c-headers)	  ; headers auto completion


(require 'yasnippet)							 ; yet another snippet
(add-hook 'prog-mode-hook #'yas-minor-mode)		 ; enable
(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; snippets path
(yas-global-mode 1)								 ; enable yas
(defvar company-mode/enable-yas t)				 ; snippets completion in company
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))



(require 'highlight-thing)				; highlight current line/word

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
