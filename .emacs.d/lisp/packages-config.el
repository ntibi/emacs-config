;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:


; installed packages

  ;; caml
  ;; company
  ;; cyberpunk-theme
  ;; dash
  ;; elpy
  ;; epl
  ;; find-file-in-pr...
  ;; flycheck
  ;; flycheck-clangc...
  ;; flycheck-ocaml
  ;; flycheck-pos-tip
  ;; flycheck-tip
  ;; flymake-easy
  ;; flymake-python-...
  ;; flymake-shell
  ;; fuzzy
  ;; highlight-inden...
  ;; highlight-paren...
  ;; highlight-thing
  ;; let-alist
  ;; linum-relative
  ;; merlin
  ;; monokai-theme
  ;; mouse+
  ;; multiple-cursors
  ;; neotree
  ;; pastels-on-dark...
  ;; pkg-info
  ;; popup
  ;; pyvenv
  ;; rainbow-delimiters
  ;; rainbow-identif...
  ;; rainbow-mode
  ;; strings
  ;; tab-group
  ;; tabbar
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



(require 'rainbow-identifiers)			; different variables color
(require 'rainbow-mode)					; colorize hex codes
(require 'rainbow-delimiters)			; parentheses color according to depth
(require 'highlight-parentheses)		; highlight surrounding parentheses
(require 'highlight-thing)				; highlight current line/word

(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
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
