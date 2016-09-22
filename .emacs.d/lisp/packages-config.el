;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:

(use-package flycheck
  :ensure t
  :defer 1
  :config
  (use-package flycheck-clangcheck :ensure t :defer t)
  (global-flycheck-mode) ; flycheck ON
  (setq flycheck-clangcheck-analyze t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)) ; check at save
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))) ; --std=c++11
  )

(use-package company					; company auto complete
  :ensure t
  :bind (("M-/" . company-complete))
  :config
  (use-package company-c-headers :defer t :ensure t)
  (add-hook 'after-init-hook 'global-company-mode) ; company auto-compete ON
  (global-company-mode)
  (company-semantic 1)							 ; company with semantic backend
  (define-key company-active-map (kbd "M-.") 'company-show-doc-buffer) ; show doc
  (define-key company-active-map (kbd "M-,") 'company-show-location) ; show source
  (add-to-list 'completion-styles 'emacs22)			  ; completion from buffer(before point) words
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'initials)		  ; initials auto complete
  (add-to-list 'completion-styles 'semantic)
  (add-to-list 'company-backends 'company-c-headers)	  ; headers auto completion
  (set 'company-clang-arguments (list (concat "-I" (file-name-directory load-file-name) "./") (concat "-I" (file-name-directory load-file-name) "/includes/") (concat "-I" (file-name-directory load-file-name) "../includes/")))
  )

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook (lambda () "" (interactive) (add-to-list 'company-backends 'company-anaconda)))
(add-hook 'python-mode-hook (lambda () "" (interactive) (pyenv-mode)))

(use-package yasnippet							 ; yet another snippet
  :ensure t
  :defer 1
  :config
  (yas-global-mode 1) ; enable yas
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; snippets path
  (defvar company-mode/enable-yas t)				 ; snippets completion in company
  (defun company-mode/backend-with-yas (backend)
	(if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		backend
	  (append (if (consp backend) backend (list backend))
			  '(:with company-yasnippet))))
  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)) ; i don't want yasnippet suggestions
  )

(use-package helm
  :defer 2
  :ensure t
  :bind (
		 ("M-x"		.	helm-M-x)
		 ("C-x C-f"	.	helm-find-files)
		 ("C-x C-b"	.	helm-buffers-list)
		 ("C-x p"	.	helm-show-kill-ring)
		 )
  :init ()
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)				; shrink minibuffer if possible
  )


(use-package find-file-in-project		; find a file anywhere in the project
  :ensure t
  :bind (("C-x f" . find-file-in-project))
  :config (setq ffip-project-file ".git")
  )


(use-package highlight-thing			; highlight current line/word
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  :config
  (setq highlight-thing-what-thing 'word)	; underline word
  (setq highlight-thing-delay-seconds 0.1)
  (custom-set-faces '(hi-yellow ((t (:underline t)))))
)

(use-package rainbow-mode					; colorize hex codes
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode)
  )
(use-package rainbow-identifiers			; different variables color
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  )
(use-package highlight-parentheses		; highlight surrounding parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  )
(use-package rainbow-delimiters			; parentheses color according to depth
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package neotree						; neo tree
  :ensure t
  :bind (
		 ("C-c a"	.	neotree-toggle)
		 ([f8]		.	neotree-toggle)
		 )
  :config
  (setq neo-smart-open t)
  (define-key neotree-mode-map (kbd "<home>") (neotree-make-executor :file-fn 'neo-open-file :dir-fn  'neo-open-dir))
  (define-key neotree-mode-map (kbd "<end>") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "C-@") 'neotree-change-root)
  )

(use-package undo-tree					; undo tree
  :ensure t
  :config
  (global-undo-tree-mode)					; set undo-tree as default undo (C-x u)
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ; undo with the fancy tree
  (define-key undo-tree-map (kbd "C--") 'undo-tree-undo) ; normal undo
  )

(use-package tabbar						; tabbar mode
  :ensure t
  :config
  (use-package tab-group					; organize tabs in groups
	:ensure t
	)
  (tabbar-mode t)							; ON
  (load "tabbar-tweek.el")				; nice tabbar config
  (global-set-key (kbd "<end>") 'tabbar-backward-tab)
  (global-set-key (kbd "<home>") 'tabbar-forward-tab)
  (global-set-key (kbd "C-<end>") 'tabbar-backward-group)
  (global-set-key (kbd "C-<home>") 'tabbar-forward-group)
  (setq tabbar-use-images nil)			; faster ?
  )

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "M-f") 'ace-jump-word-mode) ; quickly jump to a word
  (global-set-key (kbd "C-]") 'jump-char-forward)
  (setq ace-jump-mode-case-fold t)				 ; case insensitive
  (setq ace-jump-mode-move-keys (cl-loop for i from ?a to ?z collect i)) ; use [a-z]
  )


(use-package multiple-cursors			; multiple cursors
  :ensure t
  :bind (
		 ("<C-down-mouse-1>" . mc/add-cursor-on-click) ; ctrl clic to add cursor
		 ("C-x m" . mc/edit-lines)		; spawn a cursor on each line
		 )
  )

(provide 'myconfig)
;;; packages-config.el ends here
