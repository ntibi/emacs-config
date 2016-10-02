;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:

(use-package flycheck
  :ensure t
  :defer 2
  :bind (("C-x `" . flycheck-next-error))
  :init
  (use-package flycheck-clangcheck
	:ensure t
	:defer t
	:config
	(setq flycheck-clangcheck-analyze t)
	(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))) ; --std=c++11
	)
  :config
  (global-flycheck-mode) ; flycheck ON
  (setq flycheck-clang-include-path
		(quote ("/usr/include/" "/usr/local/include/" "/usr/include/c++/4.9/" "/data/include/" "./" "./include" "./includes" "../include" "../includes")))
  (setq flycheck-cppcheck-include-path
		(quote ("/usr/include/" "/usr/local/include/" "/usr/include/c++/4.9/" "/data/include/" "./" "./include" "./includes" "../include" "../includes")))
  (setq flycheck-gcc-definitions
		(quote ("./" "./includes/" "../includes/" "./include/" "../include/")))
  (setq flycheck-temp-prefix ".flycheck")
  )

(use-package company					; company auto complete
  :ensure t
  :defer t
  :bind (("M-/" . company-complete))
  :init
  (use-package company-c-headers
	:defer t
	:ensure t
	:config
	(setq company-c-headers-path-system
		  (quote ("/data/include/" "/usr/include/" "/usr/local/include/" "/usr/include/c++/4.9/")))
	(setq company-c-headers-path-user
		  (quote ("./" "./includes/" "../includes/" "./include/" "../include/")))
	)
  :config
  (global-company-mode)
  (company-semantic 1)							 ; company with semantic backend
  (define-key company-active-map (kbd "M-.") 'company-show-doc-buffer) ; show doc
  (define-key company-active-map (kbd "M-,") 'company-show-location) ; show source
  (add-to-list 'completion-styles 'emacs22)			  ; completion from buffer(before point) words
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'initials)		  ; initials auto complete
  (add-to-list 'completion-styles 'semantic)
  (add-to-list 'company-backends 'company-c-headers)	  ; headers auto completion
  ;; (set
   ;; 'company-clang-arguments
   ;; (list
	;; (concat "-I" (file-name-directory load-file-name) "./")
	;; (concat "-I" (file-name-directory load-file-name) "./includes/")
	;; (concat "-I" (file-name-directory load-file-name) "../includes/")
	;; )
   ;; )
  )

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )


(use-package yasnippet							 ; yet another snippet
  :ensure t
  :defer t
  :bind (("C-c TAB" . yas-expand))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; snippets path
  (yas-global-mode t)
  (defvar company-mode/enable-yas t)				 ; snippets completion in company
  (defun company-mode/backend-with-yas (backend)
	(if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		backend
	  (append (if (consp backend) backend (list backend))
			  '(:with company-yasnippet))))
  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)) ; i don't want yasnippet suggestions
  )

(use-package helm						; better minibuffer selection
  :defer 2
  :ensure t
  :bind (
		 ("M-x"		.	helm-M-x)
		 ("C-x C-f"	.	helm-find-files)
		 ("C-x C-b"	.	helm-buffers-list)
		 ("C-x p"	.	helm-show-kill-ring)
		 ("C-?"		.	helm-semantic-or-imenu)
		 )
  :init
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)				; shrink minibuffer if possible
  (setq helm-buffers-fuzzy-matching t)
  )

(use-package helm-swoop
  :defer t
  :ensure t
  :bind (("C-M-s" . helm-swoop))
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
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  )

(use-package rainbow-identifiers			; different variables color
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  )

(use-package highlight-parentheses		; highlight surrounding parentheses
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  :config
  (setq hl-paren-background-colors nil)
  (setq hl-paren-delay 0.01)
  )

(use-package rainbow-delimiters			; parentheses color according to depth
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (setq rainbow-identifiers-face-count 15)
  )

(use-package neotree						; neo tree (files browsing tree)
  :ensure t
  :defer t
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
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ; undo with the fancy tree
  (define-key undo-tree-map (kbd "C--") 'undo-tree-undo) ; normal undo
  (global-undo-tree-mode)
  )

(use-package tabbar						; tabbar mode (tabs navigation)
  :ensure t
  :init
  (use-package tab-group					; organize tabs in groups
	:ensure t)
  :config
  (tabbar-mode t)							; ON
  (load "tabbar-tweek.el")				; nice tabbar config
  (global-set-key (kbd "<end>") 'tabbar-backward-tab)
  (global-set-key (kbd "<home>") 'tabbar-forward-tab)
  (global-set-key (kbd "C-<end>") 'tabbar-backward-group)
  (global-set-key (kbd "C-<home>") 'tabbar-forward-group)
  (setq tabbar-use-images nil)			; faster ?
  (setq tabbar-separator (quote (0.5)))
  )

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (
		 ("M-f" . ace-jump-word-mode)
		 ("C-]" . jump-char-forward)
		 )
  :config
  (setq ace-jump-mode-case-fold t)				 ; case insensitive
  (setq ace-jump-mode-move-keys (cl-loop for i from ?a to ?z collect i)) ; use [a-z]
  )


(use-package multiple-cursors			; multiple cursors
  :ensure t
  :defer t
  :bind (
		 ("<C-down-mouse-1>" . mc/add-cursor-on-click) ; ctrl clic to add cursor
		 ("C-x m" . mc/edit-lines)		; spawn a cursor on each line
		 )
  )

(use-package magit						; magic git (git client)
  :ensure t
  :defer t
  :bind (("C-x v s" . magit-status))
  )

(use-package git-gutter					; get git diff-like characters in the margin
  :ensure t
  :defer t
  :bind (("C-x v g" . git-gutter-mode))
  :config
  (git-gutter:linum-setup)				; linum compatibility
  (set-face-background 'git-gutter:added nil)
  (set-face-background 'git-gutter:modified nil)
  (set-face-background 'git-gutter:deleted nil)
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:deleted "red")
  )

(provide 'myconfig)
;;; packages-config.el ends here
