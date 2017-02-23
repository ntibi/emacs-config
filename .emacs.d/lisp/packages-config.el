;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (setq evil-move-beyond-eol t)			; allows access to the \n
  (setq-default evil-cross-lines t)

  (define-key evil-normal-state-map "\C-z" 'suspend-frame)
  (define-key evil-normal-state-map "u" 'undo-tree-undo)
  (define-key evil-normal-state-map "U" 'undo-tree-redo)
  (define-key evil-normal-state-map "\C-U" 'undo-tree-visualize)
  (define-key evil-normal-state-map "a" 'move-beginning-of-line)
  (define-key evil-normal-state-map "e" 'move-end-of-line)
  (define-key evil-normal-state-map "F" 'evil-ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "<left>") 'tabbar-backward-tab)
  (define-key evil-normal-state-map (kbd "<right>") 'tabbar-forward-tab)
  (define-key evil-normal-state-map (kbd "<up>") 'tabbar-forward-group)
  (define-key evil-normal-state-map (kbd "<down>") 'tabbar-backward-group)

  (define-key evil-normal-state-map "J" 'tabbar-backward-tab)
  (define-key evil-normal-state-map "K" 'tabbar-forward-tab)
  (define-key evil-normal-state-map "gT" 'tabbar-backward-tab)
  (define-key evil-normal-state-map "gt" 'tabbar-forward-tab)
  (define-key evil-normal-state-map ";" 'comment-line)
  (define-key evil-normal-state-map "zf" 'evil-toggle-fold)
  (define-key evil-normal-state-map "`" 'flycheck-next-error)
  
  (define-key evil-visual-state-map "a" 'move-beginning-of-line)
  (define-key evil-visual-state-map "e" 'move-end-of-line)
  (define-key evil-visual-state-map "F" 'evil-ace-jump-word-mode)

  (define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)

  (define-key evil-motion-state-map [down-mouse-1] nil)

  (evil-ex-define-cmd "rln" 'nlinum-relative-toggle)
  (evil-ex-define-cmd "ln" 'nlinum-mode)
  (evil-ex-define-cmd "ele" 'c-toggle-electric-state)
  (evil-ex-define-cmd "nl" 'c-toggle-auto-newline)
  (evil-ex-define-cmd "hun" 'c-toggle-hungry-state)
  (evil-ex-define-cmd "tr" 'toggle-truncate-lines)
  (evil-ex-define-cmd "ro" 'read-only-mode)
  (evil-ex-define-cmd "ws" 'whitespace-mode)
  (evil-ex-define-cmd "xt" 'xterm-mouse-mode)
  (evil-ex-define-cmd "fc" 'flycheck-mode)

  )

(use-package vimish-fold
  :ensure t
  :config
  (vimish-fold-mode 1)
  )

(use-package evil-mc
  :ensure t
  :init
  (define-key evil-normal-state-map "gcc" 'evil-mc-make-all-cursors)
  (define-key evil-normal-state-map "gcr" 'evil-mc-undo-all-cursors)
  (define-key evil-normal-state-map "gch" 'evil-mc-make-cursor-here)
  (define-key evil-normal-state-map "gcp" 'evil-mc-pause-cursors)
  (define-key evil-normal-state-map "gcm" 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map "gcs" 'evil-mc-skip-and-goto-next-match)
  :config
  (global-evil-mc-mode)
  )

(use-package semantic
  :defer 2
  :bind (
		 ("C-x j" . semantic-complete-jump) ; jump to local symbol
		 ("C-c f" . semantic-symref)
		 )
  :config
  (use-package function-args			; get function args when idle
	:ensure t
	:defer t
	:init
	(add-hook 'prog-mode-hook 'function-args-mode)
	:config
	(fa-config-default)
	)
  (use-package semantic/ia)
  (use-package semantic/senator :bind (("C-c j" . senator-go-to-up-reference)))
  (global-ede-mode 1)                      ; Enable the Project management system
  (semantic-mode 1)						 ;
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)	; update DB wen idle
  (global-semantic-show-parser-state-mode)
  (global-semantic-mru-bookmark-mode)
  (global-semantic-highlight-func-mode)
  (global-semantic-idle-summary-mode 1)	; get info on thing at point when idle
  (semantic-add-system-include "/data/include" 'c++-mode)
  (semantic-add-system-include "/data/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/local/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/c++/4.9/" 'c++-mode)
  ;; (setq semantic-default-c-path
  ;; (quote ("./" "../include" "../includes" "./include" "./includes")))
  (setq semantic-default-c-path
		(list
		 (concat emacs-wd "./")
		 (concat emacs-wd "./include/")
		 (concat emacs-wd "./includes/")
		 (concat emacs-wd "../include/")
		 (concat emacs-wd "../includes/")
		 ))
  )

(use-package nlinum-relative			; get line number
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-format "%4d \u2502 ")
  (setq nlinum-relative-offset 1)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-redisplay-delay 0.05)
  )

(use-package hideshow					; factorize functions {...}
  :bind (
		 ([f5] . hs-hide-all)
		 ([f6] . hs-show-all)
		 ("C-x x" . hs-toggle-hiding)
		 ("M-v" . hs-toggle-hiding)
		 )
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
)

(use-package paren						; show matching parenthese
  :defer 2
  :config
  (show-paren-mode 1)			; ON
  (setq show-paren-delay 0)		; delay
  )

(use-package mouse
  :config
  (xterm-mouse-mode t)					; mouse on mofo
  (global-set-key (kbd "<mouse-2>") 'nil)
  (global-set-key (kbd "<mouse-3>") 'xpaste) ; right clic to paste from xclipboard
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(use-package org-mode
  :defer t
  )

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
  ;; (company-semantic 1)							 ; company with semantic backend
  (set
   'company-clang-arguments
   (list
	(concat "-I" emacs-wd "./")
	(concat "-I" emacs-wd "./include/")
	(concat "-I" emacs-wd "./includes/")
	(concat "-I" emacs-wd "../include/")
	(concat "-I" emacs-wd "../includes/")
	)
   )
  (define-key company-active-map (kbd "M-.") 'company-show-doc-buffer) ; show doc
  (define-key company-active-map (kbd "M-,") 'company-show-location) ; show source
  (add-to-list 'completion-styles 'emacs22)			  ; completion from buffer(before point) words
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'initials)		  ; initials auto complete
  (add-to-list 'completion-styles 'semantic)
  (setq company-backends (delete 'company-semantic company-backends)) ; semantic conflicts with company
  (add-to-list 'company-backends 'company-c-headers)	  ; headers auto completion
  (add-to-list 'company-backends 'company-clang)
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
  ;; (setq undo-tree-auto-save-history t)
  ;; (setq undo-tree-history-directory-alist `((".*" . ,(concat emacs-tmp-dir "undo"))))
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ; undo with the fancy tree
  (define-key undo-tree-map (kbd "C--") 'undo-tree-undo) ; normal undo
  (global-undo-tree-mode)
  )

(use-package tabbar						; tabbar mode (tabs navigation)
  :ensure t
  :init
  :config
  (use-package tab-group					; organize tabs in groups
	:ensure t)
  (tabbar-mode t)							; ON
  (global-set-key (kbd "<end>") 'tabbar-backward-tab)
  (global-set-key (kbd "<home>") 'tabbar-forward-tab)
  (global-set-key (kbd "C-<end>") 'tabbar-backward-group)
  (global-set-key (kbd "C-<home>") 'tabbar-forward-group)
  (setq tabbar-use-images nil)			; faster ?
  (setq tabbar-separator (quote (0.5)))
  (set-face-attribute 'tabbar-default nil
					  :background "gray20" :foreground "gray20"
					  :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute 'tabbar-unselected nil
					  :background "gray30" :foreground "white"
					  :box '(:line-width 5 :color "gray30" :style nil))
  (set-face-attribute 'tabbar-selected nil
					  :background "gray75" :foreground "black"
					  :box '(:line-width 5 :color "gray75" :style nil))
  (set-face-attribute 'tabbar-highlight nil
					  :background "white" :foreground "black"
					  :underline nil
					  :box '(:line-width 5 :color "white" :style nil))
  (set-face-attribute 'tabbar-button nil
					  :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute 'tabbar-separator nil
					  :background "gray20" :height 0.6)
  (defun tabbar-buffer-tab-label (tab)
	(let ((label  (if tabbar--buffer-show-groups
					  (format " [%s] " (tabbar-tab-tabset tab))
					(format " %s " (tabbar-tab-value tab)))))
	  (if tabbar-auto-scroll-flag
		  label
		(tabbar-shorten
		 label (max 1 (/ (window-width)
						 (length (tabbar-view
								  (tabbar-current-tabset)))))))))
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
