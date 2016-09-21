;;; packages-config.el --- configuration for my packages
;;; commentary:
;;; code:

; installed packages
  ;; anaconda-mode
  ;; async
  ;; auto-complete
  ;; auto-complete-c...
  ;; company
  ;; company-anaconda
  ;; company-c-headers
  ;; concurrent
  ;; ctable
  ;; cyberpunk-theme
  ;; dash
  ;; deferred
  ;; elpy
  ;; epc
  ;; epl
  ;; f
  ;; find-file-in-pr...
  ;; flycheck
  ;; flycheck-clangc...
  ;; flycheck-ocaml
  ;; flycheck-pos-tip
  ;; flycheck-tip
  ;; flymake-easy
  ;; flymake-python-...
  ;; flymake-shell
  ;; function-args
  ;; fuzzy
  ;; helm
  ;; helm-company
  ;; helm-core
  ;; highlight-inden...
  ;; highlight-paren...
  ;; highlight-thing
  ;; json-rpc
  ;; let-alist          1.
  ;; linum-relative
  ;; merlin
  ;; monokai-theme
  ;; mouse+
  ;; multiple-cursors
  ;; neotree
  ;; pastels-on-dark...
  ;; pkg-info
  ;; popup
  ;; pyenv-mode
  ;; python-environment
  ;; pyvenv
  ;; rainbow-delimiters
  ;; rainbow-identif...
  ;; rainbow-mode
  ;; s
  ;; strings
  ;; swiper
  ;; tab-group
  ;; tabbar
  ;; undo-tree
  ;; yasnippet



(use-package flycheck
  :defer 1
  :config
  (require 'flycheck-clangcheck)
  (add-hook 'after-init-hook #'global-flycheck-mode) ; flycheck ON
  (setq flycheck-clangcheck-analyze t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)) ; check at save
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))) ; --std=c++11
  )

(use-package company					; company auto complete
  :bind (("M-/" . company-complete))
  :config
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
  )

(require 'company-clang)
(set 'company-clang-arguments (list (concat "-I" (file-name-directory load-file-name) "./") (concat "-I" (file-name-directory load-file-name) "/includes/") (concat "-I" (file-name-directory load-file-name) "../includes/")))

(require 'company-c-headers)


(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook (lambda () "" (interactive) (add-to-list 'company-backends 'company-anaconda)))
(add-hook 'python-mode-hook (lambda () "" (interactive) (pyenv-mode)))

(use-package yasnippet							 ; yet another snippet
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
		 ("M-x"				.	helm-M-x)
		 ([remap find-file]			.	helm-find-files)
		 ([remap occur]				.	helm-occur)
		 ([remap list-buffers]		.	helm-buffers-list)
		 ([remap dabbrev-expand]	.	helm-dabbrev)
		 )
  :init ()
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)				; shrink minibuffer if possible
  )


(use-package find-file-in-project		; find a file anywhere in the project
  :bind (("C-x f" . find-file-in-project))
  :config (setq ffip-project-file ".git")
  )


(use-package highlight-thing			; highlight current line/word
  :init
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  :config
  (setq highlight-thing-what-thing 'word)	; underline word
  (setq highlight-thing-delay-seconds 0.1)
  (custom-set-faces '(hi-yellow ((t (:underline t)))))
)

(require 'rainbow-mode)					; colorize hex codes
(add-hook 'prog-mode-hook 'rainbow-mode)
(require 'rainbow-identifiers)			; different variables color
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(require 'highlight-parentheses)		; highlight surrounding parentheses
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(require 'rainbow-delimiters)			; parentheses color according to depth
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use-package neotree						; neo tree
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

(require 'undo-tree)					; undo tree
(global-undo-tree-mode)					; set undo-tree as default undo (C-x u)
(define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ; undo with the fancy tree
(define-key undo-tree-map (kbd "C--") 'undo-tree-undo) ; normal undo

(require 'tabbar)						; tabbar mode
(require 'tab-group)					; organize tabs in groups
(tabbar-mode t)							; ON
(load "tabbar-tweek.el")				; nice tabbar config
(global-set-key (kbd "<end>") 'tabbar-backward-tab)
(global-set-key (kbd "<home>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<end>") 'tabbar-backward-group)
(global-set-key (kbd "C-<home>") 'tabbar-forward-group)


(global-set-key (kbd "M-f") 'ace-jump-word-mode) ; quickly jump to a word
(setq ace-jump-mode-case-fold t)				 ; case insensitive
(setq ace-jump-mode-move-keys (cl-loop for i from ?a to ?z collect i)) ; use [a-z]

(global-set-key (kbd "C-]") 'jump-char-forward)


(global-set-key (kbd "C-x <right>") '(lambda () "DOCSTRING" (interactive) (tab-mode 'tabbar-forward-tab)))
(global-set-key (kbd "C-x <left>") '(lambda () "DOCSTRING" (interactive) (tab-mode 'tabbar-backward-tab)))
(global-set-key (kbd "C-x <up>") '(lambda () "DOCSTRING" (interactive) (tab-mode 'tabbar-forward-group)))
(global-set-key (kbd "C-x <down>") '(lambda () "DOCSTRING" (interactive) (tab-mode 'tabbar-backward-group)))

(setq tabbar-use-images nil)			; faster ?

(use-package multiple-cursors			; multiple cursors
  :bind (
		 ("<C-down-mouse-1>" . mc/add-cursor-on-click) ; ctrl clic to add cursor
		 ("C-x m" . mc/edit-lines)		; spawn a cursor on each line
		 )
  )

(provide 'myconfig)
;;; packages-config.el ends here
