;;; .emacs --- my .emacs
;;; commentary:
;;; code:

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  )

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; config files path
(add-to-list 'load-path "~/.emacs.d/lisp/42/") ;; 42 files path


;; load 42 files
(load "42config.el")
(load "list.el")
(load "string.el")
(load "comments.el")
(load "header.el")

;; my config on vanilla emacs
(load "myconfig.el")

(add-hook 'after-init-hook #'global-flycheck-mode)
;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(load-theme 'monokai t)

;; company auto complete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(global-set-key (kbd "M-/") 'company-complete)
(global-set-key (kbd "M-.") 'company-show-doc-buffer)
;;


;; cool neotree
(require 'neotree)
(global-set-key (kbd "C-x a a") 'neotree-toggle)

;; rainbow/highlight stuff
(require 'rainbow-identifiers)
(require 'rainbow-mode)
(require 'rainbow-delimiters)
(require 'highlight-parentheses)
(require 'highlight-thing)

(add-hook 'prog-mode-hook 'rainbow-mode) ;; Colorize color hexcodes
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode) ;; different variables colors
(add-hook 'prog-mode-hook 'highlight-parentheses-mode) ;; highlight surrounding parentheses
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ;;  parentheses color according to depth
;;

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
;;


;; tabbar
(require 'tabbar)
(require 'tab-group)

(tabbar-mode t)
(load "tabbar-tweek.el") ; nice config

(global-set-key (kbd "C-x <left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x p") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <down>") 'tabbar-backward-group)
(global-set-key (kbd "C-x <up>") 'tabbar-forward-group)
(setq tabbar-use-images nil) ; fastah
;;



;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)
;;



;; zone
(require 'zone)
(zone-when-idle 60)
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; this is not vi(m)
(defconst wq "You mean C-x C-c ?")
(defconst qq "You mean C-x C-c ?")
(defconst w "You mean C-x C-s ?")
(defconst q! "You mean C-x C-c ?")
(defconst !q "You mean C-x C-c ?")
(defconst wq! "You mean C-x C-c ?")
(defconst qw! "You mean C-x C-c ?")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(highlight-thing-what-thing (quote word))
 '(hl-paren-delay 0.01)
 '(tabbar-separator (quote (0.5)))
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
	((20 . "#dd5542")
	 (40 . "#CC5542")
	 (60 . "#fb8512")
	 (80 . "#baba36")
	 (100 . "#bdbc61")
	 (120 . "#7d7c61")
	 (140 . "#6abd50")
	 (160 . "#6aaf50")
	 (180 . "#6aa350")
	 (200 . "#6a9550")
	 (220 . "#6a8550")
	 (240 . "#6a7550")
	 (260 . "#9b55c3")
	 (280 . "#6CA0A3")
	 (300 . "#528fd1")
	 (320f . "#5180b3")
	 (340 . "#6380b3")
	 (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; installed packages


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Auto set vars :
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:inherit error :underline "red"))))
 '(flycheck-warning ((t (:inherit warning :background "yellow"))))
 '(hi-yellow ((t (:underline t))))
 '(region ((t (:background "color-240"))))
 '(show-paren-match ((t (:background "cyan" :foreground "green" :inverse-video t :weight normal))))
 '(show-paren-mismatch ((t (:background "#1B1E1C" :foreground "red" :inverse-video t :weight normal))))
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "gray20") :underline nil))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray20" :foreground "gray20" :box (:line-width 1 :color "gray20") :underline nil :height 1))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "gray20")))))

(provide '.emacs)
;;; .emacs ends here
