(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  )

(add-to-list 'load-path "~/.emacs.d/lisp/")

(menu-bar-mode -1) ; No menu bar
(kill-buffer "*scratch*") ; Remove scratch buffer

(add-hook 'prog-mode-hook 'global-linum-mode) ; linum mode when programming
(setq linum-format "%4d \u2502 ") ; nb | text...

;(load-theme 'find-a-cool-theme t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; use mouse
(require 'mouse)
(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-2>") 'nil)
(global-set-key (kbd "<mouse-3>") 'nil)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'tuareg-mode-hook 'hs-minor-mode)

(global-set-key (kbd "C-c m") 'compile)


(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key [f5] 'hs-hide-all)
(global-set-key [f6] 'hs-show-all)
(global-set-key (kbd "C-g") 'keyboard-escape-quit)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x x") 'hs-toggle-hiding)

(custom-set-variables '(scroll-conservatively 1) '(scroll-margin 10)) ; pre scroll



(global-set-key (kbd "C-x t") 'tuareg-mode)

(global-set-key (kbd "C-x a a") 'neotree-toggle)



;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backup dir

(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rainbow stuff

(require 'rainbow-identifiers)
(require 'rainbow-delimiters)
(require 'highlight-parentheses)
(require 'highlight-thing)

(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

(add-hook 'prog-mode-hook 'global-highlight-thing-mode)
(setq highlight-thing-delay-seconds 0.05)

(define-globalized-minor-mode global-highlight-parentheses-mode
highlight-parentheses-mode
(lambda ()
  (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo tree

(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; tabbar
(require 'tabbar)
(tabbar-mode t)

(load "tabbar-tweek.el") ; nice config

(global-set-key (kbd "C-n") 'tabbar-backward-tab)
(global-set-key (kbd "C-j") 'tabbar-forward-tab)
(global-set-key (kbd "M-n") 'tabbar-backward-group)
(global-set-key (kbd "M-j") 'tabbar-forward-group)
(setq tabbar-use-images nil) ; fastah
;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;; Multiple cursors

(require 'multiple-cursors)
(global-set-key (kbd "<C-down-mouse-3>") 'mc/add-cursor-on-click)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;; this is not vi(m)

(defconst wq "You mean C-x C-c ?")
(defconst qq "You mean C-x C-c ?")
(defconst w "You mean C-x C-s ?")
(defconst q! "You mean C-x C-c ?")
(defconst !q "You mean C-x C-c ?")
(defconst wq! "You mean C-x C-c ?")
(defconst qw! "You mean C-x C-c ?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;; installed packages

  ;; caml               
  ;; cyberpunk-theme    
  ;; firecode-theme     
  ;; highlight-paren... 
  ;; highlight-thing    
  ;; monokai-theme      
  ;; mouse+             
  ;; multiple-cursors   
  ;; neotree            
  ;; pastels-on-dark... 
  ;; rainbow-delimiters 
  ;; rainbow-identif... 
  ;; rainbow-mode       
  ;; strings            
  ;; tab-group          
  ;; tabbar             
  ;; tabbar-ruler       
  ;; tuareg             
  ;; undo-tree          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;; Auto set vars :


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
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
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-yellow ((t (:underline t))))
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "gray20") :underline nil))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray20" :foreground "gray20" :box (:line-width 1 :color "gray20") :underline nil :height 0.8)))))
