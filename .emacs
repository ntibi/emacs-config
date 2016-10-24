;;; .emacs --- my .emacs
;;; commentary:
;;; code:


(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; (benchmark-init/activate)										; uncomment to benchmark emacs startup
;; (add-hook 'after-init-hook 'benchmark-init/show-durations-tree) ; uncomment to get a startup summary

(unless (package-installed-p 'use-package) ; ensure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package)
  )


(add-to-list 'load-path "~/.emacs.d/lisp/")    ;; my config files path
(add-to-list 'load-path "~/.emacs.d/lisp/42/") ;; 42 config files path
(add-to-list 'load-path "~/.emacs.d/lisp/themes/")     ;; themes path
(add-to-list 'load-path "~/.emacs.d/lisp/additional/") ;; additional files


(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid))) ; set backup dir (/tmp/emacs{uid})

(defconst emacs-wd default-directory)	; directory where emacs started

;;;; load config files


;; (mapcar #'(lambda (f) (ignore-errors (load f)))
(mapcar #'(lambda (f) (load f))			; TODO: activate a 'safe mode' on error
		(list
		 "monokai-theme.el"				; dank theme
		 "packages-config.el"			; packages config
		 "myfunctions.el"				; user defined functions
		 "myconfig.el"					; vanilla config
		 "user-macros.el"				; load macro saved from other sessions
		 "42config.el"					; c norme
		 "header.el"					; 42 header
		 ))


;;;; auto-set config variables:


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(helm-swoop company-clang use-package zenburn-theme yasnippet undo-tree tabbar-ruler tab-group strings rainbow-mode rainbow-identifiers rainbow-delimiters pyenv-mode popup-complete neotree multiple-cursors mouse+ mode-icons linum-relative highlight-thing highlight-parentheses helm-company git-commit fuzzy function-args flymake-shell flymake-python-pyflakes flycheck-clangcheck find-file-in-project company-c-headers company-anaconda benchmark-init ace-jump-mode))))

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
 '(show-paren-match ((t (:background "color-33" :foreground "color-27"))))
 '(show-paren-mismatch ((t (:background "#1B1E1C" :foreground "red" :inverse-video t :weight normal))))
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "gray20") :underline nil))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray20" :foreground "gray20" :box (:line-width 1 :color "gray20") :underline nil :height 1))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "gray20")))))

(provide '.emacs)
;;; .emacs ends here
