;;; mode-line.el --- defines mode-line
;;; commentary:
;;; Defines a nicer mode-line format
;;; code:

(defconst user (getenv "USER"))
;; (defconst host (if (t) (getenv "HOSTNAME") (getenv "HOST")))

(setq mode-line-format					; better mod line
	  (list
	   "["
	   '(:eval (if overwrite-mode (propertize "O" 'face 'font-lock-preprocessor-face) (propertize "I" 'face 'font-lock-constant-face)))
	   '(:eval (if (buffer-modified-p) (propertize "*" 'face 'font-lock-type-face) (propertize "-" 'face 'font-lock-constant-face)))
	   '(:eval (if buffer-read-only (propertize "R" 'face 'font-lock-preprocessor-face) (propertize "W" 'face 'font-lock-constant-face)))
	   "] "
	   (propertize user 'face 'font-lock-preprocessor-face)
	   ;; "@"
	   ;; (propertize host 'face 'font-lock-preprocessor-face)
	   ":<"
	   '(:eval (propertize "%b" 'face 'font-lock-function-name-face)) ; buffer name
	   "> "
	   " ("
	   (propertize "%03l" 'face 'font-lock-type-face) "," ; line and column
	   (propertize "%03c" 'face 'font-lock-type-face)
	   ") ["
	   (propertize "%p" 'face 'font-lock-constant-face) ; % above top
	   "/"
	   (propertize "%I" 'face 'font-lock-constant-face) ; size
	   "] ["
	   '(:eval (propertize mode-name 'face 'font-lock-string-face)) ; global mode
	   "] - "
	   '(:eval (propertize (format-time-string "%a %b %d") 'face 'font-lock-preprocessor-face)) ; date
	   '(:eval (propertize (format-time-string " - %H:%M:%S") 'face 'font-lock-constant-face)) ; time
	   '(:eval (propertize (emacs-uptime " - Up:%hh%mm") 'face 'font-lock-function-name-face)) ; uptime
	   " -- "
	   minor-mode-alist					; minor modes
	   " %e%-"				 ; fill with '-'
	   ))

(face-remap-add-relative 'mode-line '((:foreground "color-234" :background "color-236") mode-line))
(face-remap-add-relative 'mode-line-inactive '((:foreground "color-234" :background "Color-234") mode-line))

