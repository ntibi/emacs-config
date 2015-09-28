;;; myfunctions.el --- vanilla emacs config
;;; Commentary:
;;; some homemade functions
;;; and some proudly copy-pasted functions
;;; code:


(setq llast "*scratch*")				; useful vars
(setq last "*scratch*")

(defun temp-buffer()
  "Go to scratch buffer, or go back to the last buffer if already in scratchA"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*scratch*")
	(progn (setq llast (buffer-name (last-buffer))) (switch-to-buffer last) (setq last llast))
	(progn (setq last (buffer-name (current-buffer))) (switch-to-buffer (get-buffer-create "*scratch*")))
	)
  )

(defun set-mode-line ()
  (interactive)
  (progn
	(setq mode-line-format					; better mod line
		  (list
		   "["
		   '(:eval (if overwrite-mode (propertize "O" 'face 'font-lock-preprocessor-face) (propertize "I" 'face 'font-lock-constant-face)))
		   '(:eval (if (buffer-modified-p) (propertize "*" 'face 'font-lock-type-face) (propertize "-" 'face 'font-lock-constant-face)))
		   '(:eval (if buffer-read-only (propertize "R" 'face 'font-lock-preprocessor-face) (propertize "W" 'face 'font-lock-constant-face)))
		   "] "
		   (propertize user 'face 'font-lock-preprocessor-face)
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
	)
  )

(defun text-manip-mode (cmd)			; copy cut paste
  (interactive)
  (funcall cmd 1)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "d") '(lambda () "duplicate line or region" (interactive) (text-manip-mode 'duplicate-line-or-region)))
	 (define-key map (kbd "c") '(lambda () "copy line" (interactive) (text-manip-mode 'copy-line)))
	 (define-key map (kbd "k") '(lambda () "kill line" (interactive) (text-manip-mode 'kill-whole-line)))
	 (define-key map (kbd ";") '(lambda () "(un)comment line/region" (interactive) (text-manip-mode 'comment-or-uncomment-region-or-line)))
	 (define-key map (kbd "y") '(lambda () "yank" (interactive) (text-manip-mode 'yank)))
	 (define-key map (kbd "q") 'keyboard-quit)
	 (define-key map (kbd "SPC") 'keyboard-quit)
	 (define-key map (kbd "RET") 'keyboard-quit)
	 map)))

(defun scroll-mode (cmd)			; smooth scroll with arrow keys
  (interactive)
  (funcall cmd 1)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "<up>") '(lambda () "scroll up" (interactive) (scroll-mode 'scroll-down)))
	 (define-key map (kbd "<down>") '(lambda () "scroll down" (interactive) (scroll-mode 'scroll-up)))
	 (define-key map (kbd "<left>") '(lambda () "scroll left" (interactive) (scroll-mode 'scroll-right)))
	 (define-key map (kbd "<right>") '(lambda () "scroll right" (interactive) (scroll-mode 'scroll-left)))
	 (define-key map (kbd "q") 'keyboard-quit)
	 (define-key map (kbd "SPC") 'keyboard-quit)
	 (define-key map (kbd "RET") 'keyboard-quit)
	 map)))


(setq move-n 1)							; resize variables for dynamic resize
(setq old-resize-call nil)

(defun dynamic-resize-mode (cmd)
  (interactive)
  "dynamic resize mode (offset doubles every keypress)"
  (progn
	(if (eq old-resize-call cmd)		; double resize size every recall in the same dir
		(if (>= move-n 8) (setq move-n 8) (setq move-n (* 2 move-n))) ; it has to still <= 8
	  (setq move-n 1)
	  )
	(funcall cmd move-n)
	(setq old-resize-call cmd)
	)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "<right>") '(lambda () "right" (interactive) (dynamic-resize-mode 'enlarge-window-horizontally)))
	 (define-key map (kbd "<left>") '(lambda () "left" (interactive) (dynamic-resize-mode 'shrink-window-horizontally)))
	 (define-key map (kbd "<up>") '(lambda () "right" (interactive) (dynamic-resize-mode 'shrink-window)))
	 (define-key map (kbd "<down>") '(lambda () "left" (interactive) (dynamic-resize-mode 'enlarge-window)))
	 (define-key map (kbd "q") 'keyboard-quit)
	 (define-key map (kbd "SPC") 'keyboard-quit)
	 (define-key map (kbd "RET") 'keyboard-quit)
	 map)))

(defun xpaste ()
  "paste from x clipboard"
  (interactive)
  (insert (shell-command-to-string "xsel"))
)

(defun reload-dotemacs-file ()
  "Reload .emacs."
  (interactive)
  (load-file "~/.emacs"))

(defun smart-beginning ()
  "Get the beginning of region if mark is set, or the beginning of the line"
  (interactive)
  (if (region-active-p) (region-beginning) (line-beginning-position)))

(defun smart-end ()
  "Same with the end"
  (interactive)
  (if (region-active-p) (region-end) (line-end-position)))

(defun region-execute-python ()
  "Replace region by python output."
  (interactive)
  (let ((s (replace-regexp-in-string "\"" "'" (buffer-substring (smart-beginning) (smart-end)))))
	(progn
	  (kill-region (smart-beginning) (smart-end))
	  (insert (shell-command-to-string (concat "python2.7 -c \"" s "\"")))
	  )))
																								   
(defun region-as-python-string ()
  "Replace region by python-string."
  (interactive)
  (let ((s (replace-regexp-in-string "\"" "'" (buffer-substring (smart-beginning) (smart-end)))))
	(progn
	  (kill-region (smart-beginning) (smart-end))
	  (insert (shell-command-to-string (concat "python2.7 -c \"import sys; sys.stdout.write(" s "); sys.stdout.flush()\"")))
	  )))

(defun search-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
	"http://www.google.com/search?ie=utf-8&oe=utf-8&q="
	(if mark-active
		(buffer-substring (smart-beginning) (smart-end))
	  (read-string "Google: ")))))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
	(save-excursion
	  (let ((text (if use-region        ;Get region if active, otherwise line
					  (buffer-substring (region-beginning) (region-end))
					(prog1 (thing-at-point 'line)
					  (end-of-line)
					  (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
						  (newline))))))
		(dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
		  (insert text))))
	(if use-region nil                  ;Only if we're working with a line (not a region)
	  (let ((pos (- (point) (line-beginning-position)))) ;Save column
		(if (> 0 n)                             ;Comment out original with negative arg
			(comment-region (line-beginning-position) (line-end-position)))
		(forward-line 1)
		(forward-char pos)))))

(defun copy-line (arg)
	  "Copy lines (as many as prefix argument) in the kill ring.
	  Ease of use features:
	  - Move to start of next line.
	  - Appends the copy on sequential calls.
	  - Use newline as last char even on the last line of the buffer.
	  - If region is active, copy its lines."
	  (interactive "p")
	  (let ((beg (line-beginning-position))
			(end (line-end-position arg)))
		(when mark-active
		  (if (> (point) (mark))
			  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
			(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
		(if (eq last-command 'copy-line)
			(kill-append (buffer-substring beg end) (< end beg))
		  (kill-ring-save beg end)))
	  (kill-append "\n" nil)
	  (beginning-of-line (or (and arg (1+ arg)) 2))
	  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun comment-or-uncomment-region-or-line (&optional n) ; unused n arg for now
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-or-uncomment-region beg end)
	(next-line)))

(defun move-text (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text arg))

(require 'repeat)
(defun rep (cmd)
    "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (rep 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
	(fset (intern (concat (symbol-name cmd) "-repeat"))
		  `(lambda ,(help-function-arglist cmd) ;; arg list
			 ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
			 ,(interactive-form cmd) ;; interactive form
			 ;; see also repeat-message-function
			 (setq last-repeatable-command ',cmd)
			 (repeat nil)))
	(intern (concat (symbol-name cmd) "-repeat")))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text (- arg)))

(provide 'myfunctions)
;;; myfunctions.el ends here
