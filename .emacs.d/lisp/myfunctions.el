;;; myfunctions.el --- vanilla emacs config
;;; commentary:
;;; some homemade functions
;;; and some proudly copy-pasted functions
;;; code:


(defun select-line ()
  "Select the current line"
  (interactive)
  (progn
	(end-of-line) ; move to end of line
	(set-mark (line-beginning-position))
	(next-line)
	)
  )

(cl-defmacro new-onekey (name starter onekeys &optional (doc ""))
  "GENERATE A MINOR-MODE FOR FASTER AND MORE ACCESSIBLE SHORTCUTS.
NAME: onekey-mode name
STARTER: keybind to start this mode
ONEKEYS: one key keybinds to use in this mode
DOC: describes the mode
"
  (let ((fname (intern (format "onekey-%s-mode" name)))	  ; minor mode
		(gname (intern (format "global-onekey-%s-mode" name))) ; global minor mode
		(help-function (intern (format "onekey-%s-mode-help" name)))) ; nice help
    `(progn
	   (defun ,help-function ()										 ; format a nice help
		 (interactive)
		 (with-help-window ,(symbol-name help-function) (with-current-buffer ,(symbol-name help-function) (insert
		  (mapconcat 'identity
					 (mapcar #'(lambda (e)
								 (format "\"%s\": %s" (propertize (car e) 'font-lock-face 'font-lock-string-face) (propertize (symbol-name (cdr e)) 'font-lock-face 'italic)))
							 ,onekeys)
					 "\n")))))
       (define-minor-mode ,fname
		 ,doc
		 :lighter ,(concat " " name)
		 :keymap  (mapcar (lambda (e) (interactive) (cons (kbd (car e)) (cdr e))) ; apply kbd to all keybinds (keybind . function)
						  (append ,onekeys '(
											 ("q"			.	(lambda () (interactive) (call-interactively ',gname)))
											 ("C-g"			.	(lambda () (interactive) (call-interactively ',gname)))
											 ("?"			.	,help-function)
											 ("9"			.	digit-argument)
											 ("8"			.	digit-argument)
											 ("7"			.	digit-argument)
											 ("6"			.	digit-argument)
											 ("5"			.	digit-argument)
											 ("4"			.	digit-argument)
											 ("3"			.	digit-argument)
											 ("2"			.	digit-argument)
											 ("1"			.	digit-argument)
											 ("0"			.	digit-argument)
											 ("-"			.	digit-argument)
											 ))))
	   (define-globalized-minor-mode ,gname ,fname (lambda () (unless (minibufferp) (,fname 1)))) ; create the global minor mode (only if not in minibuffer)
       (mapcar							; bind all keys minor onekey mode activation + right command exectuion
	   	#'(lambda (e)
	   		(let (
	   			  (onekey-starter (intern (format "onekey-%s-mode/%s" ,name (car e))))
	   			  (starter ,starter)
	   			  (gname ',gname)
				  (doc (format "%s\n%s calls %s" ,doc (car e) (symbol-name (cdr e))))
				  )
	   		  (eval
	   		   `(progn
	   			  (defun ,onekey-starter ()
					,doc
	   				(interactive)
	   				(when (,gname) (call-interactively ',(cdr e)))) ; start the mode + execute the command
	   			  (global-set-key (kbd (concat ,starter " " ,(car e))) ',onekey-starter)))))
	   	,onekeys))))


(defun yank-pop-forward (&optional arg)
  (interactive "p")
  (yank-pop (- arg)))


(setq llast "*scratch*")				; useful vars for scratch
(setq last "*scratch*")

(defun temp-buffer()
  "Go to scratch buffer, or go back to the last buffer if already in scratch"
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
		   " "
		   '(:eval (propertize (cond
					((string= "normal" evil-state)
					 "N")
					((string= "insert" evil-state)
					 "I")
					((string= "visual" evil-state)
					 "V")
					((string= "emacs" evil-state)
					 "E")
					((t)
					 "?"))
					'face 'font-lock-function-name-face))
		   " ["
		   '(:eval (if overwrite-mode (propertize "O" 'face 'font-lock-preprocessor-face) (propertize "I" 'face 'font-lock-constant-face)))
		   '(:eval (if (buffer-modified-p) (propertize "*" 'face 'font-lock-type-face) (propertize "-" 'face 'font-lock-constant-face)))
		   '(:eval (if buffer-read-only (propertize "R" 'face 'font-lock-preprocessor-face) (propertize "W" 'face 'font-lock-constant-face)))
		   "] "
		   (propertize user 'face 'font-lock-preprocessor-face)
		   ":<"
		   '(:eval (propertize (abbreviate-file-name default-directory) 'face 'font-lock-function-name-face)) ; get file path
		   '(:eval (propertize "%b" 'face 'font-lock-type-face)) ; buffer name
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
		   "  -- "
		   minor-mode-alist					; minor modes
		   " %-"				 ; fill with '-'
		   ))
	(face-remap-add-relative 'mode-line '((:foreground "color-234" :background "color-237") mode-line))
	(face-remap-add-relative 'mode-line-inactive '((:foreground "color-234" :background "color-234") mode-line))
	)
  )


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
	 (define-key map (kbd "<up>") '(lambda () "up" (interactive) (dynamic-resize-mode 'shrink-window)))
	 (define-key map (kbd "<down>") '(lambda () "down" (interactive) (dynamic-resize-mode 'enlarge-window)))
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
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(cl-defun move-text-down (&optional (arg 1))
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

(cl-defun move-text-up (&optional (arg 1))
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text (- arg)))

(defun save-macro (name)
 "save a macro. Take a name as argument
    and save the last defined macro under
    this name in the .macros.el"
 (interactive "SName of the macro : ")
 (kmacro-name-last-macro name)
 (find-file "~/.emacs.d/lisp/user-macros.el")
 (goto-char (point-max))
 (newline)
 (insert-kbd-macro name)
 (newline)
 (save-buffer)
 ;; (kill-this-buffer)
 (switch-to-buffer nil))

(defalias 'save-macro-temporaily 'name-last-kbd-macro)

(defun zone-choose (pgm)
  "choose a pgm to run for `zone'."
  (interactive
   (list
	(completing-read
	 "program: "
	 (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
	(zone)))

(defun switch-to-new-buffer()
  "ask for a name, create a new buffer, and switch"
  (interactive)
  (switch-to-buffer
   (get-buffer-create
	(read-string "name: "))))

(defun sudo-save ()
  "Save a write protected file by connecting in ssh with root on the localhost"
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File : ")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun replace-region-by-shell-output ()
  "Replace region or line by shell command output."
  (interactive)
  (let ((s (buffer-substring (smart-beginning) (smart-end))))
	(let ((out (shell-command-to-string (concat "echo -n " (shell-quote-argument s ) " | " (read-string "> ")))))
	  (progn
		(kill-region (smart-beginning) (smart-end))
		(insert out)
		))))

;; todo: a function to replace words according to a regex or a list of regex
;; like: ("true": "false") ("false": "true")
;; or: ("\\([A-Z]\\)": "_\1")

(provide 'myfunctions)
;;; myfunctions.el ends here
