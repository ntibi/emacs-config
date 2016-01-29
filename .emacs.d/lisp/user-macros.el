;;; user-macros.el --- macros saved with save-macro(myfunctions.el) will be added here
;;; commentary:
;;; code:


(fset 'm-format-hexdump
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("OCOC OCOC OCOC OCOC OCOC OCOC OCOC OCOC  OCOC OCOC OCOC OCOC OCOC OCOC OCOC OCOC" 0 "%d")) arg)))

