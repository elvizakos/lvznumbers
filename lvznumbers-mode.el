;;; lvznumbers-mode.el --- Minor mode for working with numbers and math.
;;; -*- coding: utf-8 -*-

;; Copyright © 2018, Nikos Skarmoutsos

;; Version: VERSION
;; Author: Nikos Skarmoutsos <elvizakos AT yahoo DOT gr>
;; Maintainer: Nikos Skarmoutsos
;; Created: May 2018
;; Keywords: numbers,hex,decimal,math
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

;;(defvar lvznumbers-map nil "Keymap for this minor mode")

;;---- CONSTANTS ------------------------------------------------------------------

(defconst lvznumbers-version "VERSION" "LVzNumbers version.")

;;---- VARIABLES ------------------------------------------------------------------

(defvar lvznumbers-keymap (make-sparse-keymap) "Keymap for lvznumbers.")

;;---- OPTIONS --------------------------------------------------------------------

(defgroup lvznumbers nil "LVzNumbers minor mode settings."
  :group 'tools)

(defcustom lvznumbers-increment-decrement 1 "Increment/Decrement default value."
  :type 'number
  :group 'lvznumbers)

(defgroup lvznumbers-keys nil "LVzNumbers minor mode settings."
  :group 'tools)
(defcustom lvznumbers-increment-keycomb "C-c <up>" "Default key combination for increment decimal number under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-decrement-keycomb "C-c <down>" "Default key combination for decrement decimal number under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-increment-digit-keycomb "C-x <up>" "Default key combination for increment decimal digit under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-decrement-digit-keycomb "C-x <down>" "Default key combination for decrement decimal number under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-increment-hex-keycomb "C-c H" "Default key combination for increment hexadecimal digit under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-decrement-hex-keycomb "C-x H" "Default key combination for decrement hexadecimal digit under cursor."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-goto-next-dec-number-keycomb "C-c C-n C-n" "Default key combination moving the cursor to over next decimal number in buffer."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-goto-previous-dec-number-keycomb "C-c C-n C-p" "Default key combination moving the cursor to over previous decimal number in buffer."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-addition-with-paste-keycomb "C-c C-v +" "Default key combination for running addition-with-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-subtract-paste-keycomb "C-c C-v -" "Default key combination for running subtract-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-multiply-paste-keycomb "C-c C-v *" "Default key combination for running multiply-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-divide-paste-keycomb "C-c C-v /" "Default key combination for running multiply-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-addition-and-copy-keycomb "C-c C-v C-c +" "Default key combination for running addition-and-copy command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-subtract-copy-keycomb "C-c C-v C-c -" "Default key combination for running subtract-copy command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-multiply-copy-keycomb "C-c C-v C-c *" "Default key combination for running multiply-copy command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-divide-copy-keycomb "C-c C-v C-c /" "Default key combination for running divide-copy command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-math-on-region-keycomb  "C-; m m" "Default key combination for running do-math-on-region command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-math-on-region-keycomb-alt  "C-M-z m" "Default alternative key combination for running do-math-on-region command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-killring-and-paste-keycomb  "C-; m C-y" "Default key combination for running do-math-on-killring-and-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-killring-and-paste-keycomb-alt  "C-; m C-v" "Default alternative key combination for running do-math-on-killring-and-paste command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-region-and-copy-keycomb  "C-; m M-w" "Default key combination for running do-math-on-region-and-copy command."
  :type 'string
  :group 'lvznumbers-keys)
(defcustom lvznumbers-do-region-and-copy-keycomb-alt  "C-; m C-c" "Default alternative key combination for running do-math-on-region-and-copy command."
  :type 'string
  :group 'lvznumbers-keys)
;;---- FUNCTIONS ------------------------------------------------------------------

(defun increment-number-at-point ( x ) "Function for increment by the value of \"lvznumbers-increment-decrement\" the decimal number under cursor."
	   (interactive "p")
	   (let (
			 (cpoint (point))
			 (incn lvznumbers-increment-decrement)
			 )
		 (if x (setq incn (* x incn)))
		 (skip-chars-backward "0-9")
		 (if (char-equal (char-before) ?-) (backward-char 1))
		 (or
		  (looking-at "[\\-]?[0-9]+")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (+ incn (string-to-number (match-string 0)))))
		 (goto-char cpoint)))

(defun increment-digit-at-point () "Function for increment by one the decimal digit after cursor."
	   (interactive)
	   (or
		(looking-at "[0-9]")
		(error "Not a decimal number at point"))
	   (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
	   (backward-char 1))

(defun decrement-number-at-point ( x )	"Function for decrement by the value of \"lvznumbers-increment-decrement\" the decimal number under cursor."
	   (interactive "p")
	   (let (
			 (cpoint (point))
			 (incn lvznumbers-increment-decrement)
			 )
		 (if x (setq incn (* x incn)))
		 (skip-chars-backward "0-9")
		 (if (char-equal (char-before) ?-) (backward-char 1))
		 (or
		  (looking-at "[\\-]?[0-9]+")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (- (string-to-number (match-string 0)) incn)))
		 (goto-char cpoint)))

(defun decrement-digit-at-point () "Function for decrement by one the decimal digit under cursor."
	   (interactive)
	   (or
		(looking-at "[0-9]")
		(error "Not a decimal number at point"))
	   (replace-match (number-to-string (1- (string-to-number (match-string 0)))))
	   (backward-char 1))

(defun increment-hex-at-point ( x ) "Function for increment by the value of \"lvznumbers-increment-decrement\" the hexadecimal number under cursor."
	   (interactive "p")
	   (let (
			 (cpoint (point))
			 (incn lvznumbers-increment-decrement)
			 )
		 (if x (setq incn (* x incn)))
		 (skip-chars-backward "0-9A-Fa-f")
		 (or
		  (looking-at "[0-9A-Fa-f]+")
		  (error "Not a hexadecimal number at point"))
		 (replace-match (format "%x" (+ incn (string-to-number (match-string 0) 16))))
		 (goto-char cpoint)))

(defun decrement-hex-at-point ( x ) "Function for decrement by the value of \"lvznumbers-increment-decrement\" the hexadecimal number under cursor."
	   (interactive "p")
	   (let (
			 (cpoint (point))
			 (incn lvznumbers-increment-decrement)
			 )
		 (if x (setq incn (* x incn)))
		 (skip-chars-backward "0-9A-Fa-f")
		 (or
		  (looking-at "[0-9A-Fa-f]+")
		  (error "Not a hexadecimal number at point"))
		 (replace-match (format "%x" (- (string-to-number (match-string 0) 16) incn)))
		 (goto-char cpoint)))

(defun addition-with-paste () "Function for adding the number in kill-ring to the number at cursor and replace the latter."
	   (interactive)
	   (let ((start (point)))
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (+ (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
		 (goto-char start)))

(defun subtract-paste () "Function for subtraction of the number in kill-ring from the number at cursor and replace the latter."
	   (interactive)
	   (let ((start (point)))
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (- (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
		 (goto-char start)))

(defun multiply-paste () "Function for multiplying the number in kill-ring with the number at cursor and replace the latter."
	   (interactive)
	   (let ((start (point)))
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (* (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
		 (goto-char start)))

(defun divide-paste () "Function for dividing the number in kill-ring to the number at cursor and replace the latter."
	   (interactive)
	   (let ((start (point)))
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (replace-match (number-to-string (/ (+ 0.0 (string-to-number (match-string 0))) (string-to-number (substring-no-properties (car kill-ring))))))
		 (goto-char start)))

(defun addition-and-copy () "Function for adding the number in kill-ring to the number at cursor and replace the first."
	   (interactive)
	   (let (
			 (start (point))
			 (res "")
			 )
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (setq res (number-to-string (+ (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
		 (kill-new res)
		 (goto-char start)
		 (message (format "Added \"%s\" to kill-ring." res))))

(defun subtract-copy () "Function for subtraction of  the number at cursor from the number in kill-ring and replace the first."
	   (interactive)
	   (let (
			 (start (point))
			 (res "")
			 )
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (setq res (number-to-string (- (string-to-number (substring-no-properties (car kill-ring))) (string-to-number (match-string 0)))))
		 (kill-new res)
		 (goto-char start)
		 (message (format "Added \"%s\" to kill-ring." res))))

(defun multiply-copy () "Function for multiplying the number in kill-ring with the number at cursor and replace the first."
	   (interactive)
	   (let (
			 (start (point))
			 (res "")
			 )
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (setq res (number-to-string (* (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
		 (kill-new res)
		 (goto-char start)
		 (message (format "Added \"%s\" to kill-ring." res))))

(defun divide-copy () "Function for dividing the number at cursor to the number in kill-ring and replace the first."
	   (interactive)
	   (let (
			 (start (point))
			 (res "")
			 )
		 (skip-chars-backward "0-9")
		 (skip-chars-backward "-")
		 (or
		  (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
		  (error "Not a decimal number at point"))
		 (setq res (number-to-string (/ (string-to-number (substring-no-properties (car kill-ring))) (+ 0.0 (string-to-number (match-string 0))))))
		 (kill-new res)
		 (goto-char start)
		 (message (format "Added \"%s\" to kill-ring." res))))

(defun number-type () "Function to return the number type under the cursor or 'nil' if there is not a number."
	   (interactive)
	   (let (
			 (cpoint (point))
			 (rett (list))
			 (retstr "")
			 )
		 (skip-chars-backward "0-9a-fA-F")(skip-chars-backward "-")
		 (if (not (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?\\($\\|[ \t\n\r(.,]\\)")) nil (progn
																						  (setq retstr (concat retstr ", decimal"))
																						  (add-to-list 'rett "decimal")))
		 (goto-char cpoint)
		 (skip-chars-backward "0-9a-fA-F")(skip-chars-backward "-")
		 (if (not (looking-at "[0-9a-fA-F]+\\($\\|[ \t\n\r(.,]\\)")) nil (progn
																		   (setq retstr (concat retstr ", hexadecimal"))
																		   (add-to-list 'rett "hexadecimal")))
		 (goto-char cpoint)
		 (skip-chars-backward "0-9a-fA-F")(skip-chars-backward "-")
		 (if (not (looking-at "[0-7]+\\($\\|[ \t\n\r(.,]\\)")) nil (progn
																	 (setq retstr (concat retstr ", octal"))
																	 (add-to-list 'rett "octal")))
		 (goto-char cpoint)
		 (skip-chars-backward "0-9a-fA-F")(skip-chars-backward "-")
		 (if (not (looking-at "[01]+\\($\\|[ \t\n\r(.,]\\)")) nil (progn
																	(setq retstr (concat retstr ", binary"))
																	(add-to-list 'rett "binary")))
		 (goto-char cpoint)
		 (if (string= retstr "")
			 (progn
			   (message "Doesn't match to a number.")
			   nil)
		   (progn
			 (setq retstr (substring retstr 2))
			 (message (format "Matches to %s." retstr))
			 rett
			 ))))

(defun lvznumbers-find-first-closing-bracket (str) "Function to return the contents of the first parenthesis without other parenthesis in it. If there isn't any parenthesis, it returns 'nil'."
	   (if (and
			(string-match-p "(" str)
			(string-match-p ")" str))
		   (let* (
				  (e (string-match ")" str))
				  (s (substring str 0 e))
				  (b (- (length s) (string-match "(" (reverse s))))
				  )
			 (list
			  (substring s 0 (1- b))
			  (substring s b e)
			  (substring str (1+ e))
			  ))
		 nil))

(defun lvznumbers-do-math (op n1 n2) "Function to do math operation between two numbers."
	   (setq
		n1 (float n1)
		n2 (float n2))
	   (cond
		((or (string= "**" op) (string= "^" op)) (expt n1 n2)) ; Power
		((or (string= "//" op) (string= "√" op)) (expt n1 (/ 1.0 n2))) ; Root
		((or (string= "*" op) (string= "×" op) (string= "·" op)) (* n1 n2)) ; Multiplication
		((or (string= "/" op) (string= "÷" op) (string= ":" op)) (/ n1 n2)) ; Division
		((string= "\\" op) (truncate (/ n1 n2))) ; Integer division
		((string= "%" op) (mod n1 n2)) ; Division remainder
		((string= "+" op) (+ n1 n2)) ; Addittion
		((or (string= "-" op) (string= "—" op)) (- n1 n2)) ; Subtraction
		))

(defun lvznumbers-do-parenthesis (str) "Function for doing all math operations in a string without parenthesis."

	   (let (
			 (mp1 0)
			 (mop "")
			 (nbr1 "")
			 (nbr2 "")
			 (str1 "")
			 (str2 "")
			 (tmpm "")
			 )
		 (while								; Powers and Roots
			 (progn
			   (setq mp1 (string-match "[/]\\{2\\}\\|√\\|[*]\\{2\\}\\|[\\^]" str))
			   (if mp1
				   (progn
					 (setq mop (match-string 0 str))
					 (setq
					  str1 (substring str 0 mp1)
					  str2 (substring str (+ mp1 (length mop))))
					 (string-match "[\\-]?[0-9]+\\([.][0-9]+\\)?[ \t\n\r]*$" str1)
					 (setq nbr1 (match-string 0 str1))
					 (string-match "^[ \t\n\r]*[\\-]?[0-9]+\\([.][0-9]+\\)?" str2)
					 (setq nbr2 (match-string 0 str2))
					 (setq
					  str1 (substring str1 0 (- (length str1) (length nbr1)))
					  str2 (substring str2 (length nbr2)))
					 
					 (setq str (concat str1 (number-to-string (lvznumbers-do-math mop (string-to-number nbr1) (string-to-number nbr2))) str2))
					 t)
				 nil)
			   mp1
			   )
		   )

		 (while								; Multiplications, Divisions, Integer divisions and Division remainders
			 (progn
			   (setq mp1 (string-match "[×÷·:*/%\\\\]" str))
			   (if mp1
				   (progn
					 (setq mop (match-string 0 str))
					 (setq
					  str1 (substring str 0 mp1)
					  str2 (substring str (+ mp1 (length mop))))
					 (string-match "[\\-]?[0-9]+\\([.][0-9]+\\)?[ \t\n\r]*$" str1)
					 (setq nbr1 (match-string 0 str1))
					 (string-match "^[ \t\n\r]*[\\-]?[0-9]+\\([.][0-9]+\\)?" str2)
					 (setq nbr2 (match-string 0 str2))
					 (setq
					  str1 (substring str1 0 (- (length str1) (length nbr1)))
					  str2 (substring str2 (length nbr2)))
					 
					 (setq str (concat str1 (number-to-string (lvznumbers-do-math mop (string-to-number nbr1) (string-to-number nbr2))) str2))
					 t)
				 nil)
			   mp1
			   )
		   )

		 (while								; Additions and Subtractions
			 (progn
			   (setq mp1 (string-match "\\([0-9][ \t\n\r]*\\)\\([+—\\-]\\)\\([ \t\n\r]*[\\-]?[0-9]\\)" str))
			   (if mp1
				   (progn
					 (setq mp1 (+ mp1 (length (match-string 1 str))))
					 (setq mop (match-string 2 str))
					 (setq
					  str1 (substring str 0 mp1)
					  str2 (substring str (+ mp1 (length mop))))
					 (string-match "[\\-]?[0-9]+\\([.][0-9]+\\)?[ \t\n\r]*$" str1)
					 (setq nbr1 (match-string 0 str1))
					 (string-match "^[ \t\n\r]*[\\-]?[0-9]+\\([.][0-9]+\\)?" str2)
					 (setq nbr2 (match-string 0 str2))
					 (setq
					  str1 (substring str1 0 (- (length str1) (length nbr1)))
					  str2 (substring str2 (length nbr2)))
					 
					 (setq str (concat str1 (number-to-string (lvznumbers-do-math mop (string-to-number nbr1) (string-to-number nbr2))) str2))
					 t)
				 nil)
			   mp1
			   )
		   )
		 

		 str
		 ))

(defun do-math-on-region ()	"Function for doing all math operations in selected area."
	   (interactive)
	   (if (use-region-p)
		   (let (
				 (cpoint (region-beginning))
				 (str (concat "(" (buffer-substring-no-properties (region-beginning) (region-end)) ")"))
				 (tmp (list))
				 )
			 (while (progn
					  (setq tmp (lvznumbers-find-first-closing-bracket str))
					  (if tmp
						  (progn
							(setq str (concat (nth 0 tmp) (lvznumbers-do-parenthesis (nth 1 tmp)) (nth 2 tmp)))
							))
					  tmp
					  ))
			 (kill-region (region-beginning) (region-end))
			 (goto-char cpoint)
			 (insert str)
			 )
		 (error "There is no selection.")))

(defun do-math-on-kill-ring-and-paste () "Function for doing all math operation in killring"
	   (interactive)
	   (let (
			 (str (concat "(" (substring-no-properties (car kill-ring)) ")"))
			 (tmp (list))
			 )
		 (while (progn
				  (setq tmp (lvznumbers-find-first-closing-bracket str))
				  (if tmp
					  (progn
						(setq str (concat (nth 0 tmp) (lvznumbers-do-parenthesis (nth 1 tmp)) (nth 2 tmp)))
						))
				  tmp
				  ))
		 (kill-new str)
		 (yank)
		 ))

(defun do-math-on-region-and-copy () "Function for doing all math in region and copy result to kill-ring"
	   (interactive)
	   (if (use-region-p)
		   (let (
				 (str (concat "(" (buffer-substring-no-properties (region-beginning) (region-end)) ")"))
				 (tmp (list))
				 )
			 (while (progn
					  (setq tmp (lvznumbers-find-first-closing-bracket str))
					  (if tmp
						  (progn
							(setq str (concat (nth 0 tmp) (lvznumbers-do-parenthesis (nth 1 tmp)) (nth 2 tmp)))
							))
					  tmp
					  ))
			 (kill-new str)
			 )
		 (error "There is no selection.")))
	   
(defun goto-next-number ( x ) "Function for moving the cursor to the next number."
	   (interactive "p")
	   (let (
			 (rpt 1)
			 (i 0)
			 )

		 (if (> x 0) (setq rpt x))
		 (while (< i rpt)
		   (progn
			 (if (looking-at "[0-9]+") (skip-chars-forward "0-9"))
			 (and
			  (looking-at "[^0-9]+[\-]?[0-9]+")
			  (skip-chars-forward "^0-9"))
			 (setq i (1+ i))))))

(defun goto-previous-number ( x ) "Function for moving the cursor to the previous number."
	   (interactive "p")
	   (let (
			 (cpoint (point))
			 (rpt 1)
			 (i 0)
			 )
		 (if (> x 0) (setq rpt x))
		 (while (< i rpt)
		   (progn
			 (if (looking-back "[0-9]+") (skip-chars-backward "0-9"))
			 (skip-chars-backward "^0-9")
			 (skip-chars-backward "0-9")
			 (if (not (looking-at "[0-9]+"))
				 (goto-char cpoint))
			 (setq i (1+ i))))))

(defun region-increase-numbers () "Increase all numbers in selected area."
	   (interactive)
	   (if (use-region-p)
		   (let (
				 (cpoint (region-beginning))
				 (str (buffer-substring-no-properties (region-beginning) (region-end)))
				 )
			 (save-match-data
			   (let ((pos 0)
					 matches)
				 (while (string-match "\\([\\-]?[0-9]+\\)\\([.][0-9]+\\)?" str pos)
				   (setq pos (match-end 0)))
				 matches))
			 )
		 (error "There is no selection.")))

;;---- MINOR MODE -----------------------------------------------------------------

(define-minor-mode lvznumbers-mode "Minor mode for working with numbers and math."
  :lighter " LVz0-9"
  :keymap (let ((lvznumbersmap (make-sparse-keymap)))

			;;---- SHORTCUTS ------------------------------------------------------------------

			(define-key-after		 ; Menu for LVzNumbers mode
			  lvznumbersmap
			  [menu-bar lvznumbersmenu]
			  (cons "LVzNumbers" (make-sparse-keymap "lvznumbers mode"))
			  'kill-buffer
			  )

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenugnn] ; goto next number
			  '("Goto next number" . goto-next-number))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenugpn] ; goto previous number
			  '("Goto previous number" . goto-previous-number))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu separator0] '("--"))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste] ; Submenu for paste operations.
			  (cons "Math operations" (make-sparse-keymap "math operations")))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr] ; Submenu for increments/decrements.
			  (cons "Increments/Decrements" (make-sparse-keymap "increments decrements")))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu separator1] '("--"))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenudomath] ; Menu item for executing function "do-math-on-region" for replacing the selected area with the math results in it.
			  '("Do math on selection" . do-math-on-region))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenucopydivision] ; Menu item for executing function "divide-copy" for replacing the number under cursor with the result of division that number with the number in kill-ring.
			  '("Copy with division" . divide-copy))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenucopymultiplication] ; Menu item for executing function "divide-copy" for replacing the number under cursor with the result of division that number with the number in kill-ring.
			  '("Copy with multiplication" . multiply-copy))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenucopysubtraction] ; Menu item for executing function "divide-copy" for replacing the number under cursor with the result of division that number with the number in kill-ring.
			  '("Copy with subtraction" . subtract-copy))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenucopyaddition] ; Menu item for executing function "divide-copy" for replacing the number under cursor with the result of division that number with the number in kill-ring.
			  '("Copy with addition" . addition-and-copy))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste separator1] '("--"))
			
			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenupastedivision] ; Menu item for executing function "divide-paste" for replacing the number under cursor with the result of division that number with the number in kill-ring.
			  '("Paste with division" . divide-paste))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenupastemultiplication] ; Menu item for executing function "multiply-paste" for replacing the number under cursor with the result of multiplicating that number with the number in kill-ring.
			  '("Paste with multiplication" . multiply-paste))
			
			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenupastesubstraction] ; Menu item for executing function "subtract-paste" for replacing the number under cursor with the result of subtracting the number in kill-ring of that number.
			  '("Paste with subtraction" . subtract-paste))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenupaste lvznumbersmenupasteaddition] ; Menu item for executing function "addition-with-paste" for replacing the number under cursor with the result of adding that number to the number in kill-ring.
			  '("Paste with addition" . addition-with-paste))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenudecrementhex] ; Menu item for executing function "decrement-hex-at-point" for decrement by one the hexadecimal number under the cursor.
			  '("Decrement hexadecimal number" . decrement-hex-at-point))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenuincrementhex] ; Menu item for executing function "increment-hex-at-point" for increment by one the hexadecimal number under the cursor.
			  '("Increment hexadecimal number" . increment-hex-at-point))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr separator2] '("--"))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenudecrementdigit] ; Menu item for executing function "decrement-digit-at-point" for decrement by one the digit right after the cursor.
			  '("Decrement digit" . decrement-digit-at-point))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenuincrementdigit] ; Menu item for executing function "increment-digit-at-point" for increment by one the digit right after the cursor.
			  '("Increment digit" . increment-digit-at-point))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr separator1] '("--"))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenudecrementdecn] ; Menu item for executing function "decrement-number-at-point" for decrement by one the decimal number under the cursor.
			  '("Decrement decimal number" . decrement-number-at-point))

			(define-key lvznumbersmap [menu-bar lvznumbersmenu lvznumbersmenuincr lvznumbersmenuincrementdecn] ; Menu item for executing function "increment-number-at-point" for increment by one the decimal number under the cursor.
			  '("Increment decimal number" . increment-number-at-point))

			(define-key lvznumbersmap (kbd lvznumbers-addition-with-paste-keycomb) 'addition-with-paste)
			(define-key lvznumbersmap (kbd lvznumbers-subtract-paste-keycomb) 'subtract-paste)
			(define-key lvznumbersmap (kbd lvznumbers-multiply-paste-keycomb) 'multiply-paste)
			(define-key lvznumbersmap (kbd lvznumbers-divide-paste-keycomb) 'divide-paste)

			(define-key lvznumbersmap (kbd lvznumbers-addition-and-copy-keycomb) 'addition-and-copy)
			(define-key lvznumbersmap (kbd lvznumbers-subtract-copy-keycomb) 'subtract-copy)
			(define-key lvznumbersmap (kbd lvznumbers-multiply-copy-keycomb) 'multiply-copy)
			(define-key lvznumbersmap (kbd lvznumbers-divide-copy-keycomb) 'divide-copy)

			(define-key lvznumbersmap (kbd lvznumbers-increment-digit-keycomb) 'increment-digit-at-point)
			(define-key lvznumbersmap (kbd lvznumbers-decrement-digit-keycomb) 'decrement-digit-at-point)

			(define-key lvznumbersmap (kbd "C-x +") 'increment-number-at-point)
			(define-key lvznumbersmap (kbd "C-x -") 'decrement-number-at-point)
			(define-key lvznumbersmap (kbd lvznumbers-increment-keycomb) 'increment-number-at-point)
			(define-key lvznumbersmap (kbd lvznumbers-decrement-keycomb) 'decrement-number-at-point)

			(define-key lvznumbersmap (kbd lvznumbers-increment-hex-keycomb) 'increment-hex-at-point)
			(define-key lvznumbersmap (kbd lvznumbers-decrement-hex-keycomb) 'decrement-hex-at-point)

			(define-key lvznumbersmap (kbd lvznumbers-do-math-on-region-keycomb) 'do-math-on-region)
			(define-key lvznumbersmap (kbd lvznumbers-do-math-on-region-keycomb-alt) 'do-math-on-region)

			(define-key lvznumbersmap (kbd lvznumbers-do-killring-and-paste-keycomb) 'do-math-on-kill-ring-and-paste)
			(define-key lvznumbersmap (kbd lvznumbers-do-killring-and-paste-keycomb-alt) 'do-math-on-kill-ring-and-paste)

			(define-key lvznumbersmap (kbd lvznumbers-do-region-and-copy-keycomb) 'do-math-on-region-and-copy)
			(define-key lvznumbersmap (kbd lvznumbers-do-region-and-copy-keycomb-alt) 'do-math-on-region-and-copy)

			(define-key lvznumbersmap (kbd lvznumbers-goto-next-dec-number-keycomb) 'goto-next-number)
			(define-key lvznumbersmap (kbd lvznumbers-goto-previous-dec-number-keycomb) 'goto-previous-number)

			lvznumbersmap)
  :global 1

  (make-local-variable 'lvznumbers-keymap)
  )

(lvznumbers-mode 1)

(provide 'lvznumbers-mode)

;;; lvznumbers-mode.el ends here

;;(use-package lvznumbers-mode
;;			 :ensure t
;;			 :config (global-lvznumbers-mode))
