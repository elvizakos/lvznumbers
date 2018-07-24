;;; lvznumbers.el --- Extension for working with numbers.
;;; -*- coding: utf-8 -*-

;; Copyright © 2018, Nikos Skarmoutsos

;; Version: 1.2.0
;; Author: Nikos Skarmoutsos <elvizakos AT yahoo DOT gr>
;; Maintainer: Nikos Skarmoutsos
;; Created: May 2018
;; Keywords: numbers,hex,decimal,math
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

;;---- CONSTANTS ------------------------------------------------------------------

(defconst lvznumbers-version "1.2.0" "LVzNumbers version.")

;;---- VARIABLES ------------------------------------------------------------------

(defvar lvznumbers-keymap (make-sparse-keymap) "Keymap for lvznumbers.")

;;---- FUNCTIONS ------------------------------------------------------------------

(defun increment-number-at-point ()	; Συνάρτηση για αύξηση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαδικοί).
  "Συνάρτηση για αύξηση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαδικοί)."
  (interactive)
  (let ((cpoint (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
	(goto-char cpoint)))

(defun increment-digit-at-point () ; Συνάρτηση για αύξηση τιμής μόνο του ψηφίου στον κέρσορα κατά 1.
  "Συνάρτηση για αύξηση τιμής μόνο του ψηφίου στον κέρσορα κατά 1."
  (interactive)
  (or
   (looking-at "[0-9]")
   (error "Not a decimal number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
  (backward-char 1))

(defun decrement-number-at-point ()	; Συνάρτηση για μείωση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαδικοί).
  "Συνάρτηση για μείωση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαδικοί)."
  (interactive)
  (let ((cpoint (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (- (string-to-number (match-string 0)) 1)))
	(goto-char cpoint)))

(defun decrement-digit-at-point () ; Συνάρτηση για μείωση τιμής μόνο του ψηφίου στον κέρσορα κατά 1.
  "Συνάρτηση για μείωση τιμής μόνο του ψηφίου στον κέρσορα κατά 1."
  (interactive)
  (or
   (looking-at "[0-9]")
   (error "Not a decimal number at point"))
  (replace-match (number-to-string (- (string-to-number (match-string 0)) 1)))
  (backward-char 1))

(defun increment-hex-at-point () ; Συνάρτηση για αύξηση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαεξαδικοί).
  "Συνάρτηση για αύξηση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαεξαδικοί)."
  (interactive)
  (let ((cpoint (point)))
	(skip-chars-backward "0-9A-Fa-f")
	(or
	 (looking-at "[0-9A-Fa-f]+")
	 (error "Not a hexadecimal number at point"))
	(replace-match (format "%x" (+ 1 (string-to-number (match-string 0) 16))))
  (goto-char cpoint)))

(defun decrement-hex-at-point () ; Συνάρτηση για μείωση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαεξαδικοί).
  "Συνάρτηση για μείωση τιμής αριθμού στη θέση του κέρσορα κατά 1 (δεκαεξαδικοί)."
  (interactive)
  (let ((cpoint (point)))
	(skip-chars-backward "0-9A-Fa-f")
	(or
	 (looking-at "[0-9A-Fa-f]+")
	 (error "Not a hexadecimal number at point"))
	(replace-match (format "%x" (- (string-to-number (match-string 0) 16) 1)))
  (goto-char cpoint)))

(defun addition-with-paste () ; Πρόσθεση του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου.
  "Πρόσθεση του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου."
  (interactive)
  (let ((start (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (+ (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
	(goto-char start)))

(defun subtract-paste () ; Αφαίρεση του αριθμού στο πρόχειρο από τον αριθμό στη θέση του δρομέα και αντικατάσταση του δεύτερου.
  "Αφαίρεση του αριθμού στο πρόχειρο από τον αριθμό στη θέση του δρομέα και αντικατάσταση του."
  (interactive)
  (let ((start (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (- (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
	(goto-char start)))

(defun multiply-paste () ; Πολλαπλασιασμός του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου.
  "Πολλαπλασιασμός του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου."
  (interactive)
  (let ((start (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (* (string-to-number (match-string 0)) (string-to-number (substring-no-properties (car kill-ring))))))
	(goto-char start)))

(defun divide-paste () ; Διαίρεση του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου.
  "Διαίρεση του αριθμού στη θέση του δρομέα με τον αριθμό στο πρόχειρο και αντικατάσταση του πρώτου."
  (interactive)
  (let ((start (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(or
	 (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?")
	 (error "Not a decimal number at point"))
	(replace-match (number-to-string (/ (+ 0.0 (string-to-number (match-string 0))) (string-to-number (substring-no-properties (car kill-ring))))))
	(goto-char start)))

(defun number-type () ; Επιστρέφει τον τύπο του αριθμού στη θέση του δρομέα ή 'nil' αν δεν υπάρχει αριθμός.
  "Επιστρέφει τον τύπο του αριθμού στη θέση του δρομέα ή 'nil' αν δεν υπάρχει αριθμός."
  (interactive)
  (let ((cpoint (point)))
	(skip-chars-backward "0-9")
	(skip-chars-backward "-")
	(if (not (looking-at "[\-]?[0-9]+\\([.][0-9]+\\)?"))
		(progn
		  (goto-char cpoint)
		  (if (not (looking-at "[0-9a-z]+"))
			  (progn
				)
			(message "hexadecimal")))
	  (message "decimal"))))

(defun lvznumbers-find-first-closing-bracket (str)	; Συνάρτηση για επιστροφή της πρώτης παρένθεσης χωρίς άλλες παρενθέσεις μέσα της. Αν δεν υπάρχει παρένθεση, επιστροφή nil.
  "Συνάρτηση για επιστροφή της πρώτης παρένθεσης χωρίς άλλες παρενθέσεις μέσα της. Αν δεν υπάρχει παρένθεση, επιστροφή nil."
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

(defun lvznumbers-do-math (op n1 n2)	; Πραγματοποίηση πράξης μεταξύ δυο αριθμών.
  "Πραγματοποίηση πράξης μεταξύ δυο αριθμών."
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

(defun lvznumbers-do-parenthesis (str)	; Πραγματοποίηση πράξεων εντός παρένθεσης.
  "Πραγματοποίηση πράξεων εντός παρένθεσης."

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
		  (setq mp1 (string-match "\\([0-9][ \t\n\r]*\\)\\([+—\\-]\\)\\([ \t\n\r]+[\\-]?[0-9]\\)" str))
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

(defun do-math-on-region ()				; Συνάρτηση για πραγματοποίηση μαθηματικών πράξεων στην επιλεγμένη περιοχή.
  "Συνάρτηση για πραγματοποίηση μαθηματικών πράξεων στην επιλεγμένη περιοχή."
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

;;---- SHORTCUTS ------------------------------------------------------------------

;;(define-key lvznumbers-keymap (kbd "C-c C-v m") 'do-math-on-region)

(global-set-key (kbd "C-c C-v +") 'addition-with-paste)
(global-set-key (kbd "C-c C-v -") 'subtract-paste)
(global-set-key (kbd "C-c C-v *") 'multiply-paste)
(global-set-key (kbd "C-c C-v /") 'divide-paste)

(global-set-key (kbd "C-M-z m") 'do-math-on-region)

(global-set-key (kbd "C-x +") 'increment-number-at-point)
(global-set-key (kbd "C-x -") 'decrement-number-at-point)
(global-set-key (kbd "C-c <up>") 'increment-number-at-point)
(global-set-key (kbd "C-c <down>") 'decrement-number-at-point)

(global-set-key (kbd "C-x <up>") 'increment-digit-at-point)
(global-set-key (kbd "C-x <down>") 'decrement-digit-at-point)

(global-set-key (kbd "C-c H") 'increment-hex-at-point)
(global-set-key (kbd "C-x H") 'decrement-hex-at-point)

;; (message "%d" (string-to-number "10" 8)) = 8
;; (message "%d" (string-to-number "101" 2)) = 5


(provide 'lvznumbers)
