;;; lvznumbers.el --- Extension for working with numbers.
;;; -*- coding: utf-8 -*-

;; Copyright © 2018, Nikos Skarmoutsos

;; Version: 1.1.0
;; Author: Nikos Skarmoutsos <elvizakos AT yahoo DOT gr>
;; Maintainer: Nikos Skarmoutsos
;; Created: May 2018
;; Keywords: numbers,hex,decimal,math
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

;;---- CONSTANTS ------------------------------------------------------------------

(defconst lvznumbers-version "1.0.0"	; LVzNumbers version.
  "LVzNumbers version.")

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

;;---- SHORTCUTS ------------------------------------------------------------------

(global-set-key (kbd "C-c C-v +") 'addition-with-paste)
(global-set-key (kbd "C-c C-v -") 'subtract-paste)
(global-set-key (kbd "C-c C-v *") 'multiply-paste)
(global-set-key (kbd "C-c C-v /") 'divide-paste)

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
