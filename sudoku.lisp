(defvar SIZE 3)
(defvar AREA (* SIZE SIZE))

;;init grille
(defparameter *grid*
  #2A((1 0 0 0 0 4 0 0 5)
      (0 0 0 9 5 0 0 8 0)
      (0 0 0 0 0 3 0 9 0)
      (0 0 5 0 0 2 0 0 4)
      (0 0 1 0 6 0 7 0 0)
      (7 0 0 3 0 0 2 0 0)
      (0 6 0 5 0 0 0 0 0)
      (0 8 0 0 1 6 0 0 0)
      (5 0 0 2 0 0 0 0 7)))

;;fonction qui nous affiche la grille
(defun sudoku() 
  (format t "   | A B C | D E F | G H I |~C" #\linefeed)
  (dotimes(tmp (+ (* AREA SIZE) 1))
    (format t "-"))
  (dotimes (l AREA)
    (format t "~C ~D | " #\linefeed (+ l 1))
    (dotimes (c AREA)
      (let ((nb (aref *grid* l c)))
	(if (zerop (mod (+ c 1) SIZE))
	    (if (zerop nb)
		(format t "  | ")
		(format t "~D | " nb))
	    (if (zerop nb)
		(format t "  ")
		(format t "~D " nb)))))
      (if (zerop (mod (+ l 1) SIZE))
	  (progn 
	    (format t "~C" #\linefeed)
	    (dotimes (tmp (+ (* AREA SIZE) 1))
	      (format t "-")))
	  NIL)))

(defun user-read ()
  (let ((l (progn (princ "ligne (entier) ? ") (read)))
	(c (progn (princ "colonne (lettre) ? ") (read)))
	(val (progn (princ "valeur ? ") (read))))
    (if (not (is-correct (- l 1) (- c 1) val))
	(user-read)
	(setf (aref *grid* (- l 1) (- c 1)) val))))

(defun is-correct (l c val)
  (if (or (or (>= 0 val) (> val AREA)) (or (> 0 l) (> l (- AREA 1))) (or (> 0 c) (> c (- AREA 1))))
      NIL
      T))
      