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

;;init grille
;(defparameter *grid*
;  #2A((1 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;     (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)
;      (0 0 0 0 0 0 0 0 0)))


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

;;On demande au joueur de choisir la ligne la colonne et la valeur d'un case
(defun user-read ()
  (let ((l (progn (princ "ligne (entier) ? ") (read)))
	(c (progn (princ "colonne (lettre) ? ") (read)))
	(val (progn (princ "valeur ? ") (read))))
    (if (not-correct (- l 1) (- c 1) val)
	(progn 
	  (format t "valeur existante ligne et/ou colonne et/ou 3x3~C" #\linefeed)
	  (user-read))
	(setf (aref *grid* (- l 1) (- c 1)) val))))

;;On vérifie si la valeur, la ligne et la colonne sont correct
(defun not-correct (l c val)
  (if (or (>= 0 val) (> val AREA) (> 0 l) (> l (- AREA 1)) (> 0 c) (> c (- AREA 1)))
      T
      (progn
	(dotimes (tmpLC AREA)
	  (cond ((or (= (aref *grid* tmpLC c) val) (= (aref *grid* l tmpLC) val))
		 (return-from exit))
		))
	(dotimes (tmpL SIZE)
	  (dotimes (tmpC SIZE)
	    (cond ((< l SIZE)
		   (cond ((and (< c SIZE) (>= c 0) (= (aref *grid* tmpL tmpC) val))
			  (format t " yolo tmpl~D tmpc~D ~C" tmpL tmpC #\linefeed)
			  (return T))
			 ((and (< c (* SIZE 2)) (>= c SIZE) (= (aref *grid* tmpL (+ tmpC SIZE)) val))
			  (format t " yolo tmpl~D tmpc~D ~C" tmpL (+ tmpC SIZE) #\linefeed)
			  (return T))
			 ((and (< c AREA) (>= c (* SIZE 2)) (= (aref *grid* tmpL (+ tmpC (* SIZE 2))) val))
			  (format t " yolo tmpl~D tmpc~D ~C" tmpL (+ tmpC (* SIZE 2)) #\linefeed)
			  (return T)))))))))))

;	(dotimes (tmpL SIZE)
;	  (dotimes (tmpC SIZE)
;	    (if (< l SIZE)
;		(if (< c SIZE)
;		    (if (= (aref *grid* tmpL tmpC) val)
;			(return T)
;;			NIL)
;		    (if (< c (* SIZE 2)) 
;			(if (= (aref *grid* tmpL (+ tmpC SIZE)) val)
;			    (progn
;			      (format t " yolo tmpl~D tmpc~D ~C" tmpL (+ tmpC SIZE) #\linefeed)
;			      (return T))
;;			    NIL)
;			(if (< c AREA)
;			    (if (= (aref *grid* tmpL (+ tmpC (* SIZE 2))) val)
;				(return T)
;;				NIL)
;			    )
;			)
;		    )
;		NIL))))))
;


(defun main ()
  (sudoku)
  (format t "~C" #\linefeed)
  (user-read)
  (main))