;;; -*- Syntax: Zetalisp; Base: 10.; Package: USER -*-

;************************************************************

;	GRAPH DISPLAY ROUTINES

(defflavor GRAPH-WINDOW
	((origin-x 30)   ;Where the x-origin is inside the window
	 (origin-y 10)   ;Where the y-origin is.  Watch out--reverse order.

	 (x-start 0)	 ;value where the x-axis begins
	 (x-end 150)     ;value where the x-axis ends
	 (x-inc 10)	 ;x tick and label increment
	 (x-factor 5)    ;pixels per x value

	 (y-start 0)     
	 (y-end 80)
	 (y-inc 10)
	 (y-factor 1)
	 )
	(tv:window tv:borders-mixin tv:top-label-mixin)
  :settable-instance-variables)




;************************************************************

;	GRAPHICAL SETUP


(defun MAKE-GRAPH-WINDOW (left top right bottom
			    border-width label)
  "Create a display window with the given attributes"
  (let ((d-win
	  (tv:make-window 'graph-window
			  :blinker-p nil
			  :edges-from :mouse)))
    ;;(send d-win :set-edges left top right bottom)
    (send d-win :set-borders border-width)
    (send d-win :set-label `(:top :string ,label))
    (send d-win :set-origin-y (- (send d-win :inside-height) 20))
    d-win))



(defmethod (GRAPH-WINDOW :TRANSFORM-COORDINATES) (x y)
  (values (loop for x-coord from origin-x by x-factor
		for x-value from x-start to x-end
		when (>= x-value x)
		  do (return x-coord)
		finally (return x-coord))
	  (loop for y-coord from origin-y downto 15 by y-factor
		for y-value from y-start
		when (>= y-value y)
		do (return y-coord)
		finally (return y-coord))))

(defmethod (GRAPH-WINDOW :SETUP) ()
  (send self :draw-x-axis)
  (send self :draw-y-axis))


(defmethod (GRAPH-WINDOW :DRAW-X-AXIS) ()
  (send self :draw-line origin-x origin-y
	(send self :transform-coordinates x-end 0) origin-y)
  (loop	for x from (+ x-start x-inc) to x-end by x-inc
	do (let ((x-coord (send self :transform-coordinates x 0)))
	     (send self :draw-line
		 x-coord (+ origin-y 2)
		 x-coord (- origin-y 2))
	     (send self :set-cursorpos (- x-coord 10) (+ origin-y 5))
	     (send self :string-out (x-space-out x)))))


(defmethod (GRAPH-WINDOW :DRAW-Y-AXIS) ()
  (send self :draw-line origin-x origin-y
	origin-x 10)
  (loop for y from (+ y-start y-inc) to y-end by y-inc
	do (multiple-value-bind (x-coord y-coord)
	       (send self :transform-coordinates 0 y)
	     (send self :draw-line
		 (+ origin-x 2) y-coord
		 (- origin-x 2) y-coord)
	   (send self :set-cursorpos (- origin-x 35) (- y-coord 4))
	   (send self :string-out (y-space-out y)))))


(defun X-SPACE-OUT (num)
  (cond ((< num 10) (format nil " ~a " num))
	((< num 100) (format nil " ~a" num))
	(t (format nil "~a" num))))

(defun Y-SPACE-OUT (num)
  (cond ((< num 10) (format nil "  ~a" num))
	((< num 100) (format nil " ~a" num))
	(t (format nil "~a" num))))

;************************************************************

;	BAR GRAPH AND LINE GRAPH ROUTINES


(defmethod (GRAPH-WINDOW :BAR-GRAPH-DATA) (x y)
  (multiple-value-bind (new-x new-y)
      (send self :transform-coordinates x y)
    (send self :draw-line new-x origin-y new-x new-y)
    (send self :draw-line new-x new-y (+ x-factor new-x) new-y)
    (send self :draw-line (+ x-factor new-x) origin-y
	  (+ x-factor new-x) new-y)))


;************************************************************

;	DEBUG ROUTINES



(defvar COG nil)

(defun STRING-SPACE (num)
  (apply 'string-append
    (loop for n below num collect " ")))
	
(defun COG ()
  (if cog t (setq cog
		  (make-graph-window
		    5 200 1050 320 4
		    (string-append (string-space 40) "Density Histogram"))))
  (send cog :expose)
  (send cog :setup))