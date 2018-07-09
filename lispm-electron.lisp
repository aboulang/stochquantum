;;; -*- Package: USER; Syntax: Zetalisp; Fonts: CPTFONT -*-
(defvar *p* (make-vec 20 2))
(defvar *q* (make-vec 0 100))
(defvar *e*)			;charge
(defvar *m*)			;mass
(defvar total-energy)		;total energy of system
(defvar stop nil)
(defvar iter-p 0)
(defvar iter-q 0)
(defvar histo (make-array '(100) :initial-value 0))
(defvar cog nil)
(defvar c-x 400)
(defvar c-y 250)
(defvar iter-tolerance 100)
(defvar *window* nil)

;;(angle r)

(defun flip (&optional (percent 50))
  (< (random 100) percent)) 

;;;+++Make an object-with-process-with-window sometime
(defflavor displayer (
		      (p (make-vec 0 0))	;momentum
		      (q (make-vec 0 0))	;position
		      (lastx (make-array '(2) :initial-value nil))
		      (lasty (make-array '(2) :initial-value nil))
		      )
	   (rule:object-with-process)
  :settable-instance-variables)

(defmethod (displayer :after :init) (&rest ignore)
  (multiple-value-bind (x y)
      (send *window* :inside-size)
    (setq c-x (// x 2))
    (setq c-y (// y 2))))
	
(defmethod (displayer :display) (&optional (stream tv:selected-window) markp &aux x y)
  (send self :total-energy)
  ;;(send self :histogram cog)
  (send stream :draw-string (format nil "Iter P = ~A       ~
                                         Iter Q = ~A       ~
			                 Diff = ~A    ~
                                         R = ~A       ~
                                         Total Energy = ~A"
				    iter-p iter-q
				    (- iter-q iter-p)
				    (vec-y *q*)
				    total-energy) 10 20)
  (setq x (+ c-x (fix (* (vec-y *q*) (cos (vec-x *q*))))))
  (setq y (+ c-y (fix (* (vec-y *q*) (sin (vec-x *q*))))))
  (unless (aref lastx 0) (setf (aref lastx 0) x))
  (unless (aref lasty 0) (setf (aref lasty 0) y))
  (setf (aref lastx 1) x)
  (setf (aref lasty 1) y)
  (send stream :draw-rectangle 3 3 x y tv:alu-seta)
  (when markp (send stream :draw-char fonts:tr18b markp x y))
  ;;(send stream :draw-wide-curve lastx lasty 3)
  (setf (aref lastx 0) x)
  (setf (aref lasty 0) y))


(defmethod (displayer :histogram) (window &aux index)
  (setq index (fix (// (vec-y *q*) 10)))
  (when (< index (array-length histo))
    (incf (aref histo index))
    (when window
      (send window :bar-graph-data index (// (aref histo index) 10)))))
  
(defmethod (displayer :total-energy) ()
  (setq total-energy (- (// (+ (expt (vec-y *p*) 2) (// (expt (vec-x *p*) 2)
							(expt (vec-y *q*) 2)))
			    (* 2 *m*))
			(// (expt *e* 2) (vec-y *q*)))))


(defmethod (displayer :do) (stream)
  (loop until stop
	do (send self :display stream)
	do (process-allow-schedule)
	   ))

(defmethod (displayer :name) ()
  'foo)

(defmethod (displayer :stop) () stop)

(defmethod (displayer :background) ()
  (send self :display *window*))

;;; (200 0), (0 100), 20.0 1.0 is circular
;;;;   p        q      e    m
(defun display-loop (&optional (stream *window*))
  (setq stop nil)
  (setq *window* (tv:window-under-mouse))
  (send *window* :set-char-aluf tv:alu-seta)
  (setq stream *window*)
  (send stream :clear-window)
  (fillarray histo '(0))
  ;;(cog)
  (setq d (rule:make-pinstance 'displayer))
  (send d :set-priority 0)
  (send d :set-quantum 1)
  (send stream :draw-circle
	c-x c-y 5
	tv:alu-xor))
