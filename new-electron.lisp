;;; -*- Package: USER; Syntax: Zetalisp -*-
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
(defvar iter-tolerance 1000000)
(defvar *window*)
(setq *window* (tv:make-window 'tv:window) #+ignore (tv:window-under-mouse))
(send *window* :set-char-aluf tv:alu-seta)
;;(angle r)

#||
(defvar ic 1.5)
(defun poly (arg) (- (* arg arg) 1))
(defun polyp (arg) (deriv (- (* arg arg) 1)))
||#

(defvar p-from-q ())
(defvar q-from-p ())

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

(defmethod (:init displayer :after) (&rest ignore)
  (multiple-value-bind (x y)
      (send *window* :inside-size)
    (setq c-x (// x 2))
    (setq c-y (// y 2))))
	
(defmethod (:display displayer) (&optional (stream *window*) markp &aux x y)
  (send self :total-energy)
  ;(send self :histogram cog)
  (send stream :set-label (format nil "Iter P = ~A       ~
                                         Iter Q = ~A       ~
			                 Diff = ~A    ~
                                         R = ~A       ~
                                         Total Energy = ~A"
				    iter-p iter-q
				    (- iter-q iter-p)
				    (vec-y *q*)
				    total-energy)) ; 10 20)
  
  (setq x (+ c-x (fix (* (vec-y *q*) (cos (vec-x *q*))))))
  (setq y (+ c-y (fix (* (vec-y *q*) (sin (vec-x *q*))))))
  (unless (aref lastx 0) (setf (aref lastx 0) x))
  (unless (aref lasty 0) (setf (aref lasty 0) y))
  (setf (aref lastx 1) x)
  (setf (aref lasty 1) y)
  ;(push (list x y) foo)
  (send stream :draw-rectangle 3 3 x y tv:alu-seta)
  (when markp (send stream :draw-char fonts:tr18b markp x y))
  ;;(send stream :draw-wide-curve lastx lasty 3)
  (setf (aref lastx 0) x)
  (setf (aref lasty 0) y))


(defmethod (:histogram displayer) (window &aux index)
  ;;(setq index (fix (// (vec-y *q*) 10)))
  (setq index (fix (+ total-energy 100)))
  (when (< index (array-length histo))
    (incf (aref histo index))
    (when window
      (send window :bar-graph-data index (fix (// (aref histo index) 2))))))
  
(defmethod (:total-energy displayer) ()
  (setq total-energy (- (// (+ (expt (vec-y *p*) 2) (// (expt (vec-x *p*) 2)
							(expt (vec-y *q*) 2)))
			    (* 2 *m*))
			(// (expt *e* 2) (vec-y *q*)))))

#+ignore
(defmethod (:do displayer) (stream)
  (loop until stop
	do (send self :display stream)
	do (process-allow-schedule)
	   ))

(defmethod (:stop displayer) () stop)

(defmethod (:background displayer) ()
  (send self :display *window*))

(defflavor dimension
	((complement-dimension))
	(rule:object-with-process)
  :settable-instance-variables)

(defmethod (:stop dimension) () stop)

(defmethod (:background dimension) ()
  (when t ;(flip 90)
    (send complement-dimension :measure)
    (when (send self :tolerance-ok)
      (send self :calc))))

(defflavor p-electron
	((p (copylist *p*)))
	(dimension)
  :settable-instance-variables)

;;vec-x does not change
(defmethod (:calc p-electron) ()
  (incf iter-p)
  (push (list :q iter-q :p iter-p) q-from-p)
  (incf (vec-y p)
	(- (// (expt (vec-x p) 2)
	       (* *m* (expt (vec-y *q*) 3)))
	   (// (expt *e* 2) (expt (vec-y *q*) 2)))))

(defmethod (:measure p-electron) (&optional markp)
  (declare (special d))
  (setq *p* p)
  (when markp
    (rule:interrupt d :display *window* #\p)
    (process-sleep 600)))


(defmethod (:background p-electron) ()
  (when t ;(flip 99)
    (send complement-dimension :measure)
    (when (send self :tolerance-ok)
      (send self :calc))))

(defmethod (:tolerance-ok p-electron) ()
  (> iter-tolerance  (- iter-p iter-q)))

(defflavor q-electron
	((q (copylist *q*)))
	(dimension)
  :settable-instance-variables)

(defmethod (:calc q-electron) ()
  (incf iter-q)
  (push (list :p iter-p :q iter-q) p-from-q)
  (incf (vec-x q)
	(// (vec-x *p*)
	    (* *m* (expt (vec-y q) 2))))
  (incf (vec-y q)
	(// (vec-y *p*) *m*)))

(defmethod (:measure q-electron) (&optional markp)
  (declare (special d))
  (setq *q* q)
  (when markp
    (rule:interrupt d :display *window* #\q)
    (process-sleep 600)))

(defmethod (:tolerance-ok q-electron) ()
  (> iter-tolerance  (- iter-q iter-p)))

;;;Lock sync
(defun test-1 ()
  (declare (special p q d))
  (setq foo nil)
  (send *window* :expose)
  (send *window* :select)
  (send *window* :clear-window)
  (setq p (make-instance 'p-electron))
  (setq q (make-instance 'q-electron))
  (setq d (make-instance 'displayer))
  (send (send p :process) :kill)
  (send (send q :process) :kill)
  (send (send d :process) :kill)
  (send *window* :draw-circle
	400 400 5
	tv:alu-xor)
  (setq *p* (make-vec 200.0 0.0))
  (setq *q* (make-vec 0.0 100.0))
  (setq *e* 30.)				;charge
  (setq *m* 1)
  (loop
	do (send d :display)
	do (send q :calc)
	do (send p :calc)
	until (eq (send *window* :tyi-no-hang) #\space)))


;;; (200 0), (0 100), 20.0 1.0 is circular
;;;;   p        q      e    m
(defun test-5 (&optional (stream *window*))
  (declare (special p q d))
  (setq stop t)
  (send *window* :expose)
  (send *window* :select)
  (send stream :clear-window)
  (fillarray histo '(0))
  ;(cog)
  ;;(send cog :refresh) (send cog :setup)

  (setq *p* (make-vec 500.0 0.0))
  (setq *q* (make-vec 0.0 100.0))
  (setq *e* 50.0)				;charge
  (setq *m* .80)
  (setq iter-p 0)
  (setq iter-q 0)
  (setq p-from-q nil)
  (setq q-from-p nil)
  (unwind-protect
      (progn
	(setq p (rule:make-pinstance 'p-electron))
	(setq q (rule:make-pinstance 'q-electron))
	(setq d (rule:make-pinstance 'displayer))
	(send p :set-priority 0)
	(send q :set-priority 0)
	(send d :set-priority .85)
	(send p :set-quantum 1)
	(send q :set-quantum 1)
	(send d :set-quantum 1)
	(send (send p :object) :set-complement-dimension q)
	(send (send q :object) :set-complement-dimension p)
	(send stream :draw-circle
	      c-x c-y 5
	      tv:alu-xor)
	;;Start them going
	(setq stop nil)
	(loop with char
	      when (send stream :listen)
	        do (setq char (send stream :tyi))
	        and when (eq char #\space) return nil
	        else when (eq char #\p)
		      do (send p :measure t)
		else when (eq char #\q)
		      do (send q :measure t)))
    (setq stop t)
    (send p :kill)
    (send q :kill)
    (send d :kill))
  nil)