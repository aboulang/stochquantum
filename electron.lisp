;;; -*- Package: USER; Syntax: Zetalisp -*-
(defvar *p* (make-vec 20 2))
(defvar *q* (make-vec 0 100))
(defvar *e* -100)				;charge
(defvar *m* 1)					;mass
(defvar total-energy)				;total energy of system
(defvar stop nil)
(defvar iter-p 0)
(defvar iter-q 0)
(defvar histo (make-array '(100) :initial-value 0))
(defvar cog nil)
(defvar c-x 400)
(defvar c-y 250)
(defvar *window*)
(setq *window* (tv:window-under-mouse))
(send *window* :set-char-aluf tv:alu-seta)
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

(defmethod (displayer :display) (&optional (stream tv:selected-window) &aux x y)
  (send self :total-energy)
  (send self :histogram cog)
  (send stream :draw-string (format nil "Iter P = ~A         ~
                                         Iter Q = ~A         ~
                                         R = ~A              ~
                                         Total Energy = ~A"
				    iter-p iter-q
				    (vec-y *q*)
				    total-energy) 10 20)
  (setq x (+ c-x (fix (* (vec-y *q*) (cos (vec-x *q*))))))
  (setq y (+ c-y (fix (* (vec-y *q*) (sin (vec-x *q*))))))
  (unless (aref lastx 0) (setf (aref lastx 0) x))
  (unless (aref lasty 0) (setf (aref lasty 0) y))
  (setf (aref lastx 1) x)
  (setf (aref lasty 1) y)
  (send stream :draw-rectangle 3 3 x y tv:alu-seta)
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



(defflavor p-electron () (rule:object-with-process))

;;vec-x does not change
(defmethod (p-electron :calc) ()
  (incf iter-p)
  (incf (vec-y *p*)
	(- (// (expt (vec-x *p*) 2)
	       (* *m* (expt (vec-y *q*) 3)))
	   (// (expt *e* 2) (expt (vec-y *q*) 2)))))


(defmethod (p-electron :do) ()
  (loop until stop
	when (flip 90)
	  do (send self :calc)
	  else do (process-sleep 10)
	do (process-allow-schedule)))

(defmethod (p-electron :measure) (observer)
  (process-sleep 30)
  (process-allow-schedule))

(defflavor q-electron () (rule:object-with-process))


(defmethod (q-electron :calc) ()
  (incf iter-q)
  (incf (vec-x *q*)
	(// (vec-x *p*)
	    (* *m* (expt (vec-y *q*) 2))))
  (incf (vec-y *q*)
	(// (vec-y *p*) *m*)))


(defmethod (q-electron :do) ()
  (loop until stop
	when (flip 90)
	  do (send self :calc)
	  else do (process-sleep 10)
	do (process-allow-schedule)))

(defmethod (q-electron :measure) (observer)
  (process-sleep 30)
  (process-allow-schedule))

;;;Lock sync
(defun test-1 ()
  (declare (special p q d))
  (send tv:selected-window :clear-window)
  (setq p (make-instance 'p-electron))
  (setq q (make-instance 'q-electron))
  (setq d (make-instance 'displayer))
  (send (send p :process) :kill)
  (send (send q :process) :kill)
  (send (send d :process) :kill)
  (send tv:selected-window :draw-circle
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
	until (eq (send tv:selected-window :tyi-no-hang) #\space)))


;;; (200 0), (0 100), 20.0 1.0 is circular
;;;;   p        q      e    m
;;;;Internal looping with randomized skips
(defun test-2 (&optional (stream *window*))
  (declare (special p q d))
  (setq stop nil)
  (send stream :clear-window)
  (fillarray histo '(0))
  (cog)
  (unwind-protect
      (progn
	(setq p (rule:make-pinstance 'p-electron))
	(setq q (rule:make-pinstance 'q-electron))
	(setq d (rule:make-pinstance 'displayer))
	(send p :set-priority 0)
	(send q :set-priority 0)
	(send d :set-priority 0)
	(send p :set-quantum 1)
	(send q :set-quantum 1)
	(send d :set-quantum 0)
	(send stream :draw-circle
	      c-x c-y 5
	      tv:alu-xor)
	(setq *p* (make-vec 200.0 0.0))
	(setq *q* (make-vec 0.0 100.0))
	(setq *e* 20.0)				;charge
	(setq *m* 0.80)
	(setq iter-p 0)
	(setq iter-q 0)
	(send d :do stream)
	(send p :do)
	(send q :do)
	(when (eq (send stream :tyi) #\space) (setq stop t)))
    (send p :kill)
    (send q :kill)
    (send d :kill))
  nil)

;;;Master looper with measurements
;;;Display within loop
(defun test-3 (&optional (stream *window*) &aux dd)
  (declare (special p q d))
  (setq stop nil)
  (send stream :clear-window)
  (fillarray histo '(0))
  (cog)
  (unwind-protect
      (progn
	(setq p (rule:make-pinstance 'p-electron))
	(setq q (rule:make-pinstance 'q-electron))
	(setq d (rule:make-pinstance 'displayer))
	(send p :set-priority 0)
	(send q :set-priority 0)
	(send d :set-priority 0)
	(send p :set-quantum 1)
	(send q :set-quantum 1)
	(send d :set-quantum 1)
	(send stream :draw-circle
	      c-x c-y 5
	      tv:alu-xor)
	(setq *p* (make-vec 200.0 0.0))
	(setq *q* (make-vec 0.0 100.0))
	(setq *e* 20.0)				;charge
	(setq *m* 1.6)
	(setq iter-p 0)
	(setq iter-q 0)
	(setq dd d)
	(setq d (symeval-in-instance dd 'rule:object))
	(send dd :kill)
	;(send d :do stream)
	(loop with char
	      for i from 0
	      do (send p :calc)
	      do (send q :calc)
	      ;when (zerop (mod i 2))
	      do (send d :display stream)
	      when (send stream :listen)
	        do (setq char (send stream :tyi))
	        and when (= char #\space) return nil
	        else when (= char #\p)
		      do (progn (send p :measure d)
				(send stream :draw-string (format nil "Measuring P") 10 50))
		else when (= char #\q)
		      do (progn (send q :measure d)
				(send stream :draw-string (format nil "Measuring Q") 10 50))))
    (setq stop t)
    (send p :kill)
    (send q :kill)
    (send-if-handles d :kill))
  nil)



(defun test-4 (&optional (stream *window*))
  (declare (special p q d))
  (setq stop nil)
  (send stream :clear-window)
  (fillarray histo '(0))
  (cog)
  (unwind-protect
      (progn
	(setq p (rule:make-pinstance 'p-electron))
	(setq q (rule:make-pinstance 'q-electron))
	(setq d (rule:make-pinstance 'displayer))
	(send p :set-priority 0)
	(send q :set-priority 0)
	(send d :set-priority 0)
	(send stream :draw-circle
	      c-x c-y 5
	      tv:alu-xor)
	(setq *p* (make-vec 200.0 0.0))
	(setq *q* (make-vec 0.0 100.0))
	(setq *e* 20.0)				;charge
	(setq *m* .80)
	(setq iter-p 0)
	(setq iter-q 0)
	(send d :do stream)
	(loop with char
	      do (send p :calc)
	      do (send q :calc)
	      when (send stream :listen)
	        do (setq char (send stream :tyi))
	        and when (= char #\space) return nil
	        else when (= char #\p)
		      do (progn (send p :measure d)
				(send stream :draw-string (format nil "Measuring P") 10 50))
		else when (= char #\q)
		      do (progn (send q :measure d)
				(send stream :draw-string (format nil "Measuring Q") 10 50))))
    (setq stop t)
    (send p :kill)
    (send q :kill)
    (send d :kill))
  nil)