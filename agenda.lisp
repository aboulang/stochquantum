;;; -*- Mode: Lisp; Package: User; Base: 10 -*-

;So code looks more like Sussman's
(defmacro set-car! (the-cons the-value)
  `(rplaca ,the-cons ,the-value))

(defmacro set-cdr! (the-cons the-value)
  `(rplacd ,the-cons ,the-value))

;The agenda and bits and pieces needed for its implementation


;First, the queue

(defflavor queue
	((front-ptr nil)
	 (rear-ptr nil))
	()
  :settable-instance-variables)

(defmethod (queue :empty?) ()
  (null front-ptr))

 (defmethod (queue :insert!) (item)
  (let ((new-pair (cons item nil)))
    (cond ((null front-ptr)			;empty queue?
	   (setq front-ptr new-pair
		 rear-ptr new-pair))
	  (t					;otherwise ...
	   (set-cdr! rear-ptr new-pair)
	   (setq rear-ptr new-pair)))))

(defmethod (queue :delete!) ()
  (cond ((null front-ptr)
	 (ferror nil "Attempt to delete from empty queue ~a"
		 self))

	(t
	 (setq front-ptr (cdr front-ptr))	;lop off first item
	 )))

(defmethod (queue :front) ()
  (if (null front-ptr)
      (ferror nil "Attempt to retrieve front item from empty queue ~a" self)
      (car front-ptr)))

(defmethod (queue :pop!) ()
  ;; KRA:  Pop the first element off the queue.
  (prog1 (send self ':front)
	 (send self ':delete!)))

(defmethod (queue :empty!) ()
  ;; KRA:  Make yourself empty.
  (setq front-ptr nil rear-ptr nil))

;Time segment

(defflavor time-segment
	(time queue)
	()
  :settable-instance-variables)

(defun make-time-segment (time queue)
  (make-instance 'time-segment
		 ':time time
		 ':queue queue))

;The agenda itself


(defflavor agenda
	((segments `(,(make-time-segment 0 (make-instance 'queue)))))
	()
  :settable-instance-variables)

(defmethod (agenda :first-segment) ()
  (car segments))

(defmethod (agenda :rest-segments) ()
  (cdr segments))

(defmethod (agenda :current-time) ()
  (send (car segments) ':time))

(defmethod (agenda :set-current-time) (new-time)
  (send (car segments) ':set-time new-time))

(defmethod (agenda :empty?) ()
  (and (send (send (car segments) ':queue)
	     ':empty?)
       (null (cdr segments))))

(defmethod (agenda :add-to-agenda!) (time recipient)
  (send self ':add-to-agenda-2 time recipient segments))

(defmethod (agenda :add-to-agenda-2) (time recipient segs)
  (if (= (send (car segs) ':time)
	 time)
      (send (send (car segs) ':queue)
	    ':insert!
	    recipient)
      (cond ((null (cdr segs))
	     (insert-new-time! time recipient segs))
	    ((> (send (cadr segs) ':time)
		time)
	     (insert-new-time! time recipient segs))
	    (t
	     (send self ':add-to-agenda-2 time recipient (cdr segs))))))

(defun insert-new-time! (time recipient segments)
  (let ((q (make-instance 'queue)))
    (send q ':insert! recipient)
    (set-cdr! segments
	      (cons (make-time-segment time q)
		    (cdr segments)))))

(defmethod (agenda :remove-first-agenda-item!) ()
  (send (send (car segments) ':queue)
	':delete!))

(defmethod (agenda :first-agenda-item) ()
  (let ((q (send (car segments) ':queue)))
    (cond ((send q ':empty?)
	   (setq segments (cdr segments))
	   (send self ':first-agenda-item))
	  (t
	   (send q ':front)))))

(defmethod (agenda :after-delay) (delay recipient)
  (send self ':add-to-agenda!
	(+ delay (send self ':current-time))
	recipient))