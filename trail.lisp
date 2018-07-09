;;; -*- Syntax: Zetalisp; Base: 10. -*-
(defun make-circular-list (length)
  ;; Make a CDR-codes circular list of length LENGTH.
  (if (zerop length)
      nil
      (let* ((list (make-list (1+ length)))
	     (last (nthcdr (1- length) list)))
	(si:%change-list-to-cons last)
	(rplacd last list)
	list)))

(defflavor trail-mixin
	((trail-size 10)			; Length of trail.
	 (trail nil))				; Circular list containing trail.
	(pop-edit-mixin)
  (:initable-instance-variables trail-size)
  (:gettable-instance-variables trail-size)
  (:required-instance-variables position)
  (:documentation
   "Produces a trail of dots that shows where the object has been recently."))

(defmethod (trail-mixin :pop-edit-items) ()
  '((trail-size "Trail Length" :number)))

(defmethod (trail-mixin :pop-edit-constraints) (ignore variable ignore new-value)
  (if (eq variable 'trail-size)
      (send self :set-trail-size new-value))
  nil)

(defmethod (trail-mixin :set-trail-size) (new-trail-size)
  (setq trail-size new-trail-size)
  (setq trail (if (zerop trail-size) nil
		  (make-circular-list trail-size))))

(defmethod (trail-mixin :after :init) (ignore)
  (send self :set-trail-size trail-size))

(defun map-over-circular-list (function list)
  (loop with head = list
	for started = t then nil
	for items on list
	until (and (not started) (eq head items))
	do (funcall function (car items))))

(defmethod (trail-mixin :print-trail) ()
  (map-over-circular-list
    #'(lambda (item) (print item))
    trail))

(defmethod (trail-mixin :trail-head) ()
  (first trail))

(defmethod (trail-mixin :after :display) (stream)
  ;; Draw current position and possibly erase the end of the tail.
  (when (> trail-size 0)
    (when (not *erasing*)
      (send self :draw-trail position stream)
      (when (car trail)				; Previous point, erase it.
	(send self :draw-trail (car trail) stream))
      (setf (car trail) position)
      (pop trail))))

(defmethod (trail-mixin :draw-trail) (the-position stream)
  ;; Draw the tail point.
  (send stream :draw-rectangle 4 4 (x-screen (vec-x the-position))
	(y-screen (vec-y the-position)) tv:alu-xor))

(defmethod (trail-mixin :before :reset) ()
  ;; Start with a fresh trail when you reset.
  (send self :set-trail-size trail-size))