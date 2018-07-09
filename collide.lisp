;;; -*- Syntax: Zetalisp; Base: 10. -*-
;;; COLLIDE-MIXIN -- Allows collisions between objects

(defflavor collide-mixin
	()
	()
  (:method-combination
   (:or :base-flavor-last :interact))
  (:documentation
   "Objects with this mixin interact with other objects
One must provide an :interact message."))

(defmethod (particle :interact) (object)
  (when (typep object 'reflector)
    (reflector-ball-collision object self)
    t))

(defmethod (ball :interact) (object)
  (when (typep object 'ball)
    (ball-ball-collision self object)
    t))

(defmethod (reflector :interact) (object)
  (if (not (typep object 'reflector))
      (send object :send-if-handles :interact self)))

(defun reflector-ball-collision (reflector ball &aux d)
  (cond ((< (setq d (- (send reflector ':position-normal ball)
			(send ball ':size)))
	    0.0)
	 (send ball :reflect-velocity		; Reflect velocity.
	       (send reflector ':perpendicular-vector))
	 (send ball :set-position
	       (v+v (send ball ':position)
		    (v*s (send reflector ':perpendicular-vector)
		         (* 2.0(abs d)))))
	 t)))
	 
(defun ball-ball-collision (ball-1 ball-2 &aux d dir)
  ;; The Physics of the collision is not correct yet, ie Momentum not conserved!!
  (cond ((< (setq d (- (send ball-1 ':distance ball-2)	; Collision?
			(+ (send ball-1 ':size)
			    (send ball-2 ':size))))
	    0.0)
	 (setq dir (send ball-1 ':direction-to ball-2))
	 (send ball-1 :reflect-velocity dir)
	 (send ball-2 :reflect-velocity dir)
	 (send ball-1 :set-position
	       (v+v (send ball-1 :position)
		    (v*s dir (+ d))))
	 (send ball-2 ':set-position
	       (v+v (send ball-2 ':position)
		    (v*s dir (- d))))
	 t)))

