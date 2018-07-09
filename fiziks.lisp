;;; -*- Syntax: Zetalisp; Base: 10. -*-
(defvar $G 2.0 "Acceleration due to Earth's gravity (velocity/tick).")
(defvar $gravity nil "The Earth's gravitational field.")
(defvar $wall-width 4 "The width of a wall in Pixels.")


(defflavor position-mixin
	(position)
	()
  :settable-instance-variables
  (:documentation "Having a 2-D position, or center."))

(defmethod (position-mixin :distance) (object)
    ;; Distance from me to object.
    (vec-length (send self ':vector-to object)))

(defmethod (position-mixin :vector-to) (object)
    ;; Vector from me to object.
    (v-v (send object ':position) position))

(defmethod (position-mixin :direction-to) (object)
    ;; A unit vector pointing in the direction from me to OBJECT.
    (vec-normalize (send self ':vector-to object)))


(defflavor point
	()
	(position-mixin simulation-object)
  (:documentation "A point in 2-D space."))

(defmethod (point :pop-edit-items) ()
  '((position "Position" :vec)))

(defmethod (point :size) ()
  ;; All points have the same small size.
  4)

(defun make-point (x y)
    ;; Simple way to make a point.
    (make-instance 'point ':position (make-vec x y)))

(defmethod (point :display) (&optional(graphics-stream $graphics-stream))
    (with-cracked-vec
     (position x y)
  (send graphics-stream ':draw-circle  (x-screen x) (y-screen y)
	(send self :size) tv:alu-xor)))


(defflavor velocity-mixin
	((velocity (make-vec 0.0 0.0)))		; At rest.
	()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:documentation "Having a velocity."))

(defmethod (velocity-mixin :pop-edit-items) ()
  '((velocity "Velocity" :vec)))

(defmethod (velocity-mixin :reflect-velocity) (direction)
  ;; Refelect velocity about a plane with unit normal DIRECTION.
    (setq velocity (vec-reflect velocity direction)))

(defmethod (velocity-mixin :speed) ()
    (vec-length velocity))

(defmethod (velocity-mixin :direction-vector) ()
  ;; Easier to compute with than :DIRECTION.
    (vec-normalize velocity))

(defmethod (velocity-mixin :direction) ()
    ;; Direction of motion, counter clockwise from verticle (degrees).
    (let ((direction (send self ':direction-vector)))
	(deg (atan (vec-x direction)
		     (vec-y direction)))))

(defmethod (velocity-mixin :set-speed) (new-speed)
    ;; Change speed to NEW-SPEED.
    (setq velocity
	  (v*s velocity (//$ new-speed
			  (vec-length velocity)))))

(defmethod (velocity-mixin :set-direction) (new-direction &aux old-speed)
    ;; Change direction to NEW-DIRECTION.
    (setq old-speed (send self ':speed)
	  velocity
	  (v*s (make-vec (sin (rad new-direction))
			 (cos (rad new-direction)))
	    old-speed)))


(defflavor particle
	()
	(trail-mixin
	 collide-mixin
	 velocity-mixin
	 moveable-simulation-object
	 point)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:documentation "Particles are points that move."))

(defmethod (particle :move) ()
    ;; Things in motion, tend to remain in motion.
    (setq position (v+v position velocity)))

(defmethod (particle :after :display) (&optional (graphics-stream $graphics-stream))
    (with-cracked-vec
     (position x y)
     (send graphics-stream ':draw-line (x-screen x) (y-screen y)
	   (x-screen (+ x (vec-x velocity)))
	   (y-screen (+ y (vec-y velocity)))
	   tv:alu-xor)))
		    
(defmethod (particle :resettable-variables) ()
    '(position velocity))

(defmethod (particle :step) ()
    (send self ':move))


(defflavor forces-mixin
    ((forces nil))		; List of forces acting on self.
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables
    (:required-methods :velocity :set-velocity :acceleration :move)
    (:documentation
     "Lets an object be influenced by several forces."))

(defmethod (forces-mixin :before :move) ()
    ;; F = Ma.
  (send self :set-velocity
	(v+v (send self :velocity) (send self :acceleration))))

(defmethod (forces-mixin :force) ()
    ;; Total force acting on me.
    (loop for force in forces
	  with total-force = (make-vec 0.0 0.0) do
	  (setq total-force
		(v+v total-force (send force :force-on self)))
	  finally (return total-force)))

(defmethod (forces-mixin :add-force) (force)
    ;; Add FORCE to the list of forces acting on me.
  (unless (memq force forces)
    (push force forces)))


(defflavor mass-mixin
    (mass)
    (forces-mixin)
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables
    (:required-instance-variables velocity)
    (:documentation
     "Give an object mass, and thus is influenced by Gravity Field."))

(defmethod (mass-mixin :pop-edit-items) ()
  '((mass "Mass" :number)))

(defmethod (mass-mixin :after :init) (ignore)
    (send self ':add-force $gravity))

(defmethod (mass-mixin :acceleration) ()
    ; F = MA.
    (v//s (send self ':force) mass))

(defmethod (mass-mixin :momentum) ()
  (v*s velocity mass))

(defmethod (mass-mixin :set-momentum) (new-momentum)
  ;; Assuming conservation of mass.
  (setq velocity (v//s new-momentum mass)))


(defflavor force
    ()
    ()
  (:required-methods :force-on))

(defflavor gravity-field
    ((g (make-vec 0.0 (-$ $G))))
    (force)
    :initable-instance-variables
    :settable-instance-variables)

(defmethod (gravity-field :force-on) (object)
    (v*s g (send object ':mass)))

(setq $gravity (make-instance 'gravity-field))



(defflavor ball
    (size
     color)
    (particle)
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

(defmethod (ball :pop-edit-items) ()
  '((color "Color" :choose (clear black))))

#Q
(defmethod (ball :display) (&optional (graphics-stream $graphics-stream))
    (with-cracked-vec
     (position x y)
      #+ignore
      (send graphics-stream ':draw-line		; Draw velocity vector.
	    (x-screen x) (y-screen y)
	    (x-screen (+$ x (vec-x velocity)))
	    (y-screen (+$ y (vec-y velocity))) tv:alu-xor)
     (send graphics-stream (if (eq color 'clear) ':draw-circle
				':draw-filled-in-circle)
	   (x-screen x) (y-screen y) size tv:alu-xor)))

(defflavor heavy-ball
	()
	(mass-mixin ball))


(defflavor linear-mixin
    (from
     to)
    ()
    :initable-instance-variables
    :gettable-instance-variables
    (:documentation
     "For linear objects that move."))

(defmethod (linear-mixin :pop-edit-items) ()
  '((from "From" :pop-edit)
    (to "To" :pop-edit)))

(defun get-position (thing)
    ; Return the position from a VEC or an instance.
    ;! Not really needed, since everything in fiziks is an instance!!
    (cond ((is-vec thing) thing)
	  ((typep thing 'point)
	   (send thing ':position))))

(defmethod (linear-mixin :length) ()
    (vec-length (v-v (get-position from)
		     (get-position to))))

(defmethod (linear-mixin :other-end) (an-end)
    (cond ((eq an-end from) to)
	  ((eq an-end to) from)
	  (t nil))) ; Error!

(defmethod (linear-mixin :direction-vector) ()
    (send self ':direction-from from)) ;! Not Efficient.

(defmethod (linear-mixin :direction-from) (an-end)
    ;; direction vector from one end to the other.
    (vec-normalize (v-v (get-position
			 (send self ':other-end an-end))
			(get-position an-end))))

(defmethod (linear-mixin :transform) (position &optional outp)
  ;; A line segement defines a coordinate axis, with from as the origin
  ;; and the y axis along the line.  When given a position, :TRANSFORM
  ;; transforms it into this new coordinate system.  The vec-y is the
  ;; distance along the line, and the vec-x is the distance perpendicular
  ;; to it.  Negative vec-x means the point is to the left of the line.
  (let ((dir (send self ':direction-vector))
	(point (v-v (get-position position)
		    (get-position from))))
    (cond (outp (setf (vec-x dir) (-$ (vec-x dir)))))	; Inverse.
    (make-vec (-$ (*$ (vec-y dir) (vec-x point))	; Rotate.
		  (*$ (vec-x dir) (vec-y point)))
	      (+$ (*$ (vec-x dir) (vec-x point))
		  (*$ (vec-y dir) (vec-y point))))))


(defflavor static-linear-mixin
    (direction-vector		; Unit vector along line
     perpendicular-vector	; Unit vector perpendicular to line.
     				; Points to left of line.
     length			; Line length.
     origin-position)
     (linear-mixin)
  :gettable-instance-variables
     (:documentation
      "Linear mixin for things that done' move, doing less computing."))

(defmethod (static-linear-mixin :after :init) (ignore)
    (setq length (send from ':distance to)
	  direction-vector (send self ':direction-from from)
	  perpendicular-vector (vec-perpendicular direction-vector)
	  origin-position  (v*v (send from ':position)
			     perpendicular-vector)))

(defmethod (static-linear-mixin :position-normal) (position-object)
    ;; Position of a point normal to the line.  Positive means to left.
    (-$ (v*v (send position-object ':position) perpendicular-vector)
      origin-position))

(defmethod (static-linear-mixin :position-along) (position-object)
    ;; The position, P, of a point, along the line.
    ;; 0 <= P <= LENGTH if point is between the endpoints of the line.
    (v*v (v-v (send position-object ':position)
	      (get-position from))
      direction-vector))

(defflavor spring
    (spring-constant
     rest-length)
    (force linear-mixin moveable-simulation-object)
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables
    (:documentation "A massless spring."))

(defmethod (spring :pop-edit-items) ()
  '((spring-constant "Spring Constant" :number)
    (rest-length "Rest Length" :number)))

#Q
(defmethod (spring :display) (graphics-stream)
  (with-cracked-vec
    ((send from ':position) x1 y1)
    (with-cracked-vec
      ((send to ':position) x2 y2)
      (send graphics-stream ':draw-line
	    (x-screen x1) (y-screen y1)
	    (x-screen x2) (y-screen y2) tv:alu-xor)))) ;; Should be dotted!

#+franz
(defmethod (spring :display) (graphics-stream)
    (format graphics-stream "~s ~a ~%" self (send self ':length)))

(defmethod (spring :after :init) (plist)
    #Q plist
    ;; Inform end points about spring forces
    (if from (send self ':set-from from))
    (if to (send self ':set-to to)))

(defmethod (spring :after :set-from) (new-from)
    (send new-from ':send-if-handles ':add-force self))

(defmethod (spring :after :set-to) (new-to)
    (send new-to ':send-if-handles ':add-force self))

(defmethod (spring :force-on) (object)
    (cond ((not (memq object (list from to)))
	   (make-vec 0.0 0.0))
	  (t (v*s (send self ':direction-from object)
	          (*$ spring-constant  (-$ (send self ':length)
				        rest-length))))))


(defflavor reflector
    ()
    (static-linear-mixin collide-mixin)
  (:documentation "A linear reflector."))


(defflavor wall
    ((width $wall-width))
    (reflector simulation-object)
  :initable-instance-variables)
#Q
(defmethod (wall :display) (&optional (graphics-stream $graphics-stream))
    (with-cracked-vec
     ((send from ':position) x1 y1)
     (with-cracked-vec
      ((send to ':position) x2 y2)
      (send graphics-stream ':draw-wide-curve
	    (fillarray (make-array 2) (list (x-screen x1) (x-screen x2)))
	    (fillarray (make-array 2) (list (y-screen y1)
					    (y-screen y2)))
	    width))))
#+franz
(defmethod (wall :display) (&optional (graphics-stream $graphics-stream))
    (format t "~s from ~s to ~s~%"
	    self (send from ':position)
	    (send to ':position)))
		 
