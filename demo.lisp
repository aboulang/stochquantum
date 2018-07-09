;;; -*- Package: USER; Fonts: MEDFNT; Base: 10.; Syntax: Zetalisp -*-

(defflavor UPDATABLE-FIZIKS-SIMULATION
	()
	(fiziks-simulation
	 update-mixin)
  )


(defflavor UPDATABLE-HEAVY-BALL
	()
	(heavy-ball
	 updatable-object-mixin)
  )

(defmethod (UPDATABLE-HEAVY-BALL :AFTER :MOVE) ()
  (send self :erase)
  (update-set 'velocity (v+v velocity (make-vec .1 0)))
  (send self :display))

(defun d1 ()
    ;; A simulation with 4 walls and 2 balls moving at increasing velocity.
    (setq height 500 width 500)
    (setq sim-1
	  (setq $sim (make-instance
		       'updatable-fiziks-simulation ':duration 300)))

    ;; Some points to build walls with.
    (setq p1 (make-point 10 10)
	  p2 (make-point width 10)
	  p3 (make-point width height)
	  p4 (make-point 10 height))

    ;; The 4 walls.
    (setq bottom (make-instance 'wall ':name 'bottom ':from p1 ':to p2)
	  right (make-instance
		 'wall ':name 'right ':from p2 ':to p3)
	  top (make-instance 'wall ':name 'top ':from p3 ':to p4)
	  left (make-instance 'wall ':name 'left ':from p4 ':to p1))

    (setq b (make-instance 'updatable-heavy-ball
			   ':name 'b
			   ':position (make-vec 200 200)
			   ':mass 10
			   ':velocity (make-vec 10 0) ':size 50
			   ':color 'clear))

    (setq c (make-instance 'updatable-heavy-ball
			   ':name 'c
			   ':position (make-vec 400 200)
			   ':mass 20
			   ':velocity (make-vec -10 0) ':size 50
			   ':color 'solid))
    (send sim-1 :set-updatable-objects (list b c))  ;makes a strange picture
    (send sim-1 :run))