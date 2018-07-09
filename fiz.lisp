;;; -*- Base: 10.; Syntax: Zetalisp -*-
;;; Some Example Simulations.
(defsim sim-1 
	"A simulation with 4 walls and 2 balls moving at constant velocity."
  (setq height 500 width 500)
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
  
  (setq b (make-instance 'heavy-ball
			 ':name 'b
			 ':position (make-vec 200 200)
			 ':mass 10
			 ':velocity (make-vec 10 0) ':size 50
			 ':color 'clear))
  
  (setq c (make-instance 'heavy-ball
			 ':name 'c
			 ':position (make-vec 400 200)
			 ':mass 20
			 ':velocity (make-vec -10 0) ':size 50
			 ':color 'solid))
  (setq p (make-instance 'particle ':name 'p
			 :position (make-vec 100 200)
			 :velocity (make-vec 10 10))))

(defsim sim-2
	"A simulation with 2 balls and a particle in a triangle."
  (setq height 500 width 500)
  
  ;; Some points to build walls with.
  (setq p1 (make-point 10 10)
	p2 (make-point width 10)
	p3 (make-point (// width 2) height))
  
  ;; The 4 walls.
  
  (setq bottom (make-instance 'wall ':name 'bottom ':from p1 ':to p2)
	left (make-instance
	       'wall :name 'left ':from p3 ':to p1)
	right (make-instance 'wall ':name 'left ':from p2 ':to p3))
  
  
  (setq b (make-instance 'heavy-ball
			 ':name 'b
			 ':position (make-vec 200 200)
			 ':mass 10
			 ':velocity (make-vec 10 0) ':size 50
			 ':color 'clear))
  
  (setq c (make-instance 'heavy-ball
			 ':name 'c
			 ':position (make-vec 350 200)
			 ':mass 20
			 ':velocity (make-vec -10 0) ':size 50
			 ':color 'solid))
  (setq p (make-instance 'particle ':name 'p
			 :position (make-vec 100 200)
			 :velocity (make-vec 10 10))))

(defsim sim-3
	"A simulation with 4 walls and a ball moving at constant velocity."
  
  (setq height 500 width 500)
  ;; Some points to build walls with.
  (setq p1 (make-point 10 10)
	p2 (make-point width 10)
	p3 (make-point width height)
	p4 (make-point 10 height))
  
  ;; The 4 walls.
  (setq bottom (make-instance 'wall ':from p1 ':to p2)
	right (make-instance
		'wall ':from p2 ':to p3)
	top (make-instance 'wall ':from p3 ':to p4)
	left (make-instance 'wall ':from p4 ':to p1))
  
  (setq b (make-instance 'heavy-ball ':position (make-vec 10 10)
			 ':mass 10
			 ':velocity (make-vec 10 10) ':size 50
			 ':color 'clear)))

(defsim sim-4
	"A simulation with a ball on a spring and a wall."
  (setq p-1 (make-point 300 10))
  (setq p-2 (make-point 300 300))
  (setq w (make-instance 'wall
			 ':from p-1 ':to p-2))
  
  
  (setq m (make-instance 'heavy-ball
			 ':color 'red ':size 30
			 ':mass 5
			 ':position (make-vec 100 100)))
  
  (setq p (make-instance 'point
			 ':position (make-vec 200 200)))
  
  
  (setq s (make-instance 'spring
			 ':from p
			 ':to m
			 ':spring-constant 5
			 ':rest-length (+$ 5 (send m ':distance p))))
  
  #+ignore(setq b (make-instance 'heavy-ball
				 ':color 'clear ':size 100
				 ':mass 0.005
				 ':position (make-vec 0 0)
				 ':velocity (make-vec 20.0 40.0)))
  )

(defsim sim-5 
	"A simulation with 4 walls and particle."
  (setq height 500 width 500)
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
  
 #+ignore  (setq b (make-instance 'heavy-ball
			 ':name 'b
			 ':position (make-vec 200 200)
			 ':mass 10
			 ':velocity (make-vec 10 0) ':size 50
			 ':color 'clear))
  
 #+ignore  (setq c (make-instance 'heavy-ball
			 ':name 'c
			 ':position (make-vec 400 200)
			 ':mass 20
			 ':velocity (make-vec -10 0) ':size 50
			 ':color 'solid))
  (setq p (make-instance 'particle ':name 'p
			 :position (make-vec 100 200)
			 :velocity (make-vec 10 10))))





(defsim sim-6
	"A simulation with a 2 ball on a spring."

  (setq height 500 width 500)
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
  

  (setq p (make-instance 'point
			 ':position (make-vec 200 200)))


  (setq m2 (make-instance 'heavy-ball
			 ':color 'red ':size 10
			 ':mass 10
			 ':position (make-vec 200 100)))
  
  
  (setq m1 (make-instance 'heavy-ball
			 ':color 'red ':size 10
			 :mass 1
			 ':position (make-vec 100 100)
			 :velocity (make-vec 10 0)))
  
  (setq s (make-instance 'spring
			 ':from p
			 ':to m2
			 ':spring-constant 2
			 ':rest-length (+$ -1 (send p ':distance m2))))
  )