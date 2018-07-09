;;; -*- Syntax: Zetalisp; Package: USER -*-
;;; THE SIMULATION, SIMULATION OBJECTS AND RELATED MIXINS

(defvar $sim nil "The simulation.")
(setq $sim nil)
(defvar $now 0 "The current simulated time.")
(defvar *simulations* nil "List of simulations.")

#|
RESETTABLE-MIXIN

Allows instance-variables to have initial values that are restored by a
:RESET message.  Any flavor mixing this in should provide a
:RESSETTABLE-VARIABLES method that returns a list of instance-variable
names that can be reset.  See examples below.
|#

(defflavor resettable-mixin
    (reset-plist)	; Plist of instance-variables & initial values.
    ()
    (:method-combination
     (:list :base-flavor-last :resettable-variables))
    (:documentation :Mixin
     "Allows initial values to be reset by a :RESET message."))

(defmethod (resettable-mixin :after :init) (init-plist)
    ;; Save the initial values.
  #Q init-plist
   (send self ':reinitialize))

(defmethod (resettable-mixin :reinitialize) ()
    ;; Set up a plist of variables that must be reset.
    (setq reset-plist
	  (loop for variable in
		(loop for variable-list in
		      (send self ':resettable-variables)
		      append variable-list)
		append (list variable
			     (symeval-in-instance self variable)))))

(defmethod (resettable-mixin :before :reset) ()
    ;; Reset variables to their initial values
    (loop for item on reset-plist by 'cddr
	  do (set-in-instance self (first item) (second item))))

(defmethod (resettable-mixin :resettable-variables) ()
    nil)

#||
EXAMPLE OF RESETTABLE-MIXIN

(defflavor foo
    ((x 5)
     (y 20))
    (resettable-mixin)
    :settable-instance-variables
    :initable-instance-variables)

(defmethod (foo :resettable-variables) ()
    '(x y))

(defflavor bar
    ((color ':green)
     (size ':huge))
    (foo resettable-mixin)
    :settable-instance-variables
    :initable-instance-variables)

(defmethod (bar :resettable-variables) ()
    '(color size))
||#
					       
(defflavor basic-simulation
    ((objects nil)		; The objects used in the simulation.
     (duration 0)		; Duration in ticks.
     (stop-p t)			; Should simulation stop.
     (documentation "")		; Description of this simulation.
    )
    (resettable-mixin named-mixin pop-edit-mixin)
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables
    (:documentation "A simulation that runs by simulated wall clock time."))

(defmethod (basic-simulation :choose-item) ()
  `(,name :value ,self :documentation ,documentation))

(defmethod (basic-simulation :pop-edit-items) ()
  '((duration "Duration" :number)))

(defmethod (basic-simulation :resettable-variables) ()
  `(stop-p))

(defmethod (basic-simulation :reset) ()
  ;; Reset simulation to its initial state.  Send each object
  ;; a :RESET.
  (loop for object in objects do
    (send object ':reset))
  (setq $now 0))

(defmethod (basic-simulation :clear-simulation) ()
  ;; Get rid of things, to start a fresh simulation.
  (setq objects nil)
  (send self :reset))

(defmethod (basic-simulation :run) ()
    ;; Run the simulation.
  (send self ':reset)
  (send self ':continue))

(defmethod (basic-simulation :continue) ()
    (loop while (and (not stop-p)
		     (< $now duration))
	  do (send self ':step)))

(defmethod (basic-simulation :before :step) ()
    (send self ':increment-time))

(defmethod (basic-simulation :step) ()
    ;; Take a simulation step.
    (loop for object in objects
	  do (send object ':step)))

(defmethod (basic-simulation :increment-time) ()
    ;; Set simulation time to the next time.  This is a separate
    ;; method so time can change in various ways.
    (setq $now (1+ $now)))

(defmethod (basic-simulation :stop) (&optional (stop t))
    ;; Stop simulation after this step.
    ;; To stop the simulation, send :STOP to $SIM.
    (setq stop-p stop))

(defmethod (basic-simulation :add-object) (object)
    ;; Add OBJECT to the list of OBJECTS.
    (append-member object objects))


(defflavor fiziks-simulation
    ((active-objects nil))	; Objects that move or change shape.
    (basic-simulation))

(defmethod (fiziks-simulation :after :init) (ignore)
  (setq *simulations*
	(cons self
	      (del #'(lambda (a b) (equal (send a :name) (send b :name)))
		   self *simulations*))))

#+ignore
(defmethod (fiziks-simulation :after :increment-time) ()
  ;; Just for debugging.
    (dprint $now))

(defmethod (fiziks-simulation :before :reset) ()
    #Q(send $graphics-stream ':clear-window))

(defmethod (fiziks-simulation :after :reset) ()
    (setq active-objects nil)
    (loop for object in objects do
	  (cond ((typep object 'moveable-mixin)
		 (push object active-objects)))))

(defmethod (fiziks-simulation :before :clear-simulation) ()
  (setq active-objects nil))

(defmethod (fiziks-simulation :step) ()
    ;; Take a simulation step, by erasing and  moving everyone that moves,
    ;; let everyone interact with everyone else.  Then redisplay everyone.
    (loop for object in active-objects do
	  (send object ':erase $graphics-stream))
    (loop for object in active-objects do
	  (send object ':send-if-handles ':move))
    (loop for sublist on objects ; Many body problem.
	  when (typep (car sublist) 'collide-mixin)
	    do
	  (loop with object-1 = (car sublist)
		for object-2 in sublist
		when (not (eq object-1 object-2)) do
		(send object-1 ':interact object-2)))
    (loop for object in active-objects do
	  (send object ':display $graphics-stream)))


(defflavor known-mixin
    ()
    ()
    (:documentation :mixin
		    "Make instance known to the simulation."))

(defmethod (known-mixin :after :init) (init-plist)
  #Q init-plist
    (send $sim ':add-object self))


(defflavor displayable-mixin
    ()
    ()
    (:required-methods :display :reset)
    (:documentation
     "Make item displayable and erasable."))
;;; Messages: :display, erase
;;; Alters messages :reset.

(defmethod  (displayable-mixin :erase) (&optional (graphics-stream $graphics-stream))
    ;; Erase yourself.  Assumes XOR'ing.
     (send self ':display graphics-stream))

(defmethod (displayable-mixin :after :reset) ()
    (send self ':display $graphics-stream))

(defflavor moveable-mixin
    ()
    (displayable-mixin)
    (:required-methods :step)
    (:documentation
     "An instance that can move and display itself needs this."))

(defmethod (moveable-mixin :before :step) ()
    (send self ':erase $graphics-stream))

(defmethod (moveable-mixin :after :step) ()
    (send self ':display $graphics-stream))


;; A basic-simulation-object provides noop :step and :reset methods.
(defflavor basic-simulation-object
    ()
    (named-mixin pop-edit-mixin))

(defmethod (basic-simulation-object :step) ()
  nil)

(defmethod (basic-simulation-object :reset) ()
  nil)

(defflavor simulation-object
    ()
    (known-mixin displayable-mixin basic-simulation-object))


(defflavor moveable-simulation-object
    ()
    (resettable-mixin moveable-mixin simulation-object))

(defmacro defsim (name documentation &body body)
  "Simple defining form for simulations."
  `(progn 'compile
	  (setq ,name
		(setq $sim
		      (make-instance 'fiziks-simulation :name ',name
				     :documentation ,documentation)))
	  ,@body))

