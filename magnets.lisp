;;; -*- Package: USER; Fonts: MEDFNT; Base: 10.; Syntax: Zetalisp -*-


;************************************************************

;	POINT-FORCE-SIM-MIXIN

(defflavor POINT-FORCE-SIM-MIXIN
	((force-types))
	(fiziks-simulation)
  :settable-instance-variables)


(defmethod (POINT-FORCE-SIM-MIXIN :BEFORE :RUN) ()
  (loop for force-type in force-types
	do (send self :initialize-force-type force-type)))


(defmethod (FORCE-TYPE-SIM-MIXIN :INITIALIZE-FORCE-TYPE) (force-type)
  (

;************************************************************

;	POINT-BASED-FORCE


;Required method for point-based-force is :GET-FORCE-BETWEEN
;two objects, with first as relevant object. 
(defflavor POINT-BASED-FORCE
	((enforcers nil)
	 (victims nil)
	 (enforcer-properties nil)
	 (victim-properties nil)
	 (constant 0))
	()
  :settable-instance-variables)


(defmethod (POINT-BASED-FORCE :CALCULATE-ACCELERATIONS) (object)
  (cond ((member object enforcers)
	 (loop for victim in victims
	       when (not (equal victim object))
	       collect (send self :get-force-between object victim)))
	((member object victims)
	 (loop for enforcer in enforcers
	       when (not (equal enforcer object))
	       collect (send self :get-force-between object enforcer)))
	(t nil)))


(defmethod (
;************************************************************

;	MAGNETIC FORCE


(defflavor MAGNETIC-FORCE
	((enforcer-properties '(:magnetic))
	 (victim-properties '(:magnetic :iron))
	 (constant 1))
	(point-based-force)
  :settable-instance-variables)


(defmethod (MAGNETIC-FORCE :GET-FORCE-BETWEEN) (object1 object2)
  (// (float (* constant (send object1 :mass) (send object2 :mass)))
      (funcall (lambda (x) (* x x))
	       (v-dist (send object1 :position)
		       (send objec2 :position)))))


(defun V-DIST (vec1 vec2)
  (sqrt (+ (expt (- (vec-x vec1) (vec-x vec2)) 2)
	   (expt (- (vec-y vec1) (vec-y vec2)) 2))))