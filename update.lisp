;;; -*- Package: USER; Fonts: MEDFNT; Base: 10.; Syntax: Zetalisp -*-


;This file contains the update mixin for the fiziks system.  The update system
;allows for handling simultaneity by postponing effects until the next round
;of updating between steps.  The update routine begins operation after
;the current round of steps.  It then sends each object an 
;update-sets message, then sends each object an update-evals message.  
;If there were any update-evals, it cycles again until there aren't any.

;Objects with the updatable-object-mixin have a stack of variables to
;set on the next update round, encoded as a list of
;specs of the form (variable-name value).  They also have a list of expressions
;to evaluate on the next update.

;The idea behind this system is that, in each round of stepping,
;the current values will change, but we defer changing them until the next update
;round, so everybody gets a look at the current values before the changes
;are made.  After the changes, another round of update may occur.  And so on.

;**************************************************

;	Flavor Definitions

(defflavor UPDATE-MIXIN  
	((updatable-objects))
	(basic-simulation)
  :settable-instance-variables)


;NOTE:  Each updatable object in a simulator using update-mixin must have the
;updatable-object-mixin.
(defflavor UPDATABLE-OBJECT-MIXIN
	((dset-stack nil)
	 (deval-stack nil))
	()
  :settable-instance-variables)


;**************************************************

;	Methods


(defmethod (UPDATE-MIXIN :AFTER :STEP) ()
  "Carry out updates relevant to this step of the simulation"
  (loop while (send self :effective-update-cycle)))


(defmethod (UPDATE-MIXIN :EFFECTIVE-UPDATE-CYCLE) ()
  "Carry out a cycle of setting, then evaling.  If anything was evaled, return t"
  (loop for object in updatable-objects
	do (send object :update-sets))
  (loop for object in updatable-objects
	with update-flag = nil
	do (if (send object :update-evals) (setq update-flag t))
	finally (return update-flag)))

(defmethod (UPDATABLE-OBJECT-MIXIN :UPDATE-SETS) ()
  "Do all the setting on the set stack."
  (loop for item in dset-stack
	do (set-in-instance self (car item) (cadr item)))
  (setq dset-stack nil))

(defmethod (UPDATABLE-OBJECT-MIXIN :UPDATE-SETS) ()
  "Do all the setting on the set stack."
  (loop for (locative . value) in dset-stack
	do (rplaca locative value))
  (setq dset-stack nil))

(defmethod (UPDATABLE-OBJECT-MIXIN :UPDATE-EVALS) ()
  "Do all the evaling on the eval stacks.  If anything was there, return t"
  (loop for item in deval-stack
	do (eval item))
  (if (null deval-stack)
      nil
      (setq deval-stack nil)
      t))

(defun-method UPDATE-SET UPDATABLE-OBJECT-MIXIN (variable value)
  "Set the value internally, but not now--put it on the stack"
  (push (list variable value) dset-stack))


(defun-method UPDATE-EVAL UPDATABLE-OBJECT-MIXIN (item)
  "Eval the expression, but not now--put it on the stack"
  (push item deval-stack))

(defvar *delayed-sets* nil "A list of (locative . value) used by dset.")

(defmacro dset (variable value)
  "Delayed setf.  Actual setting is delayed until DO-DELAYED-SETS is called."
  `(progn (push (cons (locf ,variable) ,value) *delayed-sets*)
	  ,variable))

(defun reset-delayed-sets ()
  (setq *delayed-sets* nil))

(defun do-delayed-sets ()
  (loop for (loc . value) in (nreverse *delayed-sets*)
	do (setf (car loc) value))
  (setq *delayed-sets* nil))

(defun make-keyword (&rest parts)
  (intern (apply #'string-append parts) pkg-keyword-package))

(defmacro dsetmethods (flavor &rest variables)
  `(progn 'compile
	  ,@(loop for variable in variables
		  collect `(defmethod (,flavor ,(make-keyword 'set- variable))
				      (new-value)
			     (dset ,variable new-value)))))

(defvar *delayed-sends* nil)

(defun reset-delayed-sends ()
  (setq *delayed-sends* nil))

(defun do-delayed-sends ()
  (loop for args in (nreverse *delayed-sends*)
	do (apply #'send args)))

(defmacro dsend (&rest args)
  `(push (list ,@args) *delayed-sends*))

