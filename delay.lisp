;;; -*- Syntax: Zetalisp; Base: 10. -*-
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

