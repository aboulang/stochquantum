;;; MACROS AND DEBUGGING AIDS.
#+franz(load 'comment)

(eval-when (eval compile load)
;;	   (defvar $pi (*$ 4.0 (atan 1.0 1.0)))
	   (defvar $rad//deg (//$ si:pi 180.0)))

(defmacro rad (degrees) 
  `(//$ ,degrees #.$rad//deg))

(defmacro deg (radians)
  `(*$ ,radians #.$rad//deg))

(defmacro append-member (element set)
  ;; Add a new member to the end of a list.
    `(or (member ,element ,set)
	 (setf ,set (append ,set (list ,element)))))

(defmacro push-member (element set)
  ;; Add a new member to the beginning of a list.
    `(or (member ,element ,set)
	 (setf ,set (cons ,element ,set))))

;;; DEBUGGING
#||
(defvar *debug* t "Print debugging printout when t.")
(defvar $debug-stream t)

(defmacro debugging (&body body)
    ;; Simple Debugging packet.  BODY run only when *debug* is t.
    `(and *debug*
	  (progn ,@body)))

(defmacro qprint (&rest names)
#|
Quick printout for debugging:

-> (dprint (sqrt 2) (sqrt 3))
(sqrt 2) = 1.41421 (sqrt 3) = 1.73205 

()
->
|#
    `(progn
	      ,@(loop for name in names collect
		      `(format $debug-stream "~s = ~s " ',name ,name))
	      (terpri)))

(defmacro dprint (&rest args)
    `(debugging (qprint ,@args)))
||#