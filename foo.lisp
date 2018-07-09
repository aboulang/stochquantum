;;; -*- Fonts: MEDFNT; Syntax: Zetalisp -*-


(defflavor tester
	((a (make-array 3))
	 (b 6)
	 (c '(2 3 4 5))
	 (dset-stack nil)
	 )
	()
  :settable-instance-variables)

(defmethod (tester :update) ()
  (loop for item on dset-stack
	do (setf (car item) (cadr item))))

(defvar dset-stack nil)

(defmacro dset (exp val)
  (list 'ppush exp val))

(defun ppush (vbl val)
  (setq dset-stack (cons (list vbl val) dset-stack)))