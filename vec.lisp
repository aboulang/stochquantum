;;; -*-  Base: 10.; Syntax: Zetalisp; Package: USER -*-
;;; 2-D VECTORS
;;; A vec is a simple 2-D vector represented as a list
;;; of the form (vec X Y).

#+franz(environment-lmlisp)
#+franz(declare (macros t))

;;; Making, identifying, and accessing forms for VEC's.
(defmacro make-vec (x y)
    `(list 'vec ,x ,y))

(defmacro is-vec (thing)
    `(and (listp ,thing)
	  (eq (car ,thing) 'vec)))

(defmacro vec-x (vector)
    `(second ,vector))

(defmacro vec-y (vector)
    `(third ,vector))

;; A destructuring macro
(defmacro with-cracked-vec ((vec x y) &body body)
    `(let ((,x (vec-x ,vec))
	   (,y (vec-y ,vec)))
	 ,@body))

(defun vec-length (vector)
    ;; Length of a vec.
    (sqrt (+ (expt (vec-x vector) 2)
	    (expt (vec-y vector) 2))))

(defun vec-normalize (vector)
    ;; Normalize VECTOR so its length is 1.0.
    (v//s vector (vec-length vector)))

(defun v+v (vector-1 vector-2)
    ;; VECTOR-1 + VECTOR-2
    (make-vec (+ (vec-x vector-1)
		(vec-x vector-2))
	      (+ (vec-y vector-1)
		(vec-y vector-2))))

(defun v-v (vector-1 vector-2)
    ;; VECTOR-1 - VECTOR-2
    (make-vec (- (vec-x vector-1)
		(vec-x vector-2))
	      (- (vec-y vector-1)
		(vec-y vector-2))))

(defun v*v (vector-1 vector-2)
    ;; Dot product
    (+ (* (vec-x vector-1) (vec-x vector-2))
      (* (vec-y vector-1) (vec-y vector-2))))

(defun v*s (vector scalor)
    ;; VECTOR * SCALOR
    (make-vec (* (vec-x vector) scalor)
	      (* (vec-y vector) scalor)))

(defun v//s (vector scalor)
    ;; VECTOR / SCALOR
    (v*s vector (// 1.0 scalor)))

(defun vec-reflect (vector direction)
  ;; Reflect VECTOR by a plane perpendicular to the unit normal DIRECTION.
  (v-v vector
       (v*s (v*s direction (v*v direction vector))
	    2.0)))

(defun vec-perpendicular (vector &aux length)
  ;; Returns a unit VEC perpendicular to VECTOR.
  (cond ((zerop (setq length (vec-length vector)))
	 (make-vec 1.0 0.0))			; Choose at random.
	(t
	 (make-vec (- (// (vec-y vector) length))
		   (// (vec-x vector) length)))))


(defprop :vec (print-vec read-vec nil nil nil
					"Click to enter a new vector.")
	 tv:choose-variable-values-keyword)

(defun print-vec (position &optional (stream t))
  (format stream "~f ~f" (vec-x position) (vec-y position)))

(defun read-vec (&optional stream)
  (let ((x (read stream))			; Be sure order of reads is right!
	(y (read stream)))
    (make-vec x y)))

