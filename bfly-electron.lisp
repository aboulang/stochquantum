;;; -*- Package: USER; Syntax: Zetalisp; Mode: Butterfly-Lisp; Fonts: CPTFONT -*-
(define *p* (vector 20 2))
(define *q* (vector 0 100))
(define *e* nil)			;charge
(define *m* nil)			;mass
(define total-energy nil)		;total energy of system
(define stop nil)
(define iter-p 0)
(define iter-q 0)
(define iter-u 0)
(define iter-tolerance 5000000)
(define tell-interval 1)


(define-macro (vec-x vec)
  `(vector-ref ,vec 0))

(define-macro (vec-y vec)
  `(vector-ref ,vec 1))

(define-macro (set-vec-x vec val)
  `(vector-set! ,vec 0 ,val))

(define-macro (set-vec-y vec val)
  `(vector-set! ,vec 1 ,val))

 
(define-macro (incf var . amount)
  `(sequence (set! ,var (+ ,var ,(if amount (car amount) 1))) ,var))

(define (p-loop)
  (do ()
      (stop nil)
    (if (and (> iter-tolerance  (- iter-p iter-q))
	     (> iter-tolerance  (- iter-p iter-u)))
	(p-electron-calc))))

;;vec-x does not change
(define (p-electron-calc)
  (incf iter-p)
  (set-vec-y
    *p*
    (+ (vec-y *p*)
       (- (/ (expt (vec-x *p*) 2)
	     (* *m* (expt (vec-y *q*) 3)))
	  (/ (expt *e* 2) (expt (vec-y *q*) 2))))))


(define (q-loop)
  (do ()
      (stop nil)
    (if (and (> iter-tolerance  (- iter-q iter-p))
	     (> iter-tolerance  (- iter-q iter-u)))
	(q-electron-calc))))

(define (q-electron-calc)
  (incf iter-q)
  (set-vec-x
    *q*
    (+ (vec-x *q*)
       (/ (vec-x *p*)
	  (* *m* (expt (vec-y *q*) 2)))))
  (set-vec-y
    *q*
    (+ (vec-y *q*)
       (/ (vec-y *p*) *m*))))

(define (update-loop)
  (do ()
      (stop nil)
    (incf iter-u)
    (if (zero? (modulo iter-u tell-interval))
	(host-eval `(progn
		      (setq user:iter-q ,iter-q)
		      (setq user:*q* ',(cons 'user:vec (vector->list *q*)))
		      (setq user:iter-p ,iter-p)
		      (setq user:*p* ',(cons 'user:vec (vector->list *p*))))))))

;;; (200 0), (0 100), 20.0 1.0 is circular
;;;;   p        q      e    m
(define (set-up-electron)
  (set! *p* (vector 300.0 0.0))
  (set! *q* (vector 0.0 100.0))
  (set! *e* 50.0)				;charge
  (set! *m* .80)
  (set! iter-p 0)
  (set! iter-q 0)
  (set! stop nil)
  (host-eval `(setq user:*e* ,*e*))
  (host-eval `(setq user:*m* ,*m*))
  (host-eval `(progn
		(setq user:iter-q ,iter-q)
		(setq user:*q* ',(cons 'user:vec (vector->list *q*)))))
  (host-eval `(progn
		(setq user:iter-p ,iter-p)
		(setq user:*p* ',(cons 'user:vec (vector->list *p*)))))
  (host-eval '(user:display-loop))
  (future (update-loop))
  (future (q-loop))
  (p-loop))

(define (h)
  (set! stop #!true)
  (host-eval `(setq user:stop t)))


#||
(h)
||#