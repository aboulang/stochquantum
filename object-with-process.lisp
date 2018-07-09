;;; -*- Package: (RULE); Syntax: Zetalisp -*-
;;;----------------------------------------------------------------
;;;Process management (from proces.lisp on >sys>sys2>)
;;;----------------------------------------------------------------
;;; PROCESS-RUN-FUNCTION and associated hair

;;; This is a list of processes which may be recycled by PROCESS-RUN-FUNCTION
;;; It exists to avoid excess consing of stacks and reclaiming of them via
;;; the ordinary garbage collector.
(DEFVAR PROCESS-RUN-FUNCTION-SPARE-PROCESSES NIL)

(eval-when (load eval compile)
  (shadow '(PROCESS-RUN-FUNCTION PROCESS-RUN-RESTARTABLE-FUNCTION)))

;;; Run a function in its own process
(DEFUN PROCESS-RUN-FUNCTION (NAME-OR-KWDS FUNCTION &REST ARGS)
  "Run a function in its own process.  The process is reused if the machine is warm booted
or if the process is reset."
  (PROCESS-RUN-FUNCTION-1 NAME-OR-KWDS FUNCTION ARGS NIL))

(DEFUN PROCESS-RUN-RESTARTABLE-FUNCTION (NAME FUNCTION &REST ARGS)
  "Run a function in its own process.  The process is reset and restarted when the machine
is warm booted, and restarted when the process is reset."
  (PROCESS-RUN-FUNCTION-1 NAME FUNCTION ARGS '(:RESTART-AFTER-BOOT T :RESTART-AFTER-RESET T)))

(DEFUN PROCESS-RUN-FUNCTION-1 (NAME-OR-KEYS FUNCTION ARGS LOCAL-KEYS)
  (LET ((NAME (IF (LISTP NAME-OR-KEYS) NIL NAME-OR-KEYS))
	(PRIORITY 0)
	(QUANTUM 60.)
	RESTART-AFTER-RESET RESTART-AFTER-BOOT PROCESS WARM-BOOT-ACTION)
    (KEYWORD-EXTRACT (IF (LISTP NAME-OR-KEYS) (APPEND LOCAL-KEYS NAME-OR-KEYS) LOCAL-KEYS)
		     KEYWORDS
		     (NAME PRIORITY QUANTUM RESTART-AFTER-RESET RESTART-AFTER-BOOT
			   WARM-BOOT-ACTION)
      NIL NIL)
    (SETQ PROCESS (WITHOUT-INTERRUPTS (OR (POP PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
					  (MAKE-PROCESS NAME
							':flavor 'process-with-object
							':SPECIAL-PDL-SIZE 4000
							':REGULAR-PDL-SIZE 15000))))
    (SETF (si:PROCESS-NAME PROCESS) (SETQ NAME (IF NAME (STRING NAME) "Anonymous")))
    (SETF (si:PROCESS-WARM-BOOT-ACTION PROCESS) (IF (EQ WARM-BOOT-ACTION ':FLUSH)
						 NIL
					       (OR WARM-BOOT-ACTION
						   (AND RESTART-AFTER-BOOT
							'PROCESS-WARM-BOOT-DELAYED-RESTART)
						   'PROCESS-RUN-FUNCTION-WARM-BOOT-RESET)))
    (SETF (si:SG-NAME (PROCESS-INITIAL-STACK-GROUP PROCESS)) NAME)
    (FUNCALL PROCESS ':SET-QUANTUM QUANTUM)
    (FUNCALL PROCESS ':SET-PRIORITY PRIORITY)
    (FUNCALL PROCESS ':RESET-METERS)
    (LEXPR-FUNCALL #'PROCESS-PRESET PROCESS
		   'PROCESS-RUN-FUNCTION-INTERNAL RESTART-AFTER-RESET FUNCTION ARGS)
    (PROCESS-ENABLE PROCESS)
    PROCESS))

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL (RESTART-ON-RESET FUNCTION &REST ARGS)
  (OR RESTART-ON-RESET (PROCESS-PRESET CURRENT-PROCESS
				       'PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))
  (CATCH-ERROR-RESTART ((ERROR si:ABORT) "Exit process ~A" (si:PROCESS-NAME CURRENT-PROCESS))
    (APPLY FUNCTION ARGS))
  ;; When the function returns, disable this process and make it available
  ;; for re-use.
  (PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS ()
  (si:PROCESS-FLUSH-BACKGROUND-STREAM)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ CURRENT-PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
	(PUSH CURRENT-PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES))
    (FUNCALL CURRENT-PROCESS ':KILL)))

(DEFUN PROCESS-RUN-FUNCTION-WARM-BOOT-RESET (PROCESS)
  (PROCESS-WARM-BOOT-RESET PROCESS)
  (OR (MEMQ PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
      (PUSH PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)))

(DEFUN PROCESS-WARM-BOOT-RESET (PROCESS)
  (WITHOUT-INTERRUPTS
    (FUNCALL PROCESS ':PRESET #'(LAMBDA ()
				  (FUNCALL CURRENT-PROCESS ':KILL)
				  (si:PROCESS-WAIT-FOREVER)))
    (FUNCALL PROCESS ':RESET)
    (PROCESS-ENABLE PROCESS)))

(DEFUN PROCESS-WARM-BOOT-RESTART (PROCESS)
  (PROCESS-RESET PROCESS))

;Like PROCESS-WARM-BOOT-RESTART but doesn't allow it to run until after
;initialization is complete.
(DEFUN PROCESS-WARM-BOOT-DELAYED-RESTART (PROCESS)
  (PUSH (CONS PROCESS (si:PROCESS-RUN-REASONS PROCESS)) si:DELAYED-RESTART-PROCESSES)
  (SETF (si:PROCESS-RUN-REASONS PROCESS) NIL)
  (si:PROCESS-CONSIDER-RUNNABILITY PROCESS)
  (PROCESS-RESET PROCESS))			;Won't actually unwind until given run reason


(defmacro make-pinstance (name &rest init-plist)
  `(send (make-instance ,name ,@init-plist) :process))
;;;----------------------------------------------------------------
;;;The Process
;;;----------------------------------------------------------------
(defflavor process-with-object
	(object					;flavor instance
	 thing-to-be-smushed			;queue of messages for instance
	 (message-queue	nil)
	 )
	(si:process)
  (:settable-instance-variables object)
  (:default-handler push-on-message-queue))

(defun push-on-message-queue (process ignore &rest message)
  (without-interrupts
    (push (copylist message) (symeval-in-instance process 'message-queue))
    (symeval-in-instance process 'message-queue)))

;;;Top level function in process
(defun process-message-queue (&aux (process current-process) object)
  (setq object (symeval-in-instance process 'object))
  (loop unless  (send object :stop) do (send-if-handles object :background)
	unless (symeval-in-instance process 'message-queue) do (process-allow-schedule)	;pass
	else do (loop for mesg = (without-interrupts
				    (pop (symeval-in-instance process 'message-queue)))
		      while mesg
		      ;;Trap errors here
		      do (lexpr-send object mesg))))

(defmethod (:kill process-with-object :before) ()
  (setq message-queue nil)
  (setq object nil))

;;;Interrupts  -- halts current computation, and does what is requested
;;;Note well: The caller hangs up until return!
(defsubst interrupt (pobject message &rest args)
  (send pobject :interrupt #'lexpr-send (send pobject :object) message args))



;;; This mixin allows you to give the instance a name. The instance is stored on
;;; both the class-name-property and the general 'instance property, which serves
;;; as a handle for getting at the referent of any name. This is used by some of
;;; the class functions (see class.lisp).
(defflavor named-object-mixin
	((name nil))
	()
  :initable-instance-variables
  :gettable-instance-variables)

(defmethod (:name named-object-mixin :after) ()
  (if name name
    (with-output-to-string (stream)
      (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPEP)))))

#+ignore	   
(defmethod (:init named-object-mixin :after)(&rest ignore)
  (when name (putprop name self (typep self))))

(defmethod (:init named-object-mixin :after) (&rest ignore)
  (when name (putprop name (cons self (get name 'instance)) 'instance)))


(defmethod (:print-self named-object-mixin)(stream &rest ignore)
  (si:printing-random-object (self stream :typep)
    (format stream "~@[~a~]" name)))

(defmethod (:short-name named-object-mixin)()
  (substring (string name) 0 5))

;;;----------------------------------------------------------------
;;;Associated object
;;;----------------------------------------------------------------			      
(defflavor object-with-process
	((process nil))
	(named-object-mixin)
  :settable-instance-variables)

(defmethod (:process-message object-with-process) (message)
  (lexpr-send self (car message) (cdr message)))

(defmethod (:init object-with-process :after) (&rest ignore)
  (setq process (process-run-function
		  `(:name ,(format nil "Process associated with ~A" (send self :name))
		    :priority 0
		    :quantum 60.)
		  #'process-message-queue))
  (set-in-instance  process 'object self))

;;;Background: a method that gets called on every iteration of the to level loop.
;;;stop: a method that halts the background computation

(defmethod (:stop object-with-process) () nil)

;;;User needs to define :background

#+ignore
(defmethod (si:process :signal) (condition &rest args)
   #+ignore (send self ':reset)
  (eval `(send ',self  ':interrupt #'signal ',condition  ,@args)))
