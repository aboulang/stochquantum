;;; -*- Package: USER; Syntax: Zetalisp; Base: 10. -*-
;;; FIZIKS TOP LEVEL

(defvar $tracing nil "Is tracing on?")
(setq *title* "Lagrange Physics Minilab")  ;Title of window.

(defconst fiziks-commands		      
	  ;; Put commands for the command menu here.
	  `(
	    #+ignore
	    ("Configuration" :send-superior :change-configuration
	     :documentation "Change to an alternative configuration of panes.")
	    ("Bind" :eval (print (setq \ (sim-choose)))
	     :documentation "Bind \ to a simulation object.")
	    ("Debug" :send-superior :debug
	     :documentation "Toggle debugging flag.")
	    ("Describe" :eval (do-describe)
	     :documentation "Describe this frame.")
	    ("Edit" :eval (do-edit)
	     :documentation "Edit the senario.")
	    ("Inspect" :eval (do-inspect)
	     :documentation "Inspect this window.")
	    ("Load" :eval (do-load)
	     :documentation "Load a file.")
	    ("Menu" :send-superior :update-menu
	     :documentation "Update this menu.")
	    ("Reset" :send-superior :reset-simulation
	     :documentation "Reset the simulation to its initial conditions.")
	    ("Simulation" :send-superior :select-simulation
	     :documentation "Select a simulation to run.")
	    ("Step" :send-superior :step
	     :documentation "Single step the simulation.")
	    ("Run" :send-superior :Run-simulation
	     :documentation "Stop // Start the Simulation.")
	    ("Trace" :eval (tv:trace-via-menus)
	     :documentation "Trace a function or method.")
	    ("Quit" :send-superior :exit
	     :documentation "Leave this frame.")(" Clear "
	     :send-superior :clear
	     :documentation "Redraw the window.")
	     ))

(defun run-simulation ()
  "Top level loop to run the simulation in a background process."
  (loop do (process-wait "Sleep" #'(lambda () (and $sim (not (send $sim :stop-p)))))
	   (loop do (send $sim :step)
		    (process-allow-schedule)
		    (when (send $sim :stop-p) (return nil)))))

(defflavor fiziks-frame
	((sim-process nil))	; A process to run the simulation in.
	(ex-frame)
  (:documentation "Top level frame for FIZIKS simulation.")
  (:default-init-plist
   :panes `((interaction tv:basic-interaction-pane
			 :label "Interaction Pane")
	    (title tv:bbn-title-pane
		   :title ,*title*
		   :reverse-video-p t)
	    (command-menu tv:highlight-while-executing-command-pane
			  :item-list ,fiziks-commands
			  :label nil
			  :font-map (fonts:medfnb))
	    (graphics tv:basic-pane
		      :blinker-p nil
		      :label (:string "Graphics Pane")))
   :configurations '((UPPER-RIGHT-MENU
		   (:LAYOUT (UPPER-RIGHT-MENU :COLUMN TITLE ELSE)
		    (ELSE :ROW GRAPHICS REST)
		    (REST :COLUMN COMMAND-MENU INTERACTION))
		   (:SIZES (REST (COMMAND-MENU :ASK :PANE-SIZE) :THEN (INTERACTION :EVEN))
		    (ELSE (GRAPHICS 0.5) :THEN (REST :EVEN))
		    (UPPER-RIGHT-MENU (TITLE 1 :LINES) :THEN (ELSE :EVEN)))))
   ))

(defmethod (fiziks-frame :after :before-loop) ()
  (send self :make-sim-process))

(defmethod (fiziks-frame :make-sim-process) ()
  (when sim-process
    (send sim-process :kill))
  (setq sim-process
	(PROCESS-RUN-FUNCTION
	  '(:NAME "Fiziks Simulation" :PRIORITY -30
	    :restart-after-reset t
	    :restart-after-boot t)
	  #'RUN-SIMULATION)))

(defmethod (fiziks-frame :after :select)(&rest ignore)
  (graphics-stream (send self ':get-pane 'graphics)); Graphics stream is graphics pane.
  )

(defmethod (fiziks-frame :run-simulation) (&aux stopped?)
  "Toggle simulation run state."
  (send self :highlight-menu-item 'command-menu "Run" (setq stopped?
							     (not (send $sim :stop-p))))
  (send $sim :stop stopped?))

(defmethod (fiziks-frame :highlight-menu-item) (menu-name item-name highlight?)
  (send self :send-pane menu-name
	(if highlight? :add-highlighted-item-name :remove-highlighted-item-name)
	item-name))

;;; PUT COMMAND ACTIONS HERE.
(defmethod (fiziks-frame :run-simulation) (&aux run?)
  "Toggle simulation run state."
  (setq run? (cond ((not $sim) nil)
		   ((send $sim :stop-p) nil)
		   (t t)))
  (setq run? (and $sim (send $sim :stop-p)))
  (send self :set-run-state run?)
  ;; This kludge is needed because you are trying to highlight the item you are
  ;; executing.
  (send self :highlight-menu-item 'command-menu "Run" (not run?))
  )

(defmethod (fiziks-frame :run-state) ()
  (and $sim (not (send $sim :stop-p))))

(defmethod (fiziks-frame :set-run-state) (run?)
  (send self :highlight-menu-item 'command-menu "Run" run?)
  (when $sim (send $sim :stop (not run?))))

(defmethod (fiziks-frame :reset-simulation) ()
  (send self :set-run-state nil)
  (when $sim
    (send $sim ':reset)
    (send $sim :stop)))

(defmethod (fiziks-frame :select-simulation) (&aux choice)
  (send self :set-run-state nil)
  (when (setq choice (tv:menu-choose
		       (loop for simulation in *simulations*
			     collect (send simulation :choose-item))
		       "Choose a Simulation"))
    (setq $sim choice)
    (send self :reset-simulation)))

(defmethod (fiziks-frame :step) ()
  (send self :set-run-state nil)
    (send $sim :step))

(defmethod (fiziks-frame :update-menu) ()
  (send self :send-pane 'command-menu :set-item-list fiziks-commands))

(defun do-describe ()
    (let ((choice (sim-choose)))
      (cond (choice
	     (send terminal-io ':clear-window)
	     (describe choice)))))

(defun sim-choose ()
  ;; Choose an object.
  (if (not $sim) (format t "[Please load a simulation first.]")
      (tv:menu-choose
	(loop for object in (cons *frame*
				  (cons $sim (send $sim ':objects)))
	      collect `(,(format nil " ~s " object) :value ,object)))))

(defun do-edit ()
  (send *frame* :reset-simulation)
  (let ((choice (sim-choose)))
    (cond (choice
	   (send choice :send-if-handles :erase)
	   (send choice :pop-edit)
	   (send choice :send-if-handles :reinitialize)
	   (send *frame* :reset-simulation)))))

(defun do-inspect ()
  (let ((choice (sim-choose)))
    (and choice
	 (inspect choice))))

(defun do-load (&aux name)
  (setq name
	(prompt-and-read `(:pathname :visible-default "fiziks:fiziks;fiz.lisp")
			 "Load file:~%"))
  (and name (load name)))

(defun do-run ()
    (send $sim ':run))

(defun stub (command)
  ;; Do nothing version of COMMAND
  (format t "~A is maximally grody!" command))

(eval-when (load eval)
  ;; Create an instance of this window and make it key selectable.
  (tv:add-frame-to-system 'fiziks-frame #/z *title*))


