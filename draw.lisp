;;; -*- Package: USER -*-
(defvar $screen-height 690 "The height of the screen (pixels).")
(defvar $screen-width 1084 "The width of the screen (pixels).")
(defvar $last-x 0 "The last x position the graphics cursor moved to.")
(defvar $last-y 0 "The last y position (upper left = 0) the cursor moved to.")
(defvar $graphics-stream #Q terminal-io #+franz t "The graphics stream.")

(defsubst y-screen (y) (fix (- $screen-height y)))
(defsubst x-screen (x) (fix x))

(defun graphics-stream (stream)
  ;; Direct graphics output to stream.
  ;; If stream is NIL, use TERMINAL-IO.
  (setq $graphics-stream (or stream terminal-io)
	$screen-height (send stream ':inside-height)
	$screen-width (send stream ':inside-width)
	$last-x 0
	$last-y (y-screen $screen-height)))

#|
(defvar $graphics-draw-alu tv:alu-ior "The alu for drawing graphics.")
(defvar $graphics-char-alu tv:alu-ior "The alu for drawing characters.")
(defvar $graphics-draw-last-point t "Draw the last pixel in a line?")

(defmacro while-erasing (&rest body)
  ;; do body while device is erasing
  `(let (($graphics-draw-alu tv:alu-andca)
	 ($graphics-char-alu (send $graphics-stream ':char-aluf)))
     (unwind-protect
       (progn
	 (send $graphics-stream ':set-char-aluf tv:alu-andca)
	 ,@body)
       (send $graphics-stream ':set-char-aluf $graphics-char-alu))))

(defun graphics-move (x y)
  (send $graphics-stream ':set-cursorpos (setq $last-x (fix x))
	(setq $last-y (screen-y (fix y)))))

(defun graphics-draw (x y)
  (send $graphics-stream ':draw-line
	$last-x $last-y 
	(setq $last-x (fix x)) (setq $last-y (screen-y (fix y)))
	$graphics-draw-alu
	$graphics-draw-last-point))

(defun graphics-text (string)
  (send $graphics-stream ':set-cursorpos $last-x $last-y)
  (princ string $graphics-stream))

(defun graphics-clear () (send $graphics-stream ':clear-window))

(or (boundp '$graphics-stream)
    (graphics-stream terminal-io))

(defun graphics-clear-area (left bottom right top)
  ;; Clear an area on the screen.
  (send $graphics-stream ':draw-rectangle
	(1+ (- right left)) (- top bottom)
	(screen-x left) (screen-y top) tv:alu-andca))

|#