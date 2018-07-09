(defvar n-symbols 0)
(defvar n-functions 0)
(defvar n-flavors 0)
(defvar n-operations 0)
(defun symbol-count ()
  (setq n-symbols 0
	n-functions 0
	n-flavors 0
	n-operations 0)
  (mapatoms-all #'symbol-counter)
  (format t "~%Symbols: ~a
~%Functions: ~a
~%Flavors: ~a
~%Methods: ~a
~%Methods/flavor: " n-symbols n-functions n-flavors n-operations
(// n-operations (float n-flavors))
))

(defun symbol-counter (symbol)
  (incf n-symbols)
  (if (fboundp symbol) (incf n-functions))
  (if (get symbol 'si:flavor)
      (progn
	(incf n-flavors)
	(setq n-operations
	      (+ operations (length (si:flavor-method-table (get symbol 'si:flavor)))))))
  )