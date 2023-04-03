(defvar art/XY_0sl '((X . (0.0 1.0))
		     (Y . (0.0 1.0)))
  "These are the calibration values")

(define-minor-mode art-mode
  "Automatic inteRactive coordinates for Tikz"
  :lighter " art")

(defun art/gmc (&optional prompt)
  "Returns the xy points of clicked point (if clicked) as a cons block"
  (let* ((event (read-event (or prompt "Click anywhere"))))
    (if (string-equal (car-or event) "down-mouse-1")
	(progn
	  (read-event)  ;; read the mouse up event
	  (setq pos (event-start event))
	  (setq xy (posn-x-y pos))
	  (mapcar 'float (list (car xy) (cdr xy)))))))

(defun art/2dc (&optional prompt)
  "Returns 2D coordinates as a cons block"
  (interactive)
  (let ((x (float (read-number (format "(%s): Enter X coordinate: " prompt) 0)))
	(y (float (read-number (format "(%s): Enter Y coordinate: " prompt) 0))))
    (list x y)))

(defun art/xy2x0sl (x y)
  "Convert list of x and y points to x0 (intercept) and sl (slope)"
  (let ((x1 (elt x 0))
	(x2 (elt x 1))
	(y1 (elt y 0))
	(y2 (elt y 1)))
    (setq x0 (/ (- (* x1 y2) (* x2 y1)) (- x1 x2)))
    (setq sl (/ (- y1 y2) (- x1 x2)))
    (list x0 sl)))

(defun art/calibrate ()
  "Conduct calibration to set the art/XY_0sl variable"
  (interactive)
  (let* ((XY1 (art/2dc "Point 1"))
	 (xy1 (save-excursion (art/gmc "Click on Point 1")))
	 (XY2 (art/2dc "Point 2"))
	 (xy2 (save-excursion (art/gmc "Click on Point 2"))))
    (setq Xs (mapcar '(lambda (x) (elt x 0)) (list XY1 XY2)))
    (setq Ys (mapcar '(lambda (x) (elt x 1)) (list XY1 XY2)))
    (setq xs (mapcar '(lambda (x) (elt x 0)) (list xy1 xy2)))
    (setq ys (mapcar '(lambda (x) (elt x 1)) (list xy1 xy2)))
    (setq X_0sl (art/xy2x0sl Xs xs))
    (setq Y_0sl (art/xy2x0sl Ys ys))
    (setf (alist-get 'X art/XY_0sl) X_0sl)
    (setf (alist-get 'Y art/XY_0sl) Y_0sl)
    (list XY1 XY2 xy1 xy2)
    ))

(defun art/XY2xy (XY)
  "Transform point from pixels to calibrated coordinate system"
  (list
   (/ (- (elt XY 0) (elt (alist-get 'X art/XY_0sl) 0)) (elt (alist-get 'X art/XY_0sl) 1))
   (/ (- (elt XY 1) (elt (alist-get 'Y art/XY_0sl) 0)) (elt (alist-get 'Y art/XY_0sl) 1))))

(defun art-insert-point ()
  "Query point and insert coordinates"
  (interactive)
  (let ((XY (art/gmc "Click on Point")))
    (message "%s" XY)
    (if XY (progn
	     (setq xy (art/XY2xy XY))
	     (insert (format "(%f, %f)" (elt xy 0) (elt xy 1)))
	     xy))))

(defun art-tikz-draw (&optional dopts nopts)
  "Initiate a tikz \draw instance and insert points sequentially as user clicks"
  (interactive "sDraw options: \nsNode options: ")
  (insert (format "\\draw[%s] " dopts))
  (while (art-insert-point)
    (insert (format "%s -- " nopts)))
  (delete-backward-char 4)
  (insert ";"))

(defun art-tikz-node (&optional nopts nval)
  "Initiate a tikz \node instance and insert value given by user"
  (interactive "sNode options: \nsNode value: ")
  (save-excursion
    (insert (format "\\node[%s] at " nopts))
    (art-insert-point)
    (insert (format " \{%s\};" nval))))

(provide 'art)
