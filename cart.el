(defcustom cart--XY_0sl '((X . (0.0 1.0))
                        (Y . (0.0 1.0)))
  "These are the calibration values"
  :group 'cart)

(defcustom cart-keymap-prefix "C-x a"
  "The prefix for cart-mode key bindings"
  :type 'string
  :group 'cart)

(defun cart--key (key)
  (kbd (concat cart-keymap-prefix " " key)))

(define-minor-mode cart-mode
  "Automatic inteRactive coordinates for Tikz"
  :global nil
  :group 'cart
  :lighter " cart"
  :keymap
  (list (cons (cart--key "c") #'cart-calibrate)
        (cons (cart--key "p") #'cart-insert-point)
        (cons (cart--key "d") #'cart-tikz-draw)
        (cons (cart--key "n") #'cart-tikz-node)
        (cons (cart--key "t") #'cart-translate-tikz)
        (cons (cart--key "r") #'cart-rotate-tikz))

  (if cart-mode
      (message "cart-mode activated!")
    (message "cart-mode de-activated!"))

  (add-hook 'cart-mode-hook (lambda () (message "cart mode hook was execd")))
  (add-hook 'cart-mode-on-hook (lambda () (message "cart mode hook was execd on")))
  (add-hook 'cart-mode-off-hook (lambda () (message "cart mode hook was execd off"))))

(defun cart--gmc (&optional prompt)
  "Returns the xy points of clicked point (if clicked) as a list, or,
start and end points of dragged region (if dragged) as a list of lists."
  (if (string-equal (car-or (read-event (or prompt "Click anywhere"))) "down-mouse-1")
      (let* ((event (read-event))  ;; read the mouse up/drag event
             (pos (event-start event))
             (pose (event-end event))
             (xy (posn-x-y pos))
             (xye (posn-x-y pose)))
        (if (eq pos pose)
            (mapcar 'float (list (car xy) (cdr xy)))
          (list (mapcar 'float (list (car xy) (cdr xy)))
                (mapcar 'float (list (car xye) (cdr xye))))))))

(defun cart--2dc (&optional prompt)
  "Returns 2D coordinates as a cons block"
  (interactive)
  (let ((x (float (read-number (format "(%s): Enter X coordinate: " prompt) 0)))
        (y (float (read-number (format "(%s): Enter Y coordinate: " prompt) 0))))
    (list x y)))

(defun cart--xy2x0sl (x y)
  "Convert list of x and y points to x0 (intercept) and sl (slope)"
  (let* ((x1 (elt x 0))
         (x2 (elt x 1))
         (y1 (elt y 0))
         (y2 (elt y 1))
         (x0 (/ (- (* x1 y2) (* x2 y1)) (- x1 x2)))
         (sl (/ (- y1 y2) (- x1 x2))))
    (list x0 sl)))

(defun cart-calibrate ()
  "Conduct calibration to set the cart--XY_0sl variable"
  (interactive)
  (let* ((XY1 (cart--2dc "Point 1"))
         (xy1 (save-excursion (cart--gmc "Click on Point 1")))
         (XY2 (cart--2dc "Point 2"))
         (xy2 (save-excursion (cart--gmc "Click on Point 2")))
         (Xs (mapcar #'(lambda (x) (elt x 0)) (list XY1 XY2)))
         (Ys (mapcar #'(lambda (x) (elt x 1)) (list XY1 XY2)))
         (xs (mapcar #'(lambda (x) (elt x 0)) (list xy1 xy2)))
         (ys (mapcar #'(lambda (x) (elt x 1)) (list xy1 xy2)))
         (X_0sl (cart--xy2x0sl Xs xs))
         (Y_0sl (cart--xy2x0sl Ys ys)))
    (setf (alist-get 'X cart--XY_0sl) X_0sl)
    (setf (alist-get 'Y cart--XY_0sl) Y_0sl)
    (list XY1 XY2 xy1 xy2)))

(defun cart--XY2xy (XY)
  "Transform point from pixels to calibrated coordinate system"
  (list
   (/ (- (elt XY 0) (elt (alist-get 'X cart--XY_0sl) 0)) (elt (alist-get 'X cart--XY_0sl) 1))
   (/ (- (elt XY 1) (elt (alist-get 'Y cart--XY_0sl) 0)) (elt (alist-get 'Y cart--XY_0sl) 1))))

(defun cart-insert-point ()
  "Query point and insert coordinates"
  (interactive)
  (let ((XY (cart--gmc "Click on Point")))
    (if XY (let ((xy (cart--XY2xy XY)))
             (insert (format "(%f, %f)" (elt xy 0) (elt xy 1)))
             xy))))

(defun cart--optbr (&optional opts)
  (if (not (string-empty-p opts))
      (format "[%s]" opts)
    opts))

(defun cart-tikz-draw (&optional dopts nopts)
  "Initiate a tikz \draw instance and insert points sequentially as user clicks"
  (interactive "sDraw options: \nsNode options: ")
  (insert (format "\\draw%s " (cart--optbr dopts)))
  (while (cart-insert-point)
    (insert (format "%s -- " nopts)))
  (if (y-or-n-p "Insert first point at the end?")
      (progn
        (cart--goto-begend)
        (search-forward "(")
        (while (cart--last-open-paren (1- (point)))
          (search-forward "("))
        (let ((pt1 (buffer-substring (point) (save-excursion (search-forward ")")))))
          (move-end-of-line nil)
          (insert (format "%s" pt1))))
    (delete-backward-char 4))
  (insert ";")
  (do-auto-fill))

(defun cart-tikz-node (&optional nopts nval)
  "Initiate a tikz \node instance and insert value given by user"
  (interactive "sNode options: \nsNode value: ")
  (insert (format "\\node%s at " (cart--optbr nopts)))
  (cart-insert-point)
  (insert (format " \{%s\};" nval))
  (do-auto-fill))

(defun cart--last-open-paren (&optional pos)
  "Returns the last open paren that the current point lies in.
  Optional argument POS allows user to specify point (other that current).

  Code from this stackoverflow answer: https://emacs.stackexchange.com/a/10405"
  (let ((ppss (syntax-ppss (or pos (point)))))
    (when (nth 1 ppss) (char-after (nth 1 ppss)))))

(defun cart--goto-begend (&optional enflg)
  (if enflg
      (while (cart--last-open-paren (search-forward ";" nil t)))
    (while (cart--last-open-paren (search-backward "\\" nil t))))
  (point))

(defun cart--angle (vec1 vec2)
  "Returns the angle between the two vectors (given as lists)."
  (let ((Cth (+ (* (elt vec1 0) (elt vec2 0)) (* (elt vec1 1) (elt vec2 1))))
        (Sth (- (* (elt vec1 0) (elt vec2 1)) (* (elt vec2 0) (elt vec1 1)))))
    (atan Sth Cth)))

(defun cart--translate (&optional dx dy) 
  "Conduct rigid body translation on current object.
  DX, DY are x (horizontal) and y (vertical translation."
  (goto-char (point-min))
  (let ((p0) (p1) (cds))
    (while (setq p0 (search-forward "(" (point-max) t))
      (if (cart--last-open-paren (1- p0))
          (goto-char (1+ (point)))
        (setq p1 (1- (search-forward ")")))
        (setq cds
              (mapcar 'string-to-number
                      (split-string
                       (replace-regexp-in-string
                        "\n" "" (buffer-substring p0 p1))
                       ",")))
        (delete-region p0 p1)
        (goto-char p0)
        (setf (elt cds 0) (+ (elt cds 0) (or dx 0)))
        (setf (elt cds 1) (+ (elt cds 1) (or dy 0)))
        (insert (mapconcat 'number-to-string cds ","))))))

(defun cart--rotate (&optional tht cpt rnds) 
  "Conduct rigid body rotation on current object.
THT is rotation angle.
CPT is a list storing center point.
RNDS is a boolean governing whether node contents should be rotated or not."
  (goto-char (point-min))
  (let ((p0) (p1) (cds))
    (while (setq p0 (search-forward "(" (point-max) t))
      (if (cart--last-open-paren (1- p0))
          (goto-char (1+ (point)))
        (setq p1 (1- (search-forward ")")))
        (setq cds
              (mapcar 'string-to-number
                      (split-string
                       (replace-regexp-in-string
                        "\n" "" (buffer-substring p0 p1))
                       ",")))
        (delete-region p0 p1)
        (goto-char p0)
        ;; Relative coordinates & Rotation
        (let* ((cdsrel (list (- (elt cds 0) (or (elt cpt 0) 0))
                             (- (elt cds 1) (or (elt cpt 1) 0))))
               (Cth (cos (or tht 0)))
               (Sth (sin (or tht 0)))
               (Tcds (list (+ (- (* Cth (elt cdsrel 0)) (* Sth (elt cdsrel 1))) (or (elt cpt 0) 0))
                           (+ (+ (* Sth (elt cdsrel 0)) (* Cth (elt cdsrel 1))) (or (elt cpt 1) 0)))))
          (insert (mapconcat 'number-to-string Tcds ","))))))
  ;; Rotate nodes too, if needed
  (when rnds
    (goto-char (point-min))
    (while (search-forward "node" nil t)
      (if (not (eq (char-after) (string-to-char "[")))
          (insert (format "[rotate=%f]" (radians-to-degrees tht)))
        (let ((ebr (save-excursion (search-forward "]"))))
          (if (search-forward "rotate" ebr t)
              (progn
                (right-word)
                (let ((nwang (+ (number-at-point) (radians-to-degrees tht))))
                  (skip-chars-backward "0-9.-")
                  (delete-region (point) (progn (skip-chars-forward "0-9.-") (point)))
                  (insert (format "%f" nwang)))
                (goto-char ebr))
            (goto-char (1- ebr))
            (insert (format ", rotate=%f" (radians-to-degrees tht)))))))))

(defun cart-translate-tikz ()
  "Translate objects in current sentence or under region using two points."
  (interactive)
  (let* ((XYs (cart--gmc "Click & drag from start point to end point"))
         (XY0 (elt XYs 0))
         (XY1 (elt XYs 1)))
    (unless (listp XY0)
      (setq XY0 XYs)
      (setq XY1 (cart--gmc "You had only clicked on one point. Please click target point now")))

    (let* ((xy0 (cart--XY2xy XY0))
           (xy1 (cart--XY2xy XY1))
           (dx (- (elt xy1 0) (elt xy0 0)))
           (dy (- (elt xy1 1) (elt xy0 1))))

      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (narrow-to-region (cart--goto-begend) (cart--goto-begend t)))

      (cart--translate dx dy)
      (goto-char (point-min))
      (while (not (eobp))
        (move-end-of-line nil)
        (do-auto-fill)
        (forward-line))
      (do-auto-fill)
      (widen))))

(defun cart-rotate-tikz ()
  "Rotate objects in current sentence or under region using a reference point and two target points."
  (interactive)
  (let* ((XYref (cart--gmc "Click on the center of rotation (RET to use origin) "))
         (XYs (cart--gmc "Click and drag the rotation target points "))
         (rnds (y-or-n-p "Rotate Node contents too?"))
         (XY0 (elt XYs 0))
         (XY1 (elt XYs 1))
         (xyref (if XYref (cart--XY2xy XYref) (list 0 0))))
    (unless (listp XY0)
      (setq XY0 XYs)
      (setq XY1 (cart--gmc "You had only clicked on one point. Please click target point now")))

    (let* ((xy0 (cart--XY2xy XY0))
           (xy1 (cart--XY2xy XY1))
           ;; Relative Coordinates
           (xy0 (list (- (elt xy0 0) (elt xyref 0)) (- (elt xy0 1) (elt xyref 1))))
           (xy1 (list (- (elt xy1 0) (elt xyref 0)) (- (elt xy1 1) (elt xyref 1))))
           (theta (cart--angle xy0 xy1)))

      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (narrow-to-region (cart--goto-begend) (cart--goto-begend t)))

      (cart--rotate theta xyref rnds)
      (goto-char (point-min))
      (while (not (eobp))
        (move-end-of-line nil)
        (do-auto-fill)
        (forward-line))
      (do-auto-fill)
      (widen))))

(provide 'cart)
