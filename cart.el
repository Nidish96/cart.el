;;; cart.el --- CAlibrated inteRactive coordinates for Tikz       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nidish Narayanaa Balaji

;; Author: Nidish Narayanaa Balaji <nidbid@gmail.com>
;; Keywords: tex, mouse
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/Nidish96/cart.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; cart.el provides convenient function definitions meant to be used
;; in tandem with auctex and some pdf viewer within Emacs (like
;; pdf-tools).  The main purpose is to speed up inserting and editing
;; graphical objects using Tikz/Pgf on latex documents/(beamer)
;; presentations.

;; So far there is support for inserting Tikz draw and
;; node objects, and conducting rigid body translations and
;; rotations on these objects.  The way to use this would be to first
;; calibrate the coordinate system by clicking on two points and
;; providing their coordinate values.  This allows the package to
;; establish the coordinate mapping between the pixel coordinates on
;; the frame to the Tikz/Pgf coordinates meant to be inputted.  The
;; relevant calibration variable is customizable.  So after a
;; particular calibration, if the user feels that the same view can be
;; used across sessions, then it may be saved in the customize
;; interface.  See more in the README.org file.

;;; Code:
(require 'latex)

(defcustom cart--XY_0sl '((X . (0.0 1.0))
                          (Y . (0.0 1.0)))
  "These are the calibration values.
   The behavior will be identical across sessions if these are saved."
  :type 'cons
  :group 'cart)

(defcustom cart--visual-feedback nil
  "Set to t to have artist-mode based visual feedback (Experimental).
   Works only after Emacs 29.1."
  :type 'symbol
  :group 'cart)

(defcustom cart--vis-alpha 10
  "Alpha value for artist-mode visualizer"
  :type 'number
  :group 'cart)

(defcustom cart-keymap-prefix "C-x a"
  "The prefix for `cart-mode' key bindings."
  :type 'string
  :group 'cart)

(defcustom cart--nfmt "%.4f"
  "Format specifier for the numbers.  Defaults to \"%f\"."
  :type 'string
  :group 'cart)

(defvar cart--prev-pt nil
  "Previous point inserted in current session.")

(defvar cart--vis-buffer nil
  "Visualization buffer")

(defun cart--key (key)
  "Prefix keymap-prefix to KEY and construct keybinding."
  (kbd (concat cart-keymap-prefix " " key)))

(define-minor-mode cart-mode
  "CAlibrated inteRactive coordinates for Tikz."
  :global nil
  :group 'cart
  :lighter " cart"
  :keymap
  (list (cons (cart--key "C") #'cart-calibrate)
        (cons (cart--key "p") #'cart-insert-point)
        (cons (cart--key "n") #'cart-tikz-node)
        (cons (cart--key "d") #'cart-tikz-draw)
        (cons (cart--key "e") #'cart-tikz-circle)
        (cons (cart--key "c") #'cart-tikz-coordinates)
        (cons (cart--key "t") #'cart-translate-tikz)
        (cons (cart--key "r") #'cart-rotate-tikz)
        (cons (cart--key "s") #'cart-scale-tikz)
        (cons (cart--key "m") #'cart-tikz-move-point)))

(defun cart--xy2x0sl (x y)
  "Convert list of x and y points to x0 (intercept) and sl (slope).

The two parameters, X & Y, are lists of two numbers storing the x and
y values of two points respectively."
  (let* ((x1 (elt x 0))
         (x2 (elt x 1))
         (y1 (elt y 0))
         (y2 (elt y 1))
         (x0 (/ (- (* x1 y2) (* x2 y1)) (- x1 x2)))
         (sl (/ (- y1 y2) (- x1 x2))))
    (list x0 sl)))

(defun cart--XY2xy (XY)
    "Transform point from pixels to calibrated coordinate system.

  Input parameter XY is a list of two values storing the coordinates."
    (list
     (/ (- (elt XY 0) (elt (alist-get 'X cart--XY_0sl) 0)) (elt (alist-get 'X cart--XY_0sl) 1))
     (/ (- (elt XY 1) (elt (alist-get 'Y cart--XY_0sl) 0)) (elt (alist-get 'Y cart--XY_0sl) 1))))

(defun cart--vecop (op vec1 vec2)
  "Conduct elementwise operation on vectors.

Input parameter OP is a symbol denoting the binary operation.
VEC1 and VEC2 are the two vectors (2-lists of coordinates).
It is also possible to pass in a scalar to vec2, in which case
it is interpreted as (list vec2 vec2)."
  (if (numberp vec2)
    (list (funcall op (elt vec1 0) vec2)
          (funcall op (elt vec1 1) vec2))
    (list (funcall op (elt vec1 0) (elt vec2 0))
          (funcall op (elt vec1 1) (elt vec2 1)))))

(defun cart--angle (vec1 vec2)
  "Return the angle between the two vectors in radians.
Vectors given as lists; Angle domain is [0,2pi).

Input parameters VEC1 and VEC2 are two-number-lists storing the x and
y components of the vectors."
  (let ((Cth (apply '+ (cart--vecop '* vec1 vec2)))
        (Sth (apply '- (cart--vecop '* vec1 (reverse vec2)))))
    (atan Sth Cth)))

(defun cart--norm (vec)
  "Return the 2-norm of vector.
Vector given as a list.

Input parameter VEC is a two-number-list storing the x and y components
of the vector."
  (sqrt (apply '+ (mapcar (lambda (x) (expt x 2)) vec))))

(defun cart-visual-toggle (&optional op)
  "Toggle visualization. If OP is non-nil, then visualization is enabled."
  (interactive)
  (setq cart--visual-feedback (not cart--visual-feedback))
  (if cart--visual-feedback
      (message "cart.el: Visual feedback enabled.")
    (message "cart.el: Visual feedback disabled."))
  )

(defun cart--make-vis-buffer (&optional op)
  "Cart-transparent buffer manipulation.
   If OP is non-nil, buffer is cleared."
  (let ((nfrm (make-frame-on-current-monitor '((fullscreen . maximized)
                                               (foreground-color . "blue")
                                               (background-color . "white")
                                               (cursor-color . "white")
                                               (alpha-background . 10)))))
    (set-frame-parameter nfrm 'alpha-background cart--vis-alpha)

    (select-frame nfrm)
    (toggle-frame-fullscreen)
    (find-file "cart.el drawing buffer")
    (artist-mode)
    (setq-local mode-line-format nil)
    (setq cart--vis-buffer (current-buffer))
    ;; (artist-select-op-straight-line)
    ;; (artist-toggle-rubber-banding)
    )  
  (if op (artist-clear-buffer (current-buffer))))

(defun cart--vis-draw (ev)
  "Draw the event EV using artist-mode."
  ;; (artist-select-op-straight-line)
  ;; (artist-mouse-draw-2points ev))
  (artist-down-mouse-1 ev))

(defun cart--car-or (ARG)
  "Return car of ARG if ARG is a cons, ARG otherwise."
  (if (consp ARG) (car ARG) ARG))

(defun cart--gmc (&optional prompt gpers)
  "Prompt to click on frame and return the xy coordinates.
    Two behaviors are possible: (if clicked) single point returned as a
    list with the two coordinates; (if dragged) start and end points of
    dragged region returned as a list of two point-lists (as above).

    The optional parameter PROMPT allows one to specify a user-facing
    prompt.  The prompt defaults to 'Click anywhere' if not provided.
    The optional parameter GPERS controls the behavior of the graphical
    (transparent) buffer. The following values are possible.
    0: No graphical feedback.
    1: Create graphical feedback buffer.
    2: Use the graphical feedback buffer created at a previous step.
    3: Delete the graphical feedback buffer. "
  (unless cart--visual-feedback (setq gpers 0))
  ;; (when cart--visual-feedback (cart--make-vis-buffer t))
  (let ((cbuf (current-buffer)))
    (cond ((eql 0 (or gpers 0)))  ; do nothing
          (t (if cart--vis-buffer
                 (switch-to-buffer-other-frame cart--vis-buffer)
               (cart--make-vis-buffer t))) ; switch to vis buffer if exists
          )
    (let ((basev (read-event (or prompt "Click anywhere"))))
      (when (string-equal (cart--car-or basev) "switch-frame")
          (setq basev (read-event (or prompt "Click anywhere"))))
      (if (string-equal (cart--car-or basev) "down-mouse-1")
          (let* ((event (read-event))  ;; read the mouse up/drag event
                 (pos (event-start event))
                 (pose (event-end event))
                 (xy (posn-x-y pos))
                 (xye (posn-x-y pose)))
            (when (> (or gpers 0) 0)
              (picture-mouse-set-point (list 'down-mouse-1 (event-start event)))
              (artist-key-set-point)

              (picture-mouse-set-point (list 'down-mouse-1 (event-end event)))
              (artist-key-set-point)

              (cond ((eql 2 (or gpers 0))
                     (switch-to-buffer-other-frame cbuf))
                    ((eql 3 (or gpers 0))
                     (read-char "Press any key to continue.")
                     (delete-frame)
                     (setq cart--vis-buffer nil))))
            (if (eq pos pose)
                (mapcar 'float (list (car xy) (cdr xy)))
              (list (mapcar 'float (list (car xy) (cdr xy)))
                    (mapcar 'float (list (car xye) (cdr xye)))))
            )))))

(defun cart--2dc (&optional prompt xd yd xn yn)
  "Prompt to enter coordinates in document CS and return as list.
    The user is prompted with the string
     \"(PROMPT): Enter Q coordinate: \" where Q is (X,Y) and PROMPT is an
    optional parameter.
   XD and YD are the defaults (defaults to 0).
   XN and YN are what X, Y should NOT be."
  (interactive)
  (let ((x (float (read-number (format "(%s): Enter X coordinate: "
                                       (or prompt "")) (or xd 0))))
        (y (float (read-number (format "(%s): Enter Y coordinate: "
                                       (or prompt "")) (or yd 0)))))
    (while (or (eql x xn) (eql y yn))
      (read-char (concat
                  (format "Choose a point such that x!=%d, y!=%d. " xn yn)
                  (format "Given Point: (%d,%d). " x y)
                  "Press any key to continue."))
      (setq x (float (read-number (format "(%s): Enter X coordinate: "
                                          (or prompt "")) (or xd 0))))
      (setq y (float (read-number (format "(%s): Enter Y coordinate: "
                                          (or prompt "")) (or yd 0)))))
    (list x y)))

(defun cart-calibrate ()
  "Conduct interactive calibration to set the `cart--XY_0sl' variable."
  (interactive)
  (read-char "Choose two points for calibration. Press any key to continue.")
  (let* ((cbuf (current-buffer))
         (XY1 (cart--2dc "Point 1" 0 0))
         (xy1 (save-excursion (cart--gmc "Click on Point 1" 2)))
         (XY2 (cart--2dc "Point 2" 1 1 (elt XY1 0) (elt XY1 1)))
         (xy2 (save-excursion (cart--gmc "Click on Point 2" 3)))
         (Xs (mapcar #'(lambda (x) (elt x 0)) (list XY1 XY2)))
         (Ys (mapcar #'(lambda (x) (elt x 1)) (list XY1 XY2)))
         (xs (mapcar #'(lambda (x) (elt x 0)) (list xy1 xy2)))
         (ys (mapcar #'(lambda (x) (elt x 1)) (list xy1 xy2)))
         (X_0sl (cart--xy2x0sl Xs xs))
         (Y_0sl (cart--xy2x0sl Ys ys)))
    (setf (alist-get 'X cart--XY_0sl) X_0sl)
    (setf (alist-get 'Y cart--XY_0sl) Y_0sl)
    (message "Calibration done!")
    (list XY1 XY2 xy1 xy2)))

(defun cart--gmp (&optional prompt gpers)
  "Prompt to click on frame and return the xy coordinates in drawing CS.
    Identical to `cart--gmc' except for the fact that this subsequently transforms
    the point(s) through a call to `cart--XY2xy'.
    Two behaviors are possible: (if clicked) single point returned as a
    list with the two coordinates; (if dragged) start and end points of
    dragged region returned as a list of two point-lists (as above).

    The optional parameter PROMPT allows one to specify a user-facing
    prompt.  The prompt defaults to 'Click anywhere' if not provided.
    Second optional parameter GPERS is pass to cart--gmc."
  (let ((XYs (cart--gmc prompt gpers)))
    (if (listp (elt XYs 0))
        (mapcar 'cart--XY2xy XYs)
      (cart--XY2xy XYs))))

(defun cart--fmt-point (xy)
  "Insert point as \"(<`cart--nfmt'>, <`cart--nfmt'>)\".

The parameter XY is a 2-list storing the coordinates of the point."
  (format (concat "(" cart--nfmt ", " cart--nfmt ")")
          (elt xy 0) (elt xy 1)))

(defun cart--optbr (&optional opts)
  "Insert options bounded by square braces if provided.
Otherwise do nothing.

Optional input parameter OPTS is either a string of options or nil."
  (if (not (string-empty-p opts))
      (format "[%s]" opts)
    opts))

(defun cart--top-search-forward (string &optional bound noerror count)
  "Search forward for STRING, ensuring point is on top level.

Input parameter STRING is the same as given to `search-forward'.
Optional parameters BOUND, NOERROR, and COUNT are also identical to
that in `search-forward'."
  (let ((p0 (search-forward string bound noerror count)))
    (while (save-excursion (cart--tfm-skip (1- p0)))
      (setq p0 (search-forward string bound noerror count)))
    p0))

(defun cart--top-search-backward (string &optional bound noerror count)
  "Search backward for STRING, ensuring point is on top level.

Input parameter STRING is the same as given to `search-forward'.
Optional parameters BOUND, NOERROR, and COUNT are also identical to
that in `search-forward'."
  (let ((p0 (search-backward string bound noerror count)))
    (while (save-excursion (cart--tfm-skip (1- p0)))
      (setq p0 (search-backward string bound noerror count)))
    p0))

(defun cart-insert-point (&optional prompt gpers)
  "Query for and insert clicked coordinates \"(x, y)\" at the current point.

   Optional input parameter PROMPT allows setting the user-facing
   prompt.   Defaults to \"Click on Point\".
   Parameter GPERS is sent to `cart--gmc` through `cart--gmp`. Defaults to 3."
  (interactive)
  (let ((xy (save-window-excursion (cart--gmp prompt (or gpers 3)))))
    (when xy (insert (cart--fmt-point xy)) t)))

(defun cart-tikz-draw (&optional dopts nopts)
  "Initiate a tikz \\draw and insert points sequentially.
Start with prompting the user for draw options and common node options
\(added after each point).  Format for the insertion is:
        \\draw[DOPTS] (x1, y1) NOPTS -- (x2, y2) NOPTS -- (x3, y3) NOPTS -- ...;
Note that the \"node options\" NOPTS is not bounded by square
braces. The user will have to type them in explicitly if needed.
  The user hits RET to finish inserting points. Finally a prompt shows
up checking if the user wants the first point inserted in the end
again (to make the diagram loop itself.

Optional input parameters DOPTS and NOPTS are strings of draw and node
options respectively. The user receives prompts for populating these."
  (interactive "sDraw options: \nsNode options: ")
  (insert (format "\\draw%s " (cart--optbr dopts)))
  (let ((ctflag nil)
        (cbuf (current-buffer)))
    (while (setq xys (save-window-excursion
                       (cart--gmp
                        "Click on a point/Click+Drag to include tangent (RET to stop insertion)"
                        1)))
      (switch-to-buffer cbuf)
      (if ctflag
          (progn
            (if (numberp (elt xys 0))
                (progn
                  (insert (concat " .. " (cart--fmt-point xys) nopts))
                  (setq ctflag nil))
              (insert (concat " and " (cart--fmt-point (elt xys 0))
                              " .. " (cart--fmt-point (elt xys 1))
                              nopts
                              " .. controls "
                              (cart--fmt-point
                               (cart--vecop '- (elt xys 1)
                                            (cart--vecop '- (elt xys 0) (elt xys 1)))) ))))
        (if (numberp (elt xys 0))
            (insert (concat (cart--fmt-point xys) nopts))
          (insert (concat (cart--fmt-point (elt xys 0))
                          nopts
                          " .. controls " (cart--fmt-point (elt xys 1))))
          (setq ctflag t)) )
      (unless ctflag (insert " -- ")))

    (when cart--visual-feedback
      (switch-to-buffer-other-frame cart--vis-buffer)
      (delete-frame)
      (setq cart--vis-buffer nil))

    (if (y-or-n-p "Insert first point in the end (manual closed path)?")
        (progn
          (cart--goto-begend)
          (cart--top-search-forward "(")
          (let ((pt1 (cart--read-cds)))
            (if (not ctflag)
                (progn (move-end-of-line nil) (insert (cart--fmt-point pt1)))
              (cart--top-search-forward "(")
              (setq pt1 (list pt1 (cart--vecop '- pt1 (cart--vecop '- (cart--read-cds) pt1))))
              (move-end-of-line nil)
              (insert (concat " and " (cart--fmt-point (elt pt1 1)) " .. "
                              (cart--fmt-point (elt pt1 0)))))))
      (if ctflag
          (delete-char (- (point) (search-backward " .. controls")))
        (delete-char (- (point) (search-backward " --")))))
    (insert ";")
    (do-auto-fill)))

(defun cart-tikz-node (&optional nopts nval)
  "Initiate a tikz \\node and insert value given by user.
Start with prompting the user for node options and node value.
Similar in functionality to `cart-tikz-draw' except this has exactly
only point.  Format for the insertion is:
        \\node[NOPTS] at (x, y) {NVAL};

Optional input parameters NOPTS and NVAL and the strings containing
the node options and node value respectively."
  (interactive "sNode options: \nsNode value: ")
  (insert (format "\\node%s at " (cart--optbr nopts)))
  (cart-insert-point)
  (insert (format " \{%s\};" nval))
  (do-auto-fill))

(defun cart-tikz-coordinates (&optional dopts)
  "Initiate a tikz \\draw plot [smooth] and insert points.
Start with prompting the user for draw options.  Format for the
  insertion is:
     \\draw[DOPTS] plot [smooth] coordinates {(x1, y1) (x2, y2) (x3, y3) ...};
  The user hits RET to finish inserting points.  Finally a prompt shows
up checking if the user wants the coordinates to loop.

Optional input parameter DOPTS is a string of draw options.  The user
  receives a prompt for populating these."
  (interactive "sDraw options: ")
  (insert (format "\\draw%s plot [smooth] coordinates {" (cart--optbr dopts)))
  (while (cart-insert-point "Click on a point (RET to stop insertion)" 1)
    (insert " "))

  (when cart--visual-feedback
    (switch-to-buffer-other-frame cart--vis-buffer)
    (delete-frame)
    (setq cart--vis-buffer nil))

  (save-excursion
    (when (y-or-n-p "Closed path?")
      (progn
        (cart--goto-begend)
        (search-forward "[smooth]")
        (left-char 1)
        (insert " cycle"))))
  (delete-char -1)
  (insert "};")
  (do-auto-fill))

(defun cart-tikz-circle (&optional dopts nopts)
  "Initiate a tikz \\draw and insert a circle by choosing 2 pts along circumference.
Start with prompting the user for draw options and node options
added after center point.  Format for the insertion is:
        \\draw[DOPTS] (x1, y1) NOPTS circle
                [x radius=<calc_val_x>, y radius=<calc_val_y>];
Note that the \"node options\" NOPTS is not bounded by square
braces. The user will have to type them in explicitly if needed.


Optional input parameters DOPTS and NOPTS are strings of draw and node
options respectively. The user receives prompts for populating these."
  (interactive "sDraw options: \nsNode options: ")
  (insert (format "\\draw%s " (cart--optbr dopts)))
  (let ((xys1 (cart--gmp "Click and drag (2) points along circumference" 3)))
    (when (numberp (elt xys1 0))  ;; only one point chosen
      (setq xys1 (list xys1 (save-window-excursion
                              (cart--gmp "Click the second point on circumference" 3)))))

    (insert
     (concat (cart--fmt-point
              (mapcar (lambda (x) (/ x 2)) (cart--vecop '+ (elt xys1 0) (elt xys1 1))))
             (or nopts "") " "
             (format (concat "circle[radius=" cart--nfmt "];")
                     (/ (cart--norm (cart--vecop '- (elt xys1 0) (elt xys1 1))) 2))))))

(defun cart--last-open-paren (&optional pos)
  "Return the last open paren that the current point lies in.

Optional input parameter POS allows user to specify point (defaults to
\"(point)\").

Code originally from this stackoverflow answer:
https://emacs.stackexchange.com/a/10405"
  (let ((p0 (point-min))
        (p1 (point-max)))
    ;; (LaTeX-narrow-to-environment)
    (let ((ppss (syntax-ppss (or pos (point)))))
      (widen)
      (narrow-to-region p0 p1)
      (when (nth 1 ppss) (char-after (nth 1 ppss))))))

(defun cart--tfm-skip (&optional pos)
  "Return t if current point (or POS) can be skipped for transformation.
Transformation includes translate & rotate as implemented in
`cart--translate' and `cart--rotate' functions for
`cart-translate-tikz' and `cart-rotate-tikz' respectively.
It works by requiring either that the point is at the top (not bound
by any parens), or if bound by \"{...}\", it must belong to a
coordinate set (as in `cart-tikz-smooth'). It also returns t if inside a comment. 

Optional input parameter POS allows user to specify point (defaults to
  \"(point)\")."
  (save-excursion
    (let ((ppss (syntax-ppss (or pos (point))))
          (lopa (cart--last-open-paren (or pos (point)))))
      (if (char-equal (or lopa ?\0) ?\{)
          (if (string-equal (save-excursion (search-backward "{")
                                            (left-word) (word-at-point))
                            "coordinates") nil lopa)
        (or (nth 4 ppss) lopa)))))

(defun cart--goto-begend (&optional enflg)
  "Move pointer to either the beginning or end of current statement.
Statement assumed to start with a \"\\\" and end with a \";\".

Optional input parameter ENFLG controls behavior.
If nil, point is moved to beginning.
If non-nil, point is moved to end."
  (if enflg
      (while (cart--last-open-paren (search-forward ";" nil t)))
    (while (cart--last-open-paren (search-backward "\\" nil t))))
  (point))

(defun cart--read-cds (&optional pos)
  "Return the coordinates from the current point.
Assumes that POS (or `(point)') is at the first character after a \"(\"
and reads starting there until the next \")\".

Optional parameter POS stores a starting point that defaults to `(point)'."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((p0 (point))
          (p1 (1- (search-forward ")"))))
      (mapcar 'string-to-number
              (split-string
               (replace-regexp-in-string
                "\n" "" (buffer-substring p0 p1))
               ",")))))

(defun cart--translate (&optional dxdy)
  "Conduct rigid body translation on current context.
The context is generated through narrow.  It is important for context
to start from the first object's \"\\\" character and end at the
last object's \";\" character.

Optional input parameter DXDY are x (horizontal) and y (vertical)
translation values."
  (goto-char (point-min))
  (let ((p0) (p1) (cds))
    (while (setq p0 (search-forward "(" (point-max) t))
      (if (cart--tfm-skip (1- p0))
          (goto-char (1+ (point)))
        (setq p1 (search-forward ")"))
        (setq cds (cart--read-cds p0))
        (delete-region (1- p0) p1)
        (insert (cart--fmt-point (cart--vecop '+ cds dxdy)))))))

(defun cart-translate-tikz ()
  "Translate objects in current Tikz/Pgf statement/region.
This works by first calling `narrow-to-region', followed by a call
to `cart--translate'.  If a region is not chosen, the current
statement (bound by \"\\\", \";\") is used for the narrow.  If a
region is chosen, the region is used for the narrow.  It is important
for the region to start from the first object's \"\\\" character and
end at the last object's \";\" character.

The user is queried to click & drag from the start point to end point
representing the desired translation. If the user does not drag and
instead, just clicks, a prompt is launched asking the user to click on
trget point."
  (interactive)
  (save-excursion
    (let* ((xys (save-excursion (cart--gmp "Click & drag from start point to end point" 3))))
      (when (numberp (elt xys 0))
        (message "%s" (numberp (elt xys 0)))
        (setq xyn (save-excursion (cart--gmp "You had only clicked on one point. Please click target point now" 3)))
        (setq xys (list xys xyn)))

      (let ((dxdy (cart--vecop '- (elt xys 1) (elt xys 0))))
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (narrow-to-region (cart--goto-begend) (cart--goto-begend t)))

        (cart--translate dxdy)
        (goto-char (point-min))
        (while (not (eobp))
          (move-end-of-line nil)
          (do-auto-fill)
          (forward-line))
        (do-auto-fill)
        (widen)))))

(defun cart--rotate (&optional tht cpt rnds)
  "Conduct rigid body rotation on current context.
The context is generated through narrow.  It is important for context
to start from the first object's \"\\\" character and end at the
last object's \";\" character.

Optional input parameters control the amount/type of rotations.
THT is rotation angle;
CPT is a list storing center point coordinates; and
RNDS is a boolean governing whether node contents should be rotated or not."
  (goto-char (point-min))
  (let ((p0) (p1) (cds))
    (while (setq p0 (search-forward "(" (point-max) t))
      (if (cart--tfm-skip (1- p0))
          (goto-char (1+ (point)))
        (setq p1 (search-forward ")"))
        (setq cds (cart--read-cds p0))
        (delete-region (1- p0) p1)
        ;; Relative coordinates & Rotation
        (let* ((cdsrel (cart--vecop '- cds (or cpt '(0 0))))
               (Cth (cos (or tht 0)))
               (Sth (sin (or tht 0)))
               (Tcds (list (+ (- (* Cth (elt cdsrel 0)) (* Sth (elt cdsrel 1))) (or (elt cpt 0) 0))
                           (+ (+ (* Sth (elt cdsrel 0)) (* Cth (elt cdsrel 1))) (or (elt cpt 1) 0)))))
          (insert (cart--fmt-point Tcds))))))
  ;; Rotate nodes too, if needed
  (when rnds
    (goto-char (point-min))
    (while (search-forward "node" nil t)
      (unless (cart--last-open-paren)
        (if (not (eq (char-after) (string-to-char "[")))
            (insert (format (concat "[rotate=" cart--nfmt "]")
                            (radians-to-degrees tht)))
          (let ((ebr (save-excursion (search-forward "]"))))
            (if (search-forward "rotate" ebr t)
                (progn
                  (right-word)
                  (let ((nwang (+ (number-at-point) (radians-to-degrees tht))))
                    (skip-chars-backward "0-9.-")
                    (delete-region (point) (progn (skip-chars-forward "0-9.-") (point)))
                    (insert (format cart--nfmt nwang)))
                  (goto-char ebr))
              (goto-char (1- ebr))
              (insert (format (concat ", rotate=" cart--nfmt)
                              (radians-to-degrees tht))))))))))

(defun cart-rotate-tikz ()
  "Rotate objects in current Tikz/Pgf statement/region.
This works by first calling `narrow-to-region', followed by a call to
`cart--rotate'.  If a region is not chosen, the current statement
\(bound by \"\\\", \";\") is used for the narrow.  If a region is
chosen, the region is used for the narrow.  It is important for the
region to start from the first object's \"\\\" character and end at
the last object's \";\" character.

The user is prompted to click on the center of rotation, then to click
and drag the rotation target points.  The angle of rotation is
calculated as the angle between the vectors joining the center point
with the end-points of the drag operation.  If the user fails to drag,
another prompt is launched asking the user to click on the target
point.

After the coordinate values are modified, the user is prompted to say
whether the node contents must be rotated too or not.  The \"rotate\"
field of the nodes (which comes in Tikz/Pgf) is used for this.  If no
options are present for a node, \"[rotate=THT]\" is inserted (where
THT is the angle in degrees).  If options are present for a node, and
a rotate field already exists, the existing value is replaced by its
sum with THT.  If options are present for a node, and no rotate field
exists, it is inserted."
  (interactive)
  (save-excursion
    (let* ((xyref (or (save-window-excursion (cart--gmp "Click on the center of rotation (RET to use origin) " 1)) '(0 0)))
           (xys (cart--gmp "Click and drag the rotation target points " 3))
           (rnds (y-or-n-p "Rotate node contents too?")))
      (when (numberp (elt xys 0))
        (setq xys (list xys (cart--gmp
                             "You had only clicked on one point. Please click target point now" 3))))

      (setq xys (mapcar (lambda (xy) (cart--vecop '- xy xyref)) xys))

      (let ((theta (cart--angle (elt xys 0) (elt xys 1))))
        (message "%s" (region-active-p))
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
        (widen)))))

(defun cart--scale (&optional sc cpt snds)
  "Conduct scaling in the current context.
The context is generated through narrow.  It is important for context
to start from the first object's \"\\\" character and end at the
last object's \";\" character.

Optional input parameters control the amount/type of rotations.
SC is scaling factor;
CPT is a list storing center point coordinates; and
SNDS is a boolean governing whether node contents should be scaled or not."
  (goto-char (point-min))
  (let ((p0) (p1) (cds))
    (while (setq p0 (search-forward "(" (point-max) t))
      (if (cart--tfm-skip (1- p0))
          (goto-char (1+ (point)))
        (setq p1 (search-forward ")"))
        (setq cds (cart--read-cds p0))
        (delete-region (1- p0) p1)
        ;; Relative coordinates & Rotation
        (let* ((cdsrel (cart--vecop '- cds (or cpt '(0 0))))
               (Tcds (cart--vecop '+ (cart--vecop '* cdsrel sc) (or cpt '(0 0)))))
          (insert (cart--fmt-point Tcds))))))
  ;; Scale circle objects, if existing
  (goto-char (point-min))
  (while (search-forward "radius" (point-max) t)
    (when (eql (cart--last-open-paren) (string-to-char "["))
      (right-word)
      (let ((nwang (* (number-at-point) sc)))
        (skip-chars-backward "0-9.-")
        (delete-region (point) (progn (skip-chars-forward "0-9.-") (point)))
        (insert (format cart--nfmt nwang)))))
  ;; Scale nodes too, if needed
  (when snds
    (goto-char (point-min))
    (while (search-forward "node" nil t)
      (unless (cart--last-open-paren)
        (if (not (eq (char-after) (string-to-char "[")))
            (insert (format (concat "[scale=" cart--nfmt "]") sc))
          (let ((ebr (save-excursion (search-forward "]"))))
            (if (search-forward "scale" ebr t)
                (progn
                  (right-word)
                  (let ((nwang (* (number-at-point) sc)))
                    (skip-chars-backward "0-9.-")
                    (delete-region (point) (progn (skip-chars-forward "0-9.-") (point)))
                    (insert (format cart--nfmt nwang)))
                  (goto-char ebr))
              (goto-char (1- ebr))
              (insert (format (concat ", scale=" cart--nfmt) sc)))))))))

(defun cart-scale-tikz ()
  "Scale objects in current Tikz/Pgf statement/region.
This works by first calling `narrow-to-region', followed by a call to
`cart--scale'.  If a region is not chosen, the current statement
\(bound by \"\\\", \";\") is used for the narrow.  If a region is
chosen, the region is used for the narrow.  It is important for the
region to start from the first object's \"\\\" character and end at
the last object's \";\" character.

The user is prompted to click on the center of scaling, then to click
and drag the scaling target points.  The scaling factor is calculated
as the ratio of the distances of the target points from the center
point.  If the user fails to drag, another prompt is launched asking
the user to click on the target point.

After the coordinate values are modified, the user is prompted to say
whether the node contents must be scaled too or not.  The \"scale\"
field of the nodes (which comes in Tikz/Pgf) is used for this.  If no
options are present for a node, \"[scale=SC]\" is inserted (where
SC is the scaling factor).  If options are present for a node, and
a scale field already exists, the existing value is replaced by its
product with SC.  If options are present for a node, and no scale field
exists, it is inserted."
  (interactive)
  (save-excursion
    (let* ((xyref (or (save-window-excursion (cart--gmp "Click on the center of scaling (RET to use origin) " 1)) '(0 0)))
           (xys (cart--gmp "Click and drag the scaling target points " 3))
           (snds (y-or-n-p "Scale node contents too?")))
      (when (numberp (elt xys 0))
        (setq xys (list xys (cart--gmp
                             "You had only clicked on one point. Please click target point now" 3))))
      (setq xys (mapcar (lambda (xy) (cart--vecop '- xy xyref)) xys)) ;; Relative Coordinates

      (let ((sc (apply '/ (reverse (mapcar 'cart--norm xys)))))
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (narrow-to-region (cart--goto-begend) (cart--goto-begend t)))

        (cart--scale sc xyref snds)
        (goto-char (point-min))
        (while (not (eobp))
          (move-end-of-line nil)
          (do-auto-fill)
          (forward-line))
        (do-auto-fill)
        (widen)))))

(defun cart-tikz-move-point ()
  "Move a selected point to a selected target location."
  (interactive)
  (save-excursion
    (let ((xys (cart--gmp "Select point and drag to target" 3))
          (pt (point))
          (nrm 100)
          (mval 100))
      (when (numberp (elt xys 0))
        (setq xys (list xys (cart--gmp "Only source point selected. Click the target point" 3))))

      (LaTeX-narrow-to-environment)
      (goto-char (point-min))
      (while (search-forward "(" (point-max) t)
        (unless (cart--tfm-skip (1- (point)))
          (let ((cds (cart--read-cds)))
            (setq nrm (cart--norm (cart--vecop '- (elt xys 0) cds)))
            (setq pt (if (< nrm mval) (point) pt))
            (setq mval (if (< nrm mval) nrm mval))
            (search-forward ")"))))

      (goto-char (1- pt))
      (delete-region (1- pt) (search-forward ")"))
      (insert (cart--fmt-point (elt xys 1)))

      (widen))))

(defun cart-tikz-delete-point ()
  "Delete a selected point. EXPERIMENTAL. USE AT OWN RISK."
  (interactive)
  (save-excursion
    (let ((xy (cart--gmp "Select point to delete" 3))
          (pt (point))
          (nrm 100)
          (mval 100))

      (LaTeX-narrow-to-environment)
      (goto-char (point-min))
      (while (search-forward "(" (point-max) t)
        (unless (cart--tfm-skip (1- (point)))
          (let ((cds (cart--read-cds)))
            (setq nrm (cart--norm (cart--vecop '- xy cds)))
            (setq pt (if (< nrm mval) (point) pt))
            (setq mval (if (< nrm mval) nrm mval))
            (search-forward ")"))))

      (goto-char (1+ pt))
      (delete-region (point) (cart--top-search-forward "("))

      (widen))))

(provide 'cart)
;;; cart.el ends here
