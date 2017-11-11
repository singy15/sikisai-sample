
(defpackage mir-sample
  (:use cl cl-user)
  (:export main))
(in-package :mir-sample)

(defvar +width+ 400)
(defvar +height+ 400)
(defvar +hw+ (/ +width+ 2.0))
(defvar +hh+ (/ +height+ 2.0))
(defparameter *tm* 0)
(defparameter *binary-n* 20)
(defparameter *binary-str* (make-list *binary-n*))
(defparameter *binary-x* (make-list *binary-n* :initial-element 0.0))
(defparameter *binary-y* (make-list *binary-n* :initial-element 10000.0))
(defparameter *tex-cat* nil)
(defparameter *tex-cat-move* nil)
(defparameter *mouse-old-x* 0.0)
(defparameter *mouse-old-y* 0.0)
(defparameter *cat-x* (- +hw+ 100.0))
(defparameter *cat-y* +hh+)
(defparameter *cat-sx* 1.0)

(defclass window (mir:window) ())

(defmethod mir:user-initialize ((this window))
  (setf *tex-cat* (make-instance 'mir:texture 
                                 :path "./resource/cat.raw" 
                                 :width 64 
                                 :height 64))
  (setf *tex-cat-move* (make-instance 'mir:texture 
                                      :path "./resource/cat-move.raw" 
                                      :width 64 
                                      :height 64)))

(defun clear ()
  (mir:clear :r 0.3 :g 0.3 :b 0.3))

(defun draw-mir ()
  (mir:texts "M I R" (- +hw+ 100.0) (+ +hh+ 80.0) :r 1.0 :g 1.0 :b 1.0 :sx 4.0 :sy 1.5 :aa t :a 1.0))

(defun draw-mir-logo ()
  (let* ((l 50.0)
         (fix-y -40.0)
         (n 40)
         (n-min (+ (* 1 (/ n 4)) 5))
         (n-max (- (* 3 (/ n 4)) 5))
         (n-part (- n-max n-min))
         (ptheta (/ (* 2.0 PI) (float n)))) 
    (mir:line (- +hw+ l) (+ (- +hh+ l) fix-y) (+ +hw+ l) (+ +hh+ fix-y) :w 2.0 :aa t :a 1.0)
    (mir:line (- +hw+ l) (+ (+ +hh+ l) fix-y) (+ +hw+ l) (+ +hh+ fix-y) :w 2.0 :aa t :a 1.0)
    (loop for i from n-min below n-max do 
          (progn
            (mir:poly `((,(+ +hw+ l) ,(+ +hh+ fix-y))
                        (,(+ +hw+ (* l (cos (* i ptheta)))) ,(+ +hh+ (* l (sin (* i ptheta))) fix-y))
                        (,(+ +hw+ (* l (cos (* (+ i 1) ptheta)))) ,(+ +hh+ (* l (sin (* (+ i 1) ptheta))) fix-y)))
                      :r 1.0 :g 1.0 :b 1.0 :a (* 0.5 (/ (float (- i n-min)) (float n-part))) :aa t)
            (mir:line (+ +hw+ (* l (cos (* i ptheta))))
                      (+ +hh+ (* l (sin (* i ptheta))) fix-y)
                      (+ +hw+ (* l (cos (* (+ i 1) ptheta))))
                      (+ +hh+ (* l (sin (* (+ i 1) ptheta))) fix-y)
                      :w 2.0 :a 1.0 :aa t)))))

(defun draw-rect ()
  (when (equal (mod *tm* 20) 0)
    (mir:rect (random (mir:get-width)) (random (mir:get-height)) (random (mir:get-width)) (random (mir:get-height)) 
              :r 1.0 :g 1.0 :b 1.0 :a 0.2 :aa t)))

(defun draw-point ()
	(when (equal (random 10) 0)
    (loop for i from 0 below 20 do
        (mir:point (random (mir:get-width)) (random (mir:get-height)) :s 2.0 :r 1.0 :g 1.0 :b 1.0 :a 1.0 :aa t))))

(defun draw-binary ()
  (loop for i from 0 below (length *binary-str*) do 
    (progn
      (when (> (nth i *binary-y*) (mir:get-height))
        (setf (nth i *binary-y*) (- (+ 500 (random 2000))))
        (setf (nth i *binary-x*) (random (mir:get-width)))
        (setf (nth i *binary-str*) (format nil "~30B" (random (ash 1 30)))))
      (incf (nth i *binary-y*) 2.0)
      (mir:texts (nth i *binary-str*) (nth i *binary-x*) (nth i *binary-y*) :a 0.2 :aa t :rt 90))))

(defun draw-cat ()
  (when (mir:get-key-down #\w)
    (decf *cat-y*))
  (when (mir:get-key-down #\s)
    (incf *cat-y*))
  (when (mir:get-key-down #\a)
    (decf *cat-x*)
    (setf *cat-sx* -1.0))
  (when (mir:get-key-down #\d)
    (incf *cat-x*)
    (setf *cat-sx* 1.0))
  (when (or (mir:get-key-push #\w)
            (mir:get-key-push #\s)
            (mir:get-key-push #\a)
            (mir:get-key-push #\d))
    (mir:circle *cat-x* *cat-y* 90.0 20 :r 1.0 :g 0.0 :b 0.0 :a 1.0 :w 2.0))
  (if (or (mir:get-key-down #\w)
          (mir:get-key-down #\s)
          (mir:get-key-down #\a)
          (mir:get-key-down #\d))
    (mir:image *tex-cat-move* *cat-x* *cat-y* :a 1.0 :sx *cat-sx*)
    (mir:image *tex-cat* *cat-x* *cat-y* :a 1.0 :sx *cat-sx*) ))

(defun draw-cursor ()
  (mir:line *mouse-old-x* *mouse-old-y* (mir:get-mouse-x) (mir:get-mouse-y) :a 0.8 :aa t)
  (setf *mouse-old-x* (mir:get-mouse-x))
  (setf *mouse-old-y* (mir:get-mouse-y))
  (when (mir:get-mouse-down :left)
    (mir:circle (mir:get-mouse-x) (mir:get-mouse-y) 20.0 20 :r 1.0 :g 1.0 :b 1.0 :a 1.0 :w 2.0))
  (when (mir:get-mouse-push :left)
    (mir:circle (mir:get-mouse-x) (mir:get-mouse-y) 40.0 20 :r 1.0 :g 0.0 :b 0.0 :a 1.0 :w 2.0)))

(defun draw-desc ()
  (let ((x (- +hw+ 130.0))
        (y (+ +hh+ 120.0))
        (interval 12.0))
    (mir:textb "This is demo application of Mir." x y :a 1.0)
    (incf y interval)
    (mir:textb "Press WASD to move cat." x y :a 0.5 :font glut:+bitmap-helvetica-12+) 
    (incf y interval)
    (mir:textb "Left click to show mouse position." x y :a 0.5 :font glut:+bitmap-helvetica-12+) 
    (incf y interval)
    (mir:textb "Press Esc to close." x y :r 1.0 :g 0.5 :b 0.5 :a 1.0)))

(defmethod mir:user-display ((this window))
  (incf *tm*)
  (clear)
  (draw-mir)
  (draw-mir-logo)
  (draw-rect)
  (draw-point)
  (draw-binary)
  (draw-cat)
  (draw-cursor)
  (draw-desc))

(defun main ()
  (mir:display-window (make-instance 'window 
                                     :title "mir-sample" 
                                     :width +width+
                                     :height +height+
                                     :keys (list #\w #\s #\a #\d)
                                     :fps 60)))

(in-package :cl-user)

