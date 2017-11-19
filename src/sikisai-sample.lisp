
(defpackage sikisai-sample
  (:use cl cl-user)
  (:export main))
(in-package :sikisai-sample)

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

(defclass window (sik:window) ())

(defmethod sik:user-initialize ((this window))
  (setf *tex-cat* (make-instance 'sik:texture 
                                 :path "./resource/cat.raw" 
                                 :width 64 
                                 :height 64))
  (setf *tex-cat-move* (make-instance 'sik:texture 
                                      :path "./resource/cat-move.raw" 
                                      :width 64 
                                      :height 64
																			:intrpl :nearest)))

(defun clear ()
  (sik:clear :r 0.3 :g 0.3 :b 0.3))

(defun draw-sikisai ()
  (sik:texts "SIKISAI" (- +hw+ 40.0) (+ +hh+ 100.0) :r 1.0 :g 1.0 :b 1.0 :sx 1.0 :sy 5.0 :aa t :a 1.0))

(defun hsv2rgb (h s v)
  (let* ((c (* v s))
         (hp (/ h 60))
         (x (* c (- 1 (abs (- (mod hp 2) 1)))))
         (m (- v c)))
    (multiple-value-bind 
      (r g b) 
      (cond ((and (<= 0 hp) (< hp 1)) (values c x 0))
          ((and (<= 1 hp) (< hp 2)) (values x c 0))
          ((and (<= 2 hp) (< hp 3)) (values 0 c x))
          ((and (<= 3 hp) (< hp 4)) (values 0 x c))
          ((and (<= 4 hp) (< hp 5)) (values x 0 c))
          ((and (<= 5 hp) (< hp 6)) (values c 0 x)))
      (values (+ r m) (+ g m) (+ b m)))))

(defun draw-sikisai-logo ()
  (let* ((l 50.0)
         (fix-y -40.0)
         (n 40)
         (n-min (+ (* 1 (/ n 4)) 5))
         (n-max (- (* 3 (/ n 4)) 5))
         (n-part (- n-max n-min))
         (phue (floor (/ 360 n-part)))
         (ptheta (/ (* 2.0 PI) (float n)))) 
    (sik:line (- +hw+ l) (+ (- +hh+ l) fix-y) (+ +hw+ l) (+ +hh+ fix-y) :w 2.0 :aa t :a 1.0)
    (sik:line (- +hw+ l) (+ (+ +hh+ l) fix-y) (+ +hw+ l) (+ +hh+ fix-y) :w 2.0 :aa t :a 1.0)
    (loop for i from n-min below n-max do 
          (multiple-value-bind (r g b) (hsv2rgb (* phue (- i n-min)) 1.0 1.0)
            (sik:poly `((,(+ +hw+ l) ,(+ +hh+ fix-y))
                        (,(+ +hw+ (* l (cos (* i ptheta)))) ,(+ +hh+ (* l (sin (* i ptheta))) fix-y))
                        (,(+ +hw+ (* l (cos (* (+ i 1) ptheta)))) ,(+ +hh+ (* l (sin (* (+ i 1) ptheta))) fix-y)))
                      :a (* 0.5 (/ (float (- i n-min)) (float n-part))) :aa t)
            (sik:line (+ +hw+ (* l (cos (* i ptheta))))
                      (+ +hh+ (* l (sin (* i ptheta))) fix-y)
                      (+ +hw+ (* l (cos (* (+ i 1) ptheta))))
                      (+ +hh+ (* l (sin (* (+ i 1) ptheta))) fix-y)
                      :r r :g g :b b :w 2.0 :a 1.0 :aa t)))))

(defun draw-rect ()
  (when (equal (mod *tm* 20) 0)
    (sik:rect (random (sik:get-width)) (random (sik:get-height)) (random (sik:get-width)) (random (sik:get-height)) 
              :r 1.0 :g 1.0 :b 1.0 :a 0.2 :aa t)))

(defun draw-point ()
	(when (equal (random 10) 0)
    (loop for i from 0 below 20 do
        (sik:point (random (sik:get-width)) (random (sik:get-height)) :s 2.0 :r 1.0 :g 1.0 :b 1.0 :a 1.0 :aa t))))

(defun draw-binary ()
  (loop for i from 0 below (length *binary-str*) do 
    (progn
      (when (> (nth i *binary-y*) (sik:get-height))
        (setf (nth i *binary-y*) (- (+ 500 (random 2000))))
        (setf (nth i *binary-x*) (random (sik:get-width)))
        (setf (nth i *binary-str*) (format nil "~30B" (random (ash 1 30)))))
      (incf (nth i *binary-y*) 2.0)
      (sik:texts (nth i *binary-str*) (nth i *binary-x*) (nth i *binary-y*) :a 0.2 :aa t :rt 90))))

(defun draw-cat ()
  (when (sik:get-key-down #\w)
    (decf *cat-y*))
  (when (sik:get-key-down #\s)
    (incf *cat-y*))
  (when (sik:get-key-down #\a)
    (decf *cat-x*)
    (setf *cat-sx* -1.0))
  (when (sik:get-key-down #\d)
    (incf *cat-x*)
    (setf *cat-sx* 1.0))
  (when (or (sik:get-key-push #\w)
            (sik:get-key-push #\s)
            (sik:get-key-push #\a)
            (sik:get-key-push #\d))
    (sik:circle *cat-x* *cat-y* 90.0 20 :r 1.0 :g 0.0 :b 0.0 :a 1.0 :w 2.0))
  (let ((rt (/ (sin (/ *tm* 100.0)) 3.0)))
    (if (or (sik:get-key-down #\w)
          (sik:get-key-down #\s)
          (sik:get-key-down #\a)
          (sik:get-key-down #\d))
      (progn
        (sik:image *tex-cat-move* (+ *cat-x* 10.0) (+ *cat-y* 10.0) :a 0.3 :r 1.0 :g 0.0 :b 0.0 :sx *cat-sx* :rt rt)
        (sik:image *tex-cat-move* (+ *cat-x* 20.0) (+ *cat-y* 20.0) :a 0.3 :r 0.0 :g 1.0 :b 0.0 :sx *cat-sx* :rt rt)
        (sik:image *tex-cat-move* (+ *cat-x* 30.0) (+ *cat-y* 30.0) :a 0.3 :r 0.0 :g 0.0 :b 1.0 :sx *cat-sx* :rt rt)
        (sik:image *tex-cat-move* *cat-x* *cat-y* :a 1.0 :sx *cat-sx* :rt rt))
      (sik:image *tex-cat* *cat-x* *cat-y* :a 1.0 :sx *cat-sx* :rt rt) )))

(defun draw-cursor ()
  (sik:line *mouse-old-x* *mouse-old-y* (sik:get-mouse-x) (sik:get-mouse-y) :a 0.8 :aa t)
  (setf *mouse-old-x* (sik:get-mouse-x))
  (setf *mouse-old-y* (sik:get-mouse-y))
  (when (sik:get-mouse-down :left)
    (sik:circle (sik:get-mouse-x) (sik:get-mouse-y) 20.0 20 :r 1.0 :g 1.0 :b 1.0 :a 1.0 :w 2.0))
  (when (sik:get-mouse-push :left)
    (sik:circle (sik:get-mouse-x) (sik:get-mouse-y) 40.0 20 :r 1.0 :g 0.0 :b 0.0 :a 1.0 :w 2.0)))

(defun draw-desc ()
  (let ((x (- +hw+ 130.0))
        (y (+ +hh+ 120.0))
        (interval 12.0))
    (sik:textb "This is demo application of Sikisai." x y :a 1.0)
    (incf y interval)
    (sik:textb "Press WASD to move cat." x y :a 0.5 :font glut:+bitmap-helvetica-12+) 
    (incf y interval)
    (sik:textb "Left click to show mouse position." x y :a 0.5 :font glut:+bitmap-helvetica-12+) 
    (incf y interval)
    (sik:textb "Press Esc to close." x y :r 1.0 :g 0.5 :b 0.5 :a 1.0)))

(defmethod sik:user-display ((this window))
  (incf *tm*)
  (clear)
  (draw-sikisai)
  (draw-sikisai-logo)
  (draw-rect)
  (draw-point)
  (draw-binary)
  (draw-cat)
  (draw-cursor)
  (draw-desc))

(defun main ()
  (sik:display-window (make-instance 'window 
                                     :title "sikisai-sample" 
                                     :width +width+
                                     :height +height+
                                     :keys (list #\w #\s #\a #\d)
                                     :fps 60)))

(in-package :cl-user)

