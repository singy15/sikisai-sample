
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
(defparameter *binary-z* (make-list *binary-n* :initial-element 9000.0))
(defparameter *tex-cat* nil)
(defparameter *tex-cat-move* nil)
(defparameter *mouse-old-x* 0.0)
(defparameter *mouse-old-y* 0.0)
(defparameter *cat-x* (- +hw+ 100.0))
(defparameter *cat-y* +hh+)
(defparameter *cat-sx* 1.0)
(defparameter *axis-x* 1.0)
(defparameter *axis-y* -1.0)
(defparameter *axis-z* -2.0)
(defparameter *goodies-n* 30)
(defparameter *goodies-shape* (make-list *goodies-n* :initial-element :cube))
(defparameter *goodies-c* (make-list *goodies-n* :initial-element 0.0))
(defparameter *goodies-x* (make-list *goodies-n* :initial-element 0.0))
(defparameter *goodies-y* (make-list *goodies-n* :initial-element 0.0))
(defparameter *goodies-z* (make-list *goodies-n* :initial-element 9999.0))
(defparameter *dxf-model* nil)

(defclass window (sik:window) ())

(defun get-resource-path (rel-path)
  (asdf:system-relative-pathname 'sikisai-sample rel-path))

(defmethod sik:user-initialize ((this window))
  (setf *tex-cat* 
        (make-instance 'sik:texture 
                       :path (get-resource-path "resource/cat.raw")
                       :width 64 
                       :height 64))
  (setf *tex-cat-move* 
        (make-instance 'sik:texture 
                       :path (get-resource-path "resource/cat-move.raw")
                       :width 64 
                       :height 64
                       :intrpl :nearest))
  (setf *dxf-model* (sik:load-dxf (get-resource-path "resource/ship.dxf"))))

(defun clear ()
  (sik:clear :r 0.2 :g 0.2 :b 0.2))

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
          (multiple-value-bind (r g b) (hsv2rgb (mod (floor (- (/ *tm* 2) (* phue (- i n-min)))) 360) 1.0 1.0)
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
  (let ((rt (* (sin (/ *tm* 100.0)) 10.0)))
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

(defun draw-binary ()
  (loop for i from 0 below (length *binary-str*) do 
    (progn
      (when (> (nth i *binary-z*) 20.0)
        (setf (nth i *binary-z*) (- -100.0 (random 200.0)))
        (setf (nth i *binary-x*) (- (random 10.0) 5.0))
        (setf (nth i *binary-str*) (format nil "~30B" (random (ash 1 30)))))
      (incf (nth i *binary-z*) 0.2)

      (sik:local
        (sik:load-identity)
        (sik:translate (nth i *binary-x*) 0.0 (nth i *binary-z*))
        (sik:rotate 180.0 0.0 0.0 1.0)
        (sik:rotate 90.0 0.0 1.0 0.0)
        (sik:rotate 90.0 1.0 0.0 0.0)
        (sik:scale 0.005 0.005 0.002)
        (sik:texts3 (nth i *binary-str*) :r 1.0 :g 1.0 :b 1.0 :a 0.5)))))

(defun draw-goodies ()
  (loop for i from 0 below *goodies-n* do
        (progn
            (when (> (nth i *goodies-z*) 20.0)
                  (setf (nth i *goodies-shape*) (nth (random 5) (list :cube :sphere :teapot :cone :torus)))
                  (setf (nth i *goodies-c*) (random 360.0))
                  (setf (nth i *goodies-z*) (- -100.0 (random 200.0)))
                  (setf (nth i *goodies-x*) (- (random 10.0) 5.0))
                  (setf (nth i *goodies-y*) (- 1.0 (random 2.0))))
            (incf (nth i *goodies-z*) 0.3)
            (sik:local
              (sik:translate (nth i *goodies-x*) (nth i *goodies-y*) (nth i *goodies-z*))
              (sik:rotate (* 10.0 (+ (nth i *goodies-z*) (nth i *goodies-x*))) 0.1 0.2 0.3)
              (multiple-value-bind (r g b) (hsv2rgb (nth i *goodies-c*) 1.0 1.0)
                (cond ((equal :cube (nth i *goodies-shape*)) (sik:cube3 0.2 :r r :g g :b b))
                      ((equal :sphere (nth i *goodies-shape*)) (sik:sphere3 0.2 20 20 :r r :g g :b b))
                      ((equal :teapot (nth i *goodies-shape*)) 
                       (progn (sik:teapot3 0.2 :r r :g g :b b)))
                      ((equal :cone (nth i *goodies-shape*)) (sik:cone3 0.2 0.4 20 20 :r r :g g :b b))
                      ((equal :torus (nth i *goodies-shape*)) (sik:torus3 0.1 0.2 20 20 :r r :g g :b b))
                      (t nil)))))))

(defun draw-bit ()
  (sik:local
      (sik:rotate (* *tm* 2.0) 0.0 0.0 1.0)
      (sik:scale 0.5 0.5 0.5)
      (sik:poly3 '((0.0 0.0 0.0)
                   (-0.2 0.0 1.0)
                   (0.2 0.0 1.0))
                 :r 1.0 :g 0.0 :b 0.0 :both t)
      (sik:poly3 '((0.0 0.0 0.0)
                   (0.0 -0.2 1.0) 
                   (0.0 0.2 1.0))
                 :r 1.0 :g 0.0 :b 0.0 :both t)))

(defun draw-model ()
  (sik:local
    (sik:translate (+ 0.5 (/ (sin (/ *tm* 100.0)) 2.0)) 0.0 2.0)
    (sik:rotate (* 20.0 (sin (/ *tm* 100.0))) 0.0 0.0 1.0)
    (sik:rotate (* 10.0 (sin (/ *tm* 100.0))) 0.0 1.0 0.0)
    (sik:rotate 90 1.0 0.0 0.0)
    (sik:local
      (sik:model3 *dxf-model* :r 0.9 :g 0.8 :b 0.8 :a 1.0)
      (sik:translate 0.0 0.9 0.0)
      (sik:disable :lighting)
      (loop for i from 0 below 5 do
            (sik:local
              (sik:translate (/ (random 1.0) 20.0) (/ (random 1.0) 20.0) (/ (random 1.0) 20.0))
              (sik:sphere3 0.08 20 20 :r 1.0 :g 1.0 :b 0.8 :a 0.5)))
      (sik:enable :lighting)))
  (sik:local
    (sik:translate -0.9 0.0 0.0)
    (sik:translate (+ 0.5 (/ (sin (/ *tm* 100.0)) 2.0)) 0.0 2.0)
    (draw-bit))
  (sik:local
    (sik:translate 0.9 0.0 0.0)
    (sik:translate (+ 0.5 (/ (sin (/ *tm* 100.0)) 2.0)) 0.0 2.0)
    (draw-bit)))

(defmethod sik:user-display ((this window))
  (incf *tm*)
  (clear)

  (sik:set-camera -2.0 3.0 8.0 0.0 0.0 0.0 0.0 1.0 0.0)
  (sik:enable :light0)
  (sik:light :light0 :position (list 0.0 5.0 6.0 1.0))
  (sik:light :light0 :diffuse '(1.0 1.0 1.0 0))
  (sik:light :light0 :quadratic-attenuation 0.004)
  
  (draw-binary)
  (draw-goodies)
  (draw-model)

  (draw-sikisai)
  (draw-sikisai-logo)
  (draw-rect)
  (draw-point)
  (draw-cat)
  (draw-cursor)
  (draw-desc))

(defun main ()
  (sik:display-window (make-instance 'window 
                                     :title "sikisai-sample" 
                                     :width +width+
                                     :height +height+
                                     :keys (list #\w #\s #\a #\d)
                                     :mode '(:double :rgb :depth :multisample)
                                     :fps 60)))

(in-package :cl-user)

