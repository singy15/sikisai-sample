
(in-package :cl-user)
(defpackage sikisai-sample-asd
  (:use :cl :asdf))
(in-package :sikisai-sample-asd)

(defsystem sikisai-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "src"
			:around-compile
				(lambda (thunk)
          ; dev
          (declaim (optimize (speed 0) (debug 3) (safety 3)))
          ; release
          ; (declaim (optimize (speed 3) (debug 0) (safety 0)))
					(funcall thunk))
      :components (
        (:file "sikisai")
        (:file "sikisai-sample")))))

