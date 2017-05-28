(deftype direction () '(real 0 360))
(deftype distance () 'real)

(defparameter *operators* (list (mgl-gpr:operator (blob-direction blob)
                                                  direction)
                                (mgl-gpr:operator (blob-distance blob) distance)
                                (mgl-gpr:operator (blob-type blob) type)))
(defparameter *literals* (list (mgl-gpr:literal (blob)
                                 '(closest-blob *world*))))

(defvar *world*)
(defvar *gp*
  (make-instance 'mgl-gpr:genetic-programming
                 :toplevel-type 'direction
                 :operators *operators*
                 :literals *literals*
                 :population-size 64
                 :copy-chance 0.05
                 :mutation-chance 0.1))
