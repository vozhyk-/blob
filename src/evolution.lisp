(deftype direction () '(real 0 360))
(deftype distance () 'real)

(defparameter *operators* (list (mgl-gpr:operator (blob-direction blob)
                                                  direction
                                                  :weight 0.6)
                                (mgl-gpr:operator (blob-distance blob) distance
                                                  :weight 0.6)
                                (mgl-gpr:operator (+ direction direction) direction
                                                  :weight 0.05)
                                (mgl-gpr:operator (+ distance distance) distance
                                                  :weight 0.05)
                                (mgl-gpr:operator (if condi
                                                    direction
                                                    direction) direction
                                                  :weight 0.2)
                                (mgl-gpr:operator (< distance distance) condi
                                                  :weight 0.4)
                                (mgl-gpr:operator (= type type) condi
                                                  :weight 0.4)
                                (mgl-gpr:operator (blob-type blob) type
                                                  :weight 0.05)))
(defparameter *literals* (list (mgl-gpr:literal (blob)
                                 '(closest-blob *world*))
                               (mgl-gpr:literal (direction)
                                 (random 360.0)
                                 :weight 0.05)
                               (mgl-gpr:literal (distance)
                                 (random 100)
                                 :weight 0.05)))

(defvar *world*)
(defvar *gp*
  (make-instance 'mgl-gpr:genetic-programming
                 :toplevel-type 'direction
                 :operators *operators*
                 :literals *literals*
                 :population-size 64
                 :copy-chance 0.05
                 :mutation-chance 0.1))
