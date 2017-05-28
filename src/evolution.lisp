(deftype direction () '(real 0 360))
(deftype distance () 'real)

(defparameter *operators* (list ;; Blob related operators
                                (mgl-gpr:operator (blob-direction blob) direction
                                                  :weight 0.2)
                                (mgl-gpr:operator (blob-distance blob) distance
                                                  :weight 0.2)
                                (mgl-gpr:operator (blob-type blob) typei
                                                  :weight 0.2)
                                ;; Branch
                                (mgl-gpr:operator (if condi
                                                    direction
                                                    direction) direction
                                                  :weight 0.2)
                                ;; Direction-related conditions
                                (mgl-gpr:operator (< direction direction) condi
                                                  :weight 0.1)
                                (mgl-gpr:operator (and condi condi) condi
                                                  :weight 0.1)
                                ;; Distance-related conditions
                                (mgl-gpr:operator (< distance distance) condi
                                                  :weight 0.1)
                                (mgl-gpr:operator (< distance distance) condi
                                                  :weight 0.1)
                                ;; Type-related conditions
                                (mgl-gpr:operator (= typei typei) condi
                                                  :weight 0.1)))
(defparameter *literals* (list (mgl-gpr:literal (blob)
                                 '(closest-blob *world*))
                               (mgl-gpr:literal (direction)
                                 (random 360.0))
                               (mgl-gpr:literal (distance)
                                 (random 100))
                               (mgl-gpr:literal (typei)
                                                :weight 0.1
                                                0)
                               (mgl-gpr:literal (typei)
                                                :weight 0.1
                                                1)
                               (mgl-gpr:literal (typei)
                                                :weight 0.1
                                                2)
                               (mgl-gpr:literal (condi)
                                                t)
                               (mgl-gpr:literal (direction-cond)
                                                nil)))

(defvar *world*)
(defvar *gp*
  (make-instance 'mgl-gpr:genetic-programming
                 :toplevel-type 'direction
                 :operators *operators*
                 :literals *literals*
                 :copy-chance 0.9
                 :mutation-chance 0.1))
