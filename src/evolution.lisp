(defparameter *operators* (list ;; Blob related operators
                                (mgl-gpr:operator (blob-direction blob) direction)
                                (mgl-gpr:operator (blob-distance blob) distance)
                                (mgl-gpr:operator (blob-type blob) type)
                                ;; Branch
                                (mgl-gpr:operator (if cond
                                                    direction
                                                    direction) direction
                                                  :weight 0.5)
                                (mgl-gpr:operator (and cond cond) cond
                                                  :weight 0.01)
                                (mgl-gpr:operator (or cond cond) cond
                                                  :weight 0.01)
                                ;; Direction-related condtions
                                (mgl-gpr:operator (< direction direction) cond
                                                  :weight 0.1)
                                ;; Distance-related condtions
                                (mgl-gpr:operator (< distance distance) cond
                                                  :weight 0.2)
                                (mgl-gpr:operator (< const-distance distance) cond
                                                  :weight 0.1)
                                (mgl-gpr:operator (< distance const-distance) cond
                                                  :weight 0.1)
                                ;; Type-related condtions
                                (mgl-gpr:operator (= type type) cond
                                                  :weight 0.1)
                                (mgl-gpr:operator (= const-type type) cond
                                                  :weight 0.3)))
(defparameter *literals* (list (mgl-gpr:literal (blob)
                                 '(closest-blob *world*))
                               (mgl-gpr:literal (const-direction)
                                 (random 360.0))
                               (mgl-gpr:literal (const-distance)
                                 (random 100))
                               (mgl-gpr:literal (cosnt-type) 0)
                               (mgl-gpr:literal (cosnt-type) 1)
                               (mgl-gpr:literal (const-type) 2)))

(defvar *world*)
(defvar *gp*
  (make-instance 'mgl-gpr:genetic-programming
                 :toplevel-type 'direction
                 :operators *operators*
                 :literals *literals*
                 :copy-chance 0.9
                 :mutation-chance 0.1))
