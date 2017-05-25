(deftype direction () '(real 0 360))
(deftype direction () real)

(defparameter *operators* (list (operator (blob-direction blob) direction)
                                (operator (blob-distance blob) distance)
                                (operator (blob-type blob) type)))
(defparameter *literals* (list (literal (blob)
                                 (closest-blob *world*))))

(let ((*world* (list
                (make-instance 'blob
                               :type 0
                               :position (alexandria:plist-hash-table
                                          '("x" 0 "y" 1))))))
  (random-expression *operators* *literals* direction (lambda (&rest args) (error))))
