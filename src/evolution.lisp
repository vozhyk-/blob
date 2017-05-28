(deftype direction () '(real 0 360))
(deftype distance () 'real)

(defparameter *operators* (list (mgl-gpr:operator (blob-direction blob)
                                                  direction)
                                (mgl-gpr:operator (blob-distance blob) distance)
                                (mgl-gpr:operator (blob-type blob) type)))
(defparameter *literals* (list (mgl-gpr:literal (blob)
                                 (closest-blob *world*))))

(defvar *world*
  (list
   (make-instance 'blob
                  :type 0
                  :position (alexandria:plist-hash-table
                             '("x" 0 "y" 1)
                             :test #'equal))))

(mgl-gpr:random-expression *operators* *literals* 'direction
                           (lambda (level) (declare (ignore level)) nil))
