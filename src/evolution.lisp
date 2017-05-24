(defparameter *operators* (list (operator (blob-direction 'blob) 'direction)
                                (operator (blob-distance 'blob) 'distance)
                                (operator (blob-type 'blob) 'type)))
(defparameter *literals* (list (literal ('blob)
                                 (closest-blob '*world*)))

(random-expression :type direction)
