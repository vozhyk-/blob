
(defun rad->deg (radians)
  (* (/ radians pi) 180))

(defun blob-distance (blob)
  (let* ((position (blob-position blob)))
    (with-accessors ((x x) (y y)) position
      (sqrt (* x x y y)))))

(defun closest-blob (world)
  (let ((sorted (sort world #'< :key #'blob-distance)))
    (and sorted (first sorted))))

(defun bot-direction (world)
  (let ((closest-blob (closest-blob world)))
    (if (null closest-blob)
        45
        (let ((direction (blob-direction closest-blob)))
          (case (blob-type closest-blob)
            ((0 2)
             ;(format t "Escaping a player or a mine: ~a~%" (- direction))
             (- direction))
            (1
             ;(format t "Chasing food: ~a~%" direction)
             direction)
            (t 45))))))

(defun bot-action (world)
  (alexandria:plist-hash-table
   (list "direction" (bot-direction world))))
