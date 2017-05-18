(defun decode-world (message)
  (yason:parse message))

(defun encode-action (action)
  (with-output-to-string (stream)
    (yason:encode action stream)))

(defun blob-position (blob)
  (gethash "position" blob))

(defun x (position)
  (gethash "x" position))

(defun y (position)
  (gethash "y" position))

(defun rad->deg (radians)
  (* (/ radians pi) 180))

(defun blob-direction (blob)
  (let ((position (blob-position blob)))
    (rad->deg (atan (y position)
                    (x position)))))

(defun blob-distance (blob)
  (let* ((position (blob-position blob)))
    (with-accessors ((x x) (y y)) position
      (sqrt (* x x y y)))))

(defun closest-blob (world)
  (let ((sorted (sort world #'< :key #'blob-distance)))
    (and sorted (first sorted))))

(defun blob-type (blob)
  (gethash "cellType" blob))

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
