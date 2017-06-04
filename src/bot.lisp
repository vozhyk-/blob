(defun closest-blob (world)
  (let ((sorted (sort-world world)))
    (if sorted
        (first sorted)
        +nil-blob+)))

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
