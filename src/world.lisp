(defun decode-world (message)
  (yason:parse message))

(defun encode-action (action)
  (with-output-to-string (stream)
    (yason:encode action stream)))

(defun score (message)
  (and (hash-table-p message)
       (gethash "score" message)))

(defun blob-position (blob)
  (gethash "position" blob))

(defun x (position)
  (gethash "x" position))

(defun y (position)
  (gethash "y" position))

(defun blob-direction (blob)
  (let ((position (blob-position blob)))
    (rad->deg (atan (y position)
                    (x position)))))

(defun blob-type (blob)
  (gethash "cellType" blob))
