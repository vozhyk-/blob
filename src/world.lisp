(defun decode-world (message)
  (mapcar #'parse-blob (yason:parse message)))

(defun encode-action (action)
  (with-output-to-string (stream)
    (yason:encode action stream)))

(defun score (message)
  (and (hash-table-p message)
       (gethash "score" message)))

(defclass blob ()
  ((position :initarg :position :accessor blob-position)
   (direction :accessor blob-direction)
   (type :initarg :type :accessor blob-type)))

(defun parse-blob (hash)
  (make-instance 'blob
                 :position (gethash "position" hash)
                 :type (gethash "cellType" hash)))

(defun x (position)
  (gethash "x" position))

(defun y (position)
  (gethash "y" position))

(defun compute-direction (position)
  (rad->deg (atan (y position)
                  (x position))))

(defmethod initialize-instance :after ((blob blob) &key position)
  (setf (blob-direction blob)
        (compute-direction position)))
