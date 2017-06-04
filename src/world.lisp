(defun decode-world (message)
  (mapcar #'parse-blob (yason:parse message)))

(defun encode-action (action)
  (with-output-to-string (stream)
    (format stream ";")
    (yason:encode action stream)))

(defun decode-score (message)
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

(defun rad->deg (radians)
  (* (/ radians pi) 180))

(defun compute-direction (position)
  (rad->deg (atan (y position)
                  (x position))))

(defmethod initialize-instance :after ((blob blob) &key position)
  (setf (blob-direction blob)
        (compute-direction position)))

(defun blob-distance (blob)
  (let* ((position (blob-position blob)))
    (with-accessors ((x x) (y y)) position
      (sqrt (* x x y y)))))

(defconstant +nil-blob+
  (make-instance 'blob
                 :type 0
                 :position (alexandria:plist-hash-table
                            '("x" 0 "y" 0)
                            :test #'equal)))

(defun is-nil (blob)
  (eq +nil-blob+ blob))

(defun sort-world (world)
  (sort world #'< :key #'blob-distance))

(defun get-blob (sorted-world position)
  (if (< position (length sorted-world))
    (nth position sorted-world)
    +nil-blob+))
