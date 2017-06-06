(defun encode-action (action)
  (with-output-to-string (stream)
    (format stream ";")
    (yason:encode action stream)))

(defun decode-score (message)
  (if (hash-table-p message)
      (gethash "score" message)
      nil))

(defclass blob ()
  ((position :initarg :position :accessor blob-position)
   (direction :accessor blob-direction)
   (type :initarg :type :accessor blob-type)
   (size :initarg :size :accessor blob-size)))

(defun parse-blob (hash)
  (make-instance 'blob
                 :position (gethash "position" hash)
                 :type (gethash "cellType" hash)
                 :size (gethash "size" hash)))

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
                 :size 0
                 :position (alexandria:plist-hash-table
                            '("x" 0 "y" 0)
                            :test #'equal)))

(defconstant +empty-world+
  (make-instance 'world :others nil :own-size 30))

(defun is-nil (blob)
  (eq +nil-blob+ blob))

(defclass world ()
  ((others :initarg :others :accessor others)
   (own-size :initarg :own-size :accessor own-size)))

(defun parse-world (message)
  (make-instance 'world
                 :others (mapcar #'parse-blob
                                 (gethash "others" message))
                 :own-size (gethash "ownSize" message)))

(defun sort-world (world)
  (make-instance 'world
                 :others (sort (others world) #'< :key #'blob-distance)
                 :own-size (own-size world)))

(defun get-blob (sorted-world position)
  (with-accessors ((others others)) sorted-world
    (if (< position (length others))
        (nth position others)
        +nil-blob+)))
