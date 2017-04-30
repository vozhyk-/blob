(ql:quickload '(:websocket-driver-server :clack :babel :yason :alexandria))

(use-package :websocket-driver)

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
        (let ((direction (print (blob-direction closest-blob))))
          (case (blob-type closest-blob)
            ((0 2) (- direction))
            (1 direction)
            (t 45))))))

(defun bot-action (world)
  (alexandria:plist-hash-table
   (list "direction" (print (bot-direction world)))))

(defun reply (message)
  (encode-action (bot-action (decode-world message))))


(defun bot-server (env)
  (let ((ws (make-server env)))
    (on :message ws
        (lambda (message)
          (send ws (reply message))))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun start-server ()
  (clack:clackup #'bot-server :server :wookie :port 60124))

(defvar *server* (start-server))

(defun restart-server ()
  (setf *server* (start-server)))

(defun stop-server ()
  (clack:stop *server*))
