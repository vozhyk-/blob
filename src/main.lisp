(ql:quickload '(:websocket-driver-server :clack :babel :yason :alexandria))

(use-package :websocket-driver)

(defun decode-world (message)
  (yason:parse message))

(defun encode-action (action)
  (with-output-to-string (stream)
    (yason:encode action stream)))

(defun bot-action (world)
  (declare (ignore world))
  (alexandria:plist-hash-table '("direction" 45)))

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
