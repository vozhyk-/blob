(ql:quickload '(:websocket-driver-server :clack :babel :yason :alexandria))

(load "world.lisp")
(load "bot.lisp")

(use-package :websocket-driver)

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
