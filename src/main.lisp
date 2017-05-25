(ql:quickload '(:websocket-driver-server :clack :babel :yason :alexandria :mgl-gpr))

(use-package :websocket-driver)
(use-package :mgl-gpr)

(load "world.lisp")
(load "bot.lisp")
(load "evolution.lisp")

(defun reply (decoded-message)
  (encode-action (bot-action decoded-message)))

(defun handle-message (ws message)
  (let ((decoded-message (decode-world message)))
    (if (score decoded-message)
        (format t "Bot score: ~a~%" (score decoded-message)) ; FIXME score is executed twice
        (send ws (reply decoded-message)))))

(defun bot-server (env)
  (let ((ws (make-server env)))
    (on :message ws
        (lambda (message)
          (handle-message ws message)))
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
