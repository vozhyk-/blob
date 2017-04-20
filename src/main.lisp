(ql:quickload '(:websocket-driver-server :clack))

(use-package :websocket-driver)

(defvar *echo-server*
  (lambda (env)
    (let ((ws (make-server env)))
      (on :message ws
          (lambda (message)
            (send ws (print message))))
      (lambda (responder)
        (declare (ignore responder))
        (start-connection ws)))))

;; Start Wookie server
(clack:clackup *echo-server* :server :wookie :port 5000)
