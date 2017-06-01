(ql:quickload '(:websocket-driver-server :clack :babel :yason :alexandria :mgl-gpr))

(use-package :websocket-driver)

(load "world.lisp")
(load "bot.lisp")
(load "evolution.lisp")

(defun reply (decoded-message)
  (encode-action (bot-action decoded-message)))

(defun reply-gp (expr decoded-message)
  (let* ((*sorted-world* (sort-world decoded-message))
         (direction (eval expr)))
    (alexandria:plist-hash-table
      (list "direction" direction))))

(defun handle-message (ws message expr)
  (let ((decoded-message (decode-world message)))
    (if (score decoded-message)
        (format t "Bot score: ~a~%" (score decoded-message)) ; FIXME score is executed twice
        (send ws (encode-action (reply-gp expr decoded-message))))))

(defun bot-server (env)
  (let ((ws (make-server env))
        (expr (mgl-gpr:random-gp-expression *gp* (lambda (level) (declare (ignore level)) nil))))
    (format t "Expr: ~a~%" expr)
    (on :message ws
        (lambda (message)
          (handle-message ws message expr)))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

;; Generate 16 simple examples
(dotimes (i 16)
  (format t "~a~%" (mgl-gpr:random-gp-expression *gp* (lambda (level) nil))))


(defun start-server ()
  (clack:clackup #'bot-server :server :wookie :port 60124))

(defvar *server* (start-server))

(defun restart-server ()
  (setf *server* (start-server)))

(defun stop-server ()
  (clack:stop *server*))
