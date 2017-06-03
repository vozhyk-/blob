(ql:quickload '(:websocket-driver-client :babel :yason :alexandria :mgl-gpr))

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

(defun make-join-message ()
  "!")

(defun handle-message (ws message expr)
  (let ((decoded-message (decode-world message)))
    (if (score decoded-message)
        (format t "Bot score: ~a~%" (score decoded-message)) ; FIXME score is executed twice
        (send ws (encode-action (reply-gp expr decoded-message))))))

(defun connect (url)
  (let ((ws (make-client url))
        (expr (mgl-gpr:random-gp-expression *gp* (lambda (level) (declare (ignore level)) nil))))
    (send ws (make-join-message))
    (format t "Expr: ~a~%" expr)
    (on :message ws
        (lambda (message)
          (handle-message ws message expr)))
    (start-connection ws)))

;; Generate 16 simple examples
(dotimes (i 16)
  (format t "~a~%" (mgl-gpr:random-gp-expression *gp* (lambda (level) nil))))
