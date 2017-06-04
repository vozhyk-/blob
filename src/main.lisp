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

(defun handle-message (ws message expr expr-score)
  (let ((score (decode-score message)))
    (if score
      ((setq expr-score score)
       (close-connection ws))
      (send ws (encode-action (reply-gp expr (decode-world message)))))))

(defun connect (url)
  (let ((ws (make-client url))
        (expr (mgl-gpr:random-gp-expression *gp* (lambda (level) (declare (ignore level)) nil))))
    (send ws (make-join-message))
    (format t "Expr: ~a~%" expr)
    (on :message ws
        (lambda (message)
          (handle-message ws message expr)))
    (start-connection ws)))

(defvar *url* "ws://192.168.0.12:64645/")

(defvar *gp*
  (make-instance 'mgl-gpr:genetic-programming
                 :toplevel-type 'direction
                 :operators *operators*
                 :literals *literals*
                 :population-size 8
                 :copy-chance 0.9
                 :mutation-chance 0.1
                 :evaluator 'evaluate ;; To avoid an error, mass-evaluate should take precedance
                 :mass-evaluator 'mass-evaluate
                 :randomizer 'randomize
                 :selector 'select
                 :fittest-changed-fn 'report-fittest))

(loop repeat (mgl-gpr:population-size *gp*) do
      (mgl-gpr:add-individual *gp*
        (mgl-gpr:random-gp-expression *gp*
          (lambda (level) (declare (ignore level)) nil))))

(loop repeat 16 do
      (advance-gp *gp*))
