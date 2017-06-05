(ql:quickload '(:websocket-driver-client :babel :yason :alexandria :mgl-gpr :bordeaux-threads))

(use-package :websocket-driver)

(load "world.lisp")
(load "bot.lisp")
(load "evolution.lisp")

(defun reply (decoded-message)
  (encode-action (bot-action decoded-message)))

(defun call-and-validate (fun)
  (handler-case
      (let ((result (funcall fun)))
        (assert (numberp result)))
    (error (error)
      (format t "~&Generated expression has an error: ~a~%" error)
      0)))

(defun reply-gp (generated-fun decoded-message)
  (let* ((*sorted-world* (sort-world decoded-message))
         (direction (call-and-validate generated-fun)))
    (alexandria:plist-hash-table
     (list "direction" direction))))

(defun make-join-message ()
  "!")

(defun handle-message (ws message expr)
  (let* ((parsed-msg (yason:parse message))
         (score (decode-score parsed-msg)))
    (if score
        score
        (and (send ws (encode-action (reply-gp expr (decode-world parsed-msg))))
             nil))))

(defun connect (url)
  (let ((ws (make-client url))
        (expr (mgl-gpr:random-gp-expression *gp* (lambda (level) (declare (ignore level)) nil))))
    (send ws (make-join-message))
    (format t "Expr: ~a~%" expr)
    (on :message ws
        (lambda (message)
          (handle-message ws message expr)))
    (start-connection ws)))

(defvar *gp*)

(defun reset-gp ()
  (setf *gp*
        (make-instance 'mgl-gpr:genetic-programming
                       :toplevel-type 'direction
                       :operators *operators*
                       :literals *literals*
                       :population-size 16
                       :copy-chance 0.2
                       ;; It means that there is 1 - 0.2 - 0.4 = 0.4
                       ;; chance that it will crossover
                       :mutation-chance 0.4
                       ;; To avoid an error, mass-evaluate
                       ;; should take precedance
                       :evaluator 'evaluate
                       :mass-evaluator 'mass-evaluate
                       :randomizer 'randomize
                       :selector 'select
                       :fittest-changed-fn 'report-fittest))
  (loop repeat (mgl-gpr:population-size *gp*) do
       (mgl-gpr:add-individual
        *gp*
        (mgl-gpr:random-gp-expression
         *gp*
         (lambda (level) (declare (ignore level)) nil)))))

(reset-gp)

(defvar *url*)

(defun run (url)
  (setf *url* url)
  (loop repeat 16384 do
       (advance-gp *gp*)))
