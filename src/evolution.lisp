(defparameter *operators* (list ;; Blob related operators
                           (mgl-gpr:operator (blob-direction blob) direction)
                           (mgl-gpr:operator (blob-distance blob) distance)
                           (mgl-gpr:operator (blob-type blob) type)
                           (mgl-gpr:operator (blob-size blob) size)
                           ;; sorted-world related operations
                           (mgl-gpr:operator (get-blob sorted-world position) blob)
                           (mgl-gpr:operator (is-nil blob) cond)
                           ;; Branch
                           (mgl-gpr:operator (if cond
                                                 blob
                                                 blob) blob
                                                 :weight 0.2)
                           (mgl-gpr:operator (if cond
                                                 direction
                                                 direction) direction
                                                 :weight 0.2)
                           (mgl-gpr:operator (and cond cond) cond
                                             :weight 0.01)
                           (mgl-gpr:operator (or cond cond) cond
                                             :weight 0.01)
                           ;; Direction-related condtions
                           (mgl-gpr:operator (< direction direction) cond
                                             :weight 0.1)
                           ;; Distance-related condtions
                           (mgl-gpr:operator (< distance distance) cond
                                             :weight 0.2)
                           (mgl-gpr:operator (< const-distance distance) cond
                                             :weight 0.1)
                           (mgl-gpr:operator (< distance const-distance) cond
                                             :weight 0.1)
                           ;; Type-related condtions
                           (mgl-gpr:operator (= type type) cond
                                             :weight 0.1)
                           (mgl-gpr:operator (= const-type type) cond
                                             :weight 0.3)
                           ;; Size-related condtions
                           (mgl-gpr:operator (< size size) cond
                                             :weight 0.2)
                           (mgl-gpr:operator (< const-size size) cond
                                             :weight 0.1)
                           (mgl-gpr:operator (< size const-size) cond
                                             :weight 0.1)))
(defparameter *literals* (list (mgl-gpr:literal (sorted-world)
                                 '*sorted-world*)
                               (mgl-gpr:literal (position)
                                 (random 8))
                               (mgl-gpr:literal (const-direction)
                                 (random 360.0))
                               (mgl-gpr:literal (const-direction)
                                 (random 360.0))
                               (mgl-gpr:literal (const-distance)
                                 (random 100))
                               (mgl-gpr:literal (const-type) 0)
                               (mgl-gpr:literal (const-type) 1)
                               (mgl-gpr:literal (const-type) 2)
                               (mgl-gpr:literal (const-size)
                                 (random 1024))))

(defvar *sorted-world*)

(let ((out *standard-output*))
  (defun get-score (expr)
    (let ((*standard-output* out)
          (ws (make-client *url*))
          (lock (bt:make-lock)) ; For done-p
          (done-p nil)
          (done-cond (bt:make-condition-variable))
          (score 0)
          (responses 0)
          (life-limit (max 256 (* 32 (mgl-gpr:generation-counter *gp*)))))
      (flet ((done ()
               (bt:with-lock-held (lock)
                 (setf done-p t)
                 (bt:condition-notify done-cond))))
        (send ws (make-join-message))
        (on :close ws
            (lambda (&key code reason)
              (format t "Sent ~A responses~%" responses)
              (when code
                (format t "Closed '~A' (Code=~A) ~%" reason code))
              (done)))
        (on :error ws
            (lambda (error)
              (format t "Error: ~S~%" error)
              (done)))
        (on :message ws
            (lambda (message)
              (let ((l-score (handle-message ws message expr)))
                (incf responses)
                (when l-score
                  (setf score l-score))
                (when (>= responses life-limit)
                  (done)))))
        (start-connection ws)
        (let ((i 0))
          (loop while (and (> 16 i) (not done-p)) do
               (incf i)
               (bt:with-lock-held (lock)
                 (bt:condition-wait done-cond lock :timeout 20))))
        (unless done-p
          (format t "~&Spurious wakeup!~%"))
        (close-connection ws)
        score))))

(defun compile-expression (expr)
  (compile nil `(lambda () ,expr)))

(defun valid-p (expr)
  (handler-case
      (let ((*sorted-world* nil))
        (numberp (eval expr)))
    (error () nil)))

(defun evaluate (gp expr)
  (declare (ignore gp))
  (format t "Expr: ~a~%" expr)
  (if (not (valid-p expr))
      -400
      (let ((score (get-score (compile-expression expr)))
            (size (mgl-gpr:count-nodes expr)))
        (format t "Bot score, size: ~a, ~a~%" score size)
        (cond
          ((> size 128) ;; Penalize big expressions
           (- score (/ (- size 128) 2)))
          ((< size 64) ;; Penalize small expressions
           (- score (- 64 size)))
          (t
           score)))))

(defun mass-evaluate (gp population fitnesses)
  (let* ((len (length population))
         (threads (make-array len)))
    (format t "Mass-evaluate ~S expressions~%" len)
    (dotimes (i len)
      (let ((i i))
        (setf (aref threads i)
              (bt:make-thread (lambda ()
                                (setf (aref fitnesses i)
                                      (evaluate gp (aref population i))))))))
    (dotimes (i len)
      (bt:join-thread (aref threads i)))))

(defun randomize (gp type expr)
  ;; This should modify the expression sometimes instead of replacing with new
  (mgl-gpr:random-gp-expression gp (lambda (level) nil)
                                :type type))

(defun report-fittest (gp fittest fitness)
  (format t "Best fitness until generation ~S: ~S for~%  ~S~%"
          (mgl-gpr:generation-counter gp) fitness fittest))

(defun select (gp fitnesses)
  (declare (ignore gp))
  (mgl-gpr:hold-tournament fitnesses
                           :n-contestants 4))

(defun advance-gp (gp)
  (format t "Generation ~S~%" (mgl-gpr:generation-counter gp))
  (mgl-gpr:advance gp))
