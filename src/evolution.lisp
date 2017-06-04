(defparameter *operators* (list ;; Blob related operators
                                (mgl-gpr:operator (blob-direction blob) direction)
                                (mgl-gpr:operator (blob-distance blob) distance)
                                (mgl-gpr:operator (blob-type blob) type)
                                ;; sorted-world related operations
                                (mgl-gpr:operator (get-blob sorted-world position) blob)
                                (mgl-gpr:operator (is-nil blob) cond)
                                ;; Branch
                                (mgl-gpr:operator (if cond
                                                    direction
                                                    direction) direction
                                                  :weight 0.5)
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
                                                  :weight 0.3)))
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
                               (mgl-gpr:literal (const-type) 2)))

(defvar *sorted-world*)

(defun get-score (expr)
  (let ((ws (make-client *url*))
        (lock (bt:make-lock)) ;; Only needed by condition-wait
        (done (bt:make-condition-variable))
        (score 0)
        (responses 0)
        (life-limit (* 512 (+ 1 (mgl-gpr:generation-counter *gp*)))))
    (send ws (make-join-message))
    (on :close ws
        (lambda (&key code reason)
          (format t "Sent ~A responses~%" responses)
          (if code
            (format t "Closed '~A' (Code=~A) ~%" reason code))
          (bt:condition-notify done)))
    (on :error ws
        (lambda (error)
          (format t "Error: ~S~%" error)
          (bt:condition-notify done)))
    (on :message ws
        (lambda (message)
          (let ((l-score (handle-message ws message expr)))
            (incf responses)
            (if l-score
              (setf score l-score))
            (if (>= responses life-limit)
              (bt:condition-notify done)))))
    (start-connection ws)
    (bt:with-lock-held (lock)
                       (bt:condition-wait done lock)
                       (close-connection ws)
                       score)))

(defun evaluate (gp expr)
  (declare (ignore gp))
  (format t "Expr: ~a~%" expr)
  (let ((score (get-score expr))
        (size (mgl-gpr:count-nodes expr)))
        (format t "Bot score, size: ~a, ~a~%" score size)
        (cond
          ((> size 64) ;; Penalize big expressions
           (- score size))
          ((= size 4) ;; Penalize one-liners
           (- score 16))
          (t
            score))))

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
