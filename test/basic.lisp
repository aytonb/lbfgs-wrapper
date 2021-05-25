(uiop:define-package #:lbfgs-wrapper/test/basic
    (:use #:cl
          #:lbfgs-wrapper
          #:fiveam)
  (:export #:approximately-equal))

(in-package #:lbfgs-wrapper/test/basic)


;; Convenience definition
(defun approximately-equal (x y)
  (<= (abs (- x y)) 1d-5))


;; Quadratic test function, f(x) = (x - 3)^2 + 2
;; f_min = 2, x_min = 3
(defun quadratic-fn (instance x n step)
  (declare (ignore instance n step))
  (list (+ (expt (- (aref x 0) 3) 2) 2)
        (make-array 1 :initial-contents (list (* 2 (- (aref x 0) 3))))))


;; Rosenbrock test function, f(x) = (1 - x)^2 + 100 (y - x^2)^2
;; f_min = 0, x_min = (1, 1)
(defun rosenbrock-fn (instance x n step)
  (declare (ignore instance n step))
  (list (+ (expt (- 1 (aref x 0)) 2)
           (* 100 (expt (- (aref x 1) (expt (aref x 0) 2)) 2)))
        (make-array 2 :initial-contents
                    (list (+ (* -2 (- 1 (aref x 0)))
                             (* -400
                                (- (aref x 1) (expt (aref x 0) 2))
                                (aref x 0)))
                          (* 200
                             (- (aref x 1) (expt (aref x 0) 2)))))))


(def-suite basic-tests)
(in-suite basic-tests)


(test quadratic-basic
  (let ((solver (make-instance 'lbfgs-solver
                               :x (make-array 1 :initial-contents (list 0.5d0))
                               :evaluation-fn #'quadratic-fn
                               :n 1))
        solution)
    (lbfgs-solve solver)
    (setf solution (lbfgs-solution solver))
    (is (approximately-equal (aref (first solution) 0) 3d0))
    (is (approximately-equal (second solution) 2d0))))


(test quadratic-verbose
  (let ((solver (make-instance 'lbfgs-solver
                               :x (make-array 1 :initial-contents (list 0.5d0))
                               :evaluation-fn #'quadratic-fn
                               :progress-fn :verbose
                               :n 1))
        solution)
    (lbfgs-solve solver)
    (setf solution (lbfgs-solution solver))
    (is (approximately-equal (aref (first solution) 0) 3d0))
    (is (approximately-equal (second solution) 2d0))))


(test rosenbrock-basic
  (let ((solver (make-instance 'lbfgs-solver
                               :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                               :evaluation-fn #'rosenbrock-fn
                               :n 2))
        solution)
    (lbfgs-solve solver)
    (setf solution (lbfgs-solution solver))
    (is (approximately-equal (aref (first solution) 0) 1d0))
    (is (approximately-equal (aref (first solution) 1) 1d0))
    (is (approximately-equal (second solution) 0d0))))


(test rosenbrock-verbose
  (let ((solver (make-instance 'lbfgs-solver
                               :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                               :evaluation-fn #'rosenbrock-fn
                               :progress-fn :verbose
                               :n 2))
        solution)
    (lbfgs-solve solver)
    (setf solution (lbfgs-solution solver))
    (is (approximately-equal (aref (first solution) 0) 1d0))
    (is (approximately-equal (aref (first solution) 1) 1d0))
    (is (approximately-equal (second solution) 0d0))))


;; (test rosenbrock-verbose-max-step
;;   (let ((solver (make-instance 'lbfgs-solver
;;                                :x (make-array 2 :initial-contents (list 0.5d0 3d0))
;;                                :evaluation-fn #'rosenbrock-fn
;;                                :progress-fn :verbose
;;                                :n 2
;;                                :max-step 1d-2))
;;         solution)
;;     (lbfgs-solve solver)
;;     (setf solution (lbfgs-solution solver))
;;     (is (approximately-equal (aref (first solution) 0) 1d0))
;;     (is (approximately-equal (aref (first solution) 1) 1d0))
;;     (is (approximately-equal (second solution) 0d0))))

