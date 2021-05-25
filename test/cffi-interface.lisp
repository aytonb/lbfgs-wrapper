(uiop:define-package #:lbfgs-wrapper/test/cffi-interface
    (:use #:cl
          #:cffi
          #:fiveam
          #:lbfgs-wrapper
          #:lbfgs-wrapper/test/basic))

(in-package #:lbfgs-wrapper/test/cffi-interface)


;; Rosenbrock test function, f(x) = (1 - x)^2 + 100 (y - x^2)^2
;; f_min = 0, x_min = (1, 1)
(defun rosenbrock-fn (instance x g n step)
  (declare (ignore instance n step))
  (let ((x0 (mem-aref x 'lbfgsfloatval-t 0))
        (x1 (mem-aref x 'lbfgsfloatval-t 1)))
    (setf (mem-aref g 'lbfgsfloatval-t 0) (+ (* -2 (- 1 x0))
                                             (* -400 (- x1 (expt x0 2)) x0))
          (mem-aref g 'lbfgsfloatval-t 1) (* 200 (- x1 (expt x0 2))))
    (+ (expt (- 1 x0) 2)
       (* 100 (expt (- x1 (expt x0 2)) 2)))))


(def-suite cffi-interface-tests)
(in-suite cffi-interface-tests)


(test rosenbrock-cffi-interface
  (let ((solver (make-instance 'lbfgs-solver
                               :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                               :evaluation-fn #'rosenbrock-fn
                               :n 2
                               :cffi-evaluation t))
        solution)
    (lbfgs-solve solver)
    (setf solution (lbfgs-solution solver))
    (is (approximately-equal (aref (first solution) 0) 1d0))
    (is (approximately-equal (aref (first solution) 1) 1d0))
    (is (approximately-equal (second solution) 0d0))))

