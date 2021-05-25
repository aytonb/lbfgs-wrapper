(uiop:define-package #:lbfgs-wrapper/src/solver/solver
    (:use #:cl
          #:cffi
          #:lbfgs-wrapper/src/cffi/setup
          #:lbfgs-wrapper/src/cffi/definitions
          #:lbfgs-wrapper/src/cffi/lbfgs-functions)
  (:export #:lbfgs-solver
           #:lbfgs-solver-x
           #:lbfgs-solver-fx
           #:lbfgs-solver-evaluation-fn
           #:lbfgs-solver-progress-fn
           #:lbfgs-solver-n
           #:lbfgs-solver-instance
           #:lbfgs-solve
           #:lbfgs-solution))

(in-package #:lbfgs-wrapper/src/solver/solver)


;; Solver class
(defclass lbfgs-solver ()
  ((x
    :initarg :x
    :initform nil
    :accessor lbfgs-solver-x
    :documentation "Initial guess of minimum.")
   (fx
    :initform nil
    :accessor lbfgs-solver-fx
    :documentation "Function value, set at convergence.")
   (evaluation-fn
    :initarg :evaluation-fn
    :initform nil
    :accessor lbfgs-solver-evaluation-fn
    :documentation "Function to optimize.")
   (progress-fn
    :initarg :progress-fn
    :initform nil
    :accessor lbfgs-solver-progress-fn
    :documentation "Progress function")
   (n
    :initarg :n
    :initform nil
    :accessor lbfgs-solver-n
    :documentation "Dimensionality of the input vector.")
   (instance
    :initarg :instance
    :initform (null-pointer)
    :accessor lbfgs-solver-instance
    :documentation "Shared data sent to each function call.")
   (cffi-evaluation
    :initarg :cffi-evaluation
    :initform nil
    :accessor lbfgs-solver-cffi-evaluation
    :documentation "T if using cffi evaluation interface.")
   (max-step
    :initarg :max-step
    :initform nil
    :accessor lbfgs-solver-max-step
    :documentation "Maximum step size taken during line search.")
   (past
    :initarg :past
    :initform nil
    :accessor lbfgs-solver-past
    :documentation "Number of previous iterations to check for delta based convergence.")
   (delta
    :initarg :delta
    :initform nil
    :accessor lbfgs-solver-delta
    :documentation "Objective value tolerance parameter.")))


;; Default progress function
(defun default-progress-fn (instance x g fx xnorm gnorm step n k ls)
  (declare (ignore instance x g fx xnorm gnorm step n k ls))
  0)


;; Verbose header
(defun verbose-header (n)
  (format t "~% k  |    f(x)    |")
  (dotimes (i n)
    (format t "     x[~d]  " i))
  (format t "~%------------------")
  (dotimes (i n)
    (format t "~11,,,va" #\- #\-))
  (format t "~%"))


;; Verbose progress function
(defun verbose-progress-fn (instance x g fx xnorm gnorm step n k ls)
  (declare (ignore instance g xnorm gnorm step ls))
  (format t "~3d | ~10,4f |" k fx)
  (dotimes (i n)
    (format t " ~10,4f"
            (mem-aref x 'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t i)))
  (format t "~%")
  0)


;; Solve the problem
(defmethod lbfgs-solve ((solver lbfgs-solver))
  (let (x-ptr
        evaluation-callback
        progress-callback)
    (handler-case
        ;; Make the function value pointer
        (with-foreign-object
            (fx-ptr 'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t 1)
          ;; Make the initial guess
          (setf x-ptr (lbfgs-malloc (lbfgs-solver-n solver)))
          (dotimes (i (lbfgs-solver-n solver))
            (setf (mem-aref x-ptr
                            'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t i)
                  (aref (lbfgs-solver-x solver) i)))
          
          ;; Setup a default parameter structure 
          (with-foreign-object (param '(:struct lbfgs-parameter-t))
            (lbfgs-parameter-init param)
            
            ;; Override default parameters if specified
            (when (lbfgs-solver-max-step solver)
              (setf (foreign-slot-value param '(:struct lbfgs-parameter-t) 'max-step)
                    (coerce (lbfgs-solver-max-step solver) 'double-float)))
            (when (lbfgs-solver-delta solver)
              (setf (foreign-slot-value param '(:struct lbfgs-parameter-t) 'delta)
                    (coerce (lbfgs-solver-delta solver) 'double-float)))
            (when (lbfgs-solver-past solver)
              (setf (foreign-slot-value param '(:struct lbfgs-parameter-t) 'past)
                    (coerce (lbfgs-solver-past solver) 'integer)))
            
            ;; Make the evaluation callback
            (if (lbfgs-solver-cffi-evaluation solver)
                (setf evaluation-callback
                      (make-cffi-interface-evaluation-callback
                       (lbfgs-solver-evaluation-fn solver)))
                (setf evaluation-callback
                      (make-evaluation-callback
                       (lbfgs-solver-evaluation-fn solver))))
            
            ;; Make the progress callback, using the default if not specified
            (case (lbfgs-solver-progress-fn solver)
              ((nil) (setf progress-callback
                           (make-progress-callback #'default-progress-fn)))
              (:verbose (verbose-header (lbfgs-solver-n solver))
               (setf progress-callback
                     (make-progress-callback #'verbose-progress-fn)))
              (otherwise (setf progress-callback
                               (make-progress-callback
                                (lbfgs-solver-progress-fn solver)))))
            
            ;; Run L-BFGS
            (lbfgs-solve-status
             (lbfgs (lbfgs-solver-n solver)
                    x-ptr
                    fx-ptr
                    evaluation-callback
                    progress-callback
                    (lbfgs-solver-instance solver)
                    param))

            ;; Extract solution values
            (setf (lbfgs-solver-x solver)
                  (make-array (lbfgs-solver-n solver) :element-type 'double-float))
            (dotimes (i (lbfgs-solver-n solver))
              (setf (aref (lbfgs-solver-x solver) i)
                    (mem-aref x-ptr
                              'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t
                              i)))
            (setf (lbfgs-solver-fx solver)
                  (mem-ref fx-ptr 'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t))

            ;; Clear the pointer
            (lbfgs-free x-ptr)))

      ;(error (e)
      ; (lbfgs-free x-ptr)
      ;  (error e))
      )))


;; Extract solution
(defmethod lbfgs-solution ((solver lbfgs-solver))
  (unless (lbfgs-solver-fx solver)
    (error "L-BFGS problem has not been solved."))
  (list (lbfgs-solver-x solver) (lbfgs-solver-fx solver)))


;; Test
(defun test-lbfgs ()
  (let (evaluation-fn
        solver)
    (setf evaluation-fn
          (lambda (instance x-array n step)
            (declare (ignore instance n step))
            (list (expt (aref x-array 0) 2)
                  (make-array 1 :initial-contents (list (* 2 (aref x-array 0)))))))
    (setf solver (make-instance 'lbfgs-solver
                                :x (make-array 1 :initial-contents (list 0.5d0))
                                :evaluation-fn evaluation-fn
                                :n 1))
    (lbfgs-solve solver)
    (lbfgs-solution solver)))

(defun test-lbfgs-2 ()
  (let (evaluation-fn
        solver)
    (setf evaluation-fn
          (lambda (instance x-array n step)
            (declare (ignore instance n step))
            (list (expt (- (aref x-array 0) 3) 2)
                  (make-array 1 :initial-contents (list (* 2 (- (aref x-array 0) 3)))))))
    (setf solver (make-instance 'lbfgs-solver
                                :x (make-array 1 :initial-contents (list 0.5d0))
                                :evaluation-fn evaluation-fn
                                :progress-fn :verbose
                                :n 1))
    (lbfgs-solve solver)
    (lbfgs-solution solver)))
