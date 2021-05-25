(uiop:define-package #:lbfgs-wrapper/src/cffi/definitions
    (:use #:cl
          #:cffi
          #:lbfgs-wrapper/src/cffi/setup)
  (:export #:lbfgsfloatval-t
           #:lbfgs-parameter-t
           #:min-step
           #:max-step
           #:past
           #:delta
           #:lbfgs-parameter-init
           #:lbfgs-malloc
           #:lbfgs-free
           #:lbfgs
           #:lbfgs-strerror))

(in-package #:lbfgs-wrapper/src/cffi/definitions)


;; Define types
(defctype lbfgsfloatval-t :double)

(defcstruct lbfgs-parameter-t
  ;; Corrections to approximate inverse Hessian
  (m :int)
  ;; Epsilon for convergence
  (epsilon lbfgsfloatval-t)
  ;; Iterations for delta based convergence
  (past :int)
  ;; Delta for convergence test
  (delta lbfgsfloatval-t)
  ;; Max number of iterations
  (max-iterations :int)
  ;; Line search algorithm to be used
  (linesearch :int)
  ;; Maximum number of trials for linesearch
  (max-linesearch :int)
  ;; Minimum step of linesearch
  (min-step lbfgsfloatval-t)
  ;; Maximum steo of linesearch
  (max-step lbfgsfloatval-t)
  ;; Accuracy of linesearch
  (ftol lbfgsfloatval-t)
  ;; Wolfe condition coefficient
  (wolfe lbfgsfloatval-t)
  ;; Accuracy of linesearch for gradient
  (gtol lbfgsfloatval-t)
  ;; Machine precision for floating points
  (xtol lbfgsfloatval-t)
  ;; Coefficient for the L1 norm of variables
  (orthantwise-c lbfgsfloatval-t)
  ;; Start index for L1 norm
  (orthantwise-start :int)
  ;; End index for L1 norm
  (orthantwise-end :int))


;; Define functions
(defcfun ("lbfgs_parameter_init" lbfgs-parameter-init) :void
  (param :pointer))

;; Works with:

;; (with-foreign-object (ptr '(:struct lbfgs-parameter-t))
;;   (lbfgs-parameter-init ptr)
;;   (with-foreign-slots ((m epsilon) ptr (:struct lbfgs-parameter-t))
;;     (list m epsilon)))


(defcfun ("lbfgs_malloc" lbfgs-malloc) :pointer
  (n :int))

;; Works with

;; (defparameter x (lbfgs-malloc 2))
;; (setf (mem-aref x 'lbfgsfloatval-t 1) 0.5d0)


(defcfun ("lbfgs_free" lbfgs-free) :void
  (x :pointer))

;; Works with

;; (lbfgs-free x)


(defcfun ("lbfgs" lbfgs) :int
  (n :int)
  (x :pointer)
  (fx-ptr :pointer)
  (proc-evaluate :pointer)
  (proc-progress :pointer)
  (instance :pointer)
  (param :pointer))


(defcfun ("lbfgs_strerror" lbfgs-strerror) :pointer
  (err :int))
