(uiop:define-package #:lbfgs-wrapper/src/cffi/lbfgs-functions
    (:use #:cl
          #:cffi
          #:lbfgs-wrapper/src/cffi/setup
          #:lbfgs-wrapper/src/cffi/definitions)
  (:export #:make-evaluation-callback
           #:make-cffi-interface-evaluation-callback
           #:make-progress-callback
           #:lbfgs-solve-status))

(in-package #:lbfgs-wrapper/src/cffi/lbfgs-functions)


(defparameter *evaluation-fn* nil)
(defparameter *progress-fn* nil)


(defcallback evaluation-callback lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t
    ((instance :pointer)
     (x :pointer)
     (g :pointer)
     (n :int)
     (step lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t))
  (let ((x-array (make-array n :element-type 'double-float)))
    ;; Extract a lisp array for x
    (dotimes (i n)
      (setf (aref x-array i)
            (mem-aref x 'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t i)))
    
    ;; Run the input function
    (destructuring-bind (f g-array) (funcall *evaluation-fn* instance x-array n step)
      
      ;; Put the solved gradient in the g vector
      (dotimes (i n)
        (setf (mem-aref g 'lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t i)
              (aref g-array i)))
      
      ;; Return the output value
      f)))


;; Define evaluation function
;; The evaluation function must have signature
;; (f g) <- (evaluation-fn instance x n step)
(defun make-evaluation-callback (evaluation-fn)
  (setf *evaluation-fn* evaluation-fn)
  (callback evaluation-callback))


(defcallback cffi-interface-evaluation-callback
    lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t
    ((instance :pointer)
     (x :pointer)
     (g :pointer)
     (n :int)
     (step lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t))
  ;; Run the input function
  (funcall *evaluation-fn* instance x g n step))


;; Define cffi interface evaluation function
;; The evaluation function must have signature
;; f <- (evaluation-fn instance x g n step)
(defun make-cffi-interface-evaluation-callback (evaluation-fn)
  (setf *evaluation-fn* evaluation-fn)
  (callback cffi-interface-evaluation-callback))


(defcallback progress-callback :int
    ((instance :pointer)
     (x :pointer)
     (g :pointer)
     (fx lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t)
     (xnorm lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t)
     (gnorm lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t)
     (step lbfgs-wrapper/src/cffi/definitions:lbfgsfloatval-t)
     (n :int)
     (k :int)
     (ls :int))
  (funcall *progress-fn* instance x g fx xnorm gnorm step n k ls))


;; Define progress function
;; The progress-fn must have signature
;; out <- (progress-fn instance x g fx xnorm gnorm step n k ls)
;; where out = 0 signals to keep going
(defun make-progress-callback (progress-fn)
  (setf *progress-fn* progress-fn)
  (callback progress-callback))


;; Return a lisp string describing solve status from an integer code
(defun lbfgs-solve-status (error-code)
  (foreign-string-to-lisp (lbfgs-strerror error-code)))
