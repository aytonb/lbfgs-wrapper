(uiop:define-package #:lbfgs-wrapper/src/cffi/setup
    (:use #:cl
          #:cffi)
  (:export ))

(in-package #:lbfgs-wrapper/src/cffi/setup)


;; Define where to look for the lbfgs library
(defparameter *lbfgs-library-path* #P"/lbfgs-dependency/")

;; Is L-BFGS loaded
(defparameter *lbfgs-loaded-p* nil)


(defun setup-lbfgs ()
  "Loads the lbfgs library."
  (unless *lbfgs-loaded-p*
    (unless (member *lbfgs-library-path* *foreign-library-directories* :test #'equal)
      (pushnew *lbfgs-library-path* *foreign-library-directories* :test #'equal))
    (unless (member #P"/usr/local/lib/" *foreign-library-directories* :test #'equal)
      (pushnew #P"/usr/local/lib/" *foreign-library-directories* :test #'equal))
    (load-foreign-library "liblbfgs.so")
    (setf *lbfgs-loaded-p* t)))


(setup-lbfgs)
