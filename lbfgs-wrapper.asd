(defsystem #:lbfgs-wrapper
  :description "Call L-BFGS from lisp!"
  :pathname ""
  :class :package-inferred-system
  :depends-on (#:lbfgs-wrapper/lbfgs-wrapper)
  :in-order-to ((test-op (load-op "lbfgs-wrapper/test/all")))
  :perform (test-op (op c) (symbol-call :fiveam :run-all-tests)))
