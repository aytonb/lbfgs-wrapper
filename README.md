# L-BFGS Wrapper for Common Lisp


## Purpose

This package provides an interface that allows the limited memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) algorithm to be called in Common Lisp. It is a very thin wrapper around the MIT-licensed C implementation written by Naoaki Okazaki, available at https://github.com/chokkan/liblbfgs.

L-BFGS is a quasi-Newton unconstrained minimization algorithm. It converges to a local minimum of a function given a method to evaluate the function value and gradients.

There doesn't appear to be a readily available implementation of L-BFGS in Common Lisp. GSLL has an implementation of non-limited memory BFGS, but the limited memory variant can be significantly faster and more stable. Maxima provides an L-BFGS implementation, but it is not clear how to run it outside of Maxima's interal lisp. 


## License

This project is released under the BSD 3-Clause License. This is research level code, provided in an "as is" state.


## Dependencies

The following dependency **not available in quicklisp** is required:

- [liblbfgs](https://github.com/chokkan/liblbfgs) (MIT license)

The following dependencies **available in quicklisp** are required:

- [cffi](https://github.com/cffi/cffi) (MIT license)
- [fiveam](https://github.com/lispci/fiveam) (Custom permissive license)


## Setup

You will need to clone and compile the C implementation of L-BFGS from https://github.com/chokkan/liblbfgs, creating the compiled library liblbfgs.so.

lbfgs-wrapper will search for liblbfgs.so using CFFI, so it should be found if it is anywhere that CFFI typically looks. In addition, for convenience, CFFI will look in the directory `/lbfgs-dependency/`.

Verify your installation using `(asdf:test-sytem :lbfgs-wrapper)`.


## Quick-Start

Here is an example using L-BFGS to minimize the Rosenbrock function, which is a function of two variables, f(x,y) = (1 - x)^2 + 100*(y - x^2)^2. Its minimum is f(x) = 0 at x = (1, 1).

```
(use-package :lbfgs-wrapper)

;; Function definition, returns (f(x) g(x))
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
 
;; Define the solver, with initial guess
;; All parameters must be double-floats!
(defparameter solver (make-instance 'lbfgs-solver
                                    :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                                    :evaluation-fn #'rosenbrock-fn
                                    :n 2))
                                    
;; Solve
(lbfgs-solve solver)

(lbfgs-solution solver)
;; => (#(1.0000014280435368d0 1.0000028443341438d0) 2.0531262724834173d-12)
```

## Minimization Target Definition

The function to be minimized has the following signature.

```
(f g) <= (fn-to-min instance x n step)
```

- `f` is the value of function at input `x`. Must be double float when all elements of `x` are double floats.
- `g` is a vector of length `n` which contains the gradient of the function at input `x`. Must be double float when all elements of `x` are double floats.
- `instance` is a pointer to a C object that is constant throughout the optimization process. Use this to store expensive data that you don't want to have to bake into the function or recompute at every step.
- `x` is a vector of length `n` where the function must be evaluated.
- `n` is an integer that gives the number of input variables to the function.
- `step` is a double float giving the step size for line search.


## lbfgs-solver Definition

The init-args for the lbfgs-solver class are

- `:x`, a vector of length `n` containing the initial guess of the minimum. All elements must be double floats.
- `:evaluation-fn`, the handle of the function to minimize.
- `:n`, the number of input variables to the function.
- `:progress-fn`, `:verbose` to print out convergence information, or the handle of a custom progress function.
- `:instance`, a pointer to a C object that is constant throughout the optimization process.
- `:cffi-evaluation`, set to `T` if you want the evaluation function to operate directly on C pointers instead.


## Progress Function Definition

A progress function is called at each iteration of optimization. It is useful for printing information about the optimization, or for halting optimization when certain conditions are reached. 

If you just want to print out function values to monitor convergence, then using the `:verbose` progress function argument will use a pre-defined print function, as below.

```
;; Define the solver, with initial guess
;; All parameters must be double-floats!
(defparameter solver (make-instance 'lbfgs-solver
                                    :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                                    :evaluation-fn #'rosenbrock-fn
                                    :progress-fn :verbose
                                    :n 2))
                                    
;; Solve
(lbfgs-solve solver)
;; =>
;;  k  |    f(x)    |     x[0]       x[1]  
;; ----------------------------------------
;;   1 |    69.7454 |     1.2077     2.2935
;;   2 |     3.3149 |     1.4320     2.2274
;;   3 |     0.6247 |     1.5077     2.2125
;;   4 |     0.2395 |     1.4869     2.2159
;;   5 |     0.2383 |     1.4879     2.2155
;;   6 |     0.2379 |     1.4877     2.2141
;;   7 |     0.2366 |     1.4864     2.2084
;;   8 |     0.2318 |     1.4794     2.1842
;;   9 |     0.2223 |     1.4625     2.1299
;;  10 |     0.2059 |     1.4287     2.0264
;;  11 |     0.1826 |     1.3904     1.9159
;;  12 |     0.1304 |     1.3274     1.7469
;;  13 |     0.0767 |     1.2756     1.6243
;;  14 |     0.0586 |     1.2325     1.5122
;;  15 |     0.0416 |     1.1383     1.2807
;;  16 |     0.0203 |     1.1424     1.3043
;;  17 |     0.0105 |     1.1012     1.2110
;;  18 |     0.0058 |     1.0684     1.1380
;;  19 |     0.0020 |     1.0326     1.0631
;;  20 |     0.0004 |     1.0200     1.0408
;;  21 |     0.0000 |     1.0022     1.0039
;;  22 |     0.0000 |     1.0017     1.0032
;;  23 |     0.0000 |     1.0000     1.0001
;;  24 |     0.0000 |     1.0000     1.0000


(lbfgs-solution solver)
;; => (#(1.0000014280435368d0 1.0000028443341438d0) 2.0531262724834173d-12)
```

You can define any progress function and use that instead by passing the function handle to the `:progress-fn` initarg of lbfgs-solver. The progress function has the following signature.

```
continue <= (progress-fn instance x g fx xnorm gnorm step n k ls) 
```

- `continue` is an integer value. `0` signals that optimization should continue, any other value will halt optimization.
- `instance` is a pointer to a C object that is constant throughout the optimization process. Use this to store expensive data that you don't want to have to bake into the function or recompute at every step.
- `x` is a pointer to a C array of length `n` of double floats, where the function has been evaluated.
- `g` is a pointer to a C array of length `n` of double floats, containing the gradient of the function at `x`.
- `fx` is a double float of the value of the function at input `x`.
- `xnorm` is the Euclidean norm of `x`.
- `gnorm` is the Euclidean norm of `g`.
- `step` is a double float giving the step size for line search.
- `n` is an integer that gives the number of input variables to the function.
- `k` is the iteration count.
- `ls` is the number of function evaluations for the current iteration.


## (Advanced) Using the CFFI Interface for Evaluation Functions

There is a bit of inefficiency in the formulation described above. Every time an evaluation is needed at x, lbfgs-solver converts the C array of x to a lisp array, and then converts the lisp array of g to a C array to be used by liblbfgs. This way, you as the user can be blind to CFFI structures when defining the evaluation function. However, this can be wasteful, especially when the function is cheap to evaluate or when the number of dimensions is large.

Setting `:cffi-evaluation` to `T` in the lbfgs-solver will let you use a different signature for the evaluation function that operates on C arrays for x and g directly. This can be more efficient, but requires the user to make use of CFFI pointer reading functions. In this case, the signature of the function must be as follows.

```
f <= (fn-to-min instance x g n step)
```

- `f` is the value of function at input `x`. Must be double float when all elements of `x` are double floats.
- `instance` is a pointer to a C object that is constant throughout the optimization process. Use this to store expensive data that you don't want to have to bake into the function or recompute at every step.
- `x` is a pointer to a C array of length `n` of double floats, where the function must be evaluated.
- `g` is a pointer to a C array of length `n` of double floats, where the computed gradient of the function at input `x` must be placed.
- `n` is an integer that gives the number of input variables to the function.
- `step` is a double float giving the step size for line search.

An example use for the Rosenbrock function is below.

```
(use-package :lbfgs-wrapper)
(use-package :cffi)

;; Function definition, returns f(x)
(defun rosenbrock-cffi-fn (instance x g n step)
  (declare (ignore instance n step))
  (let ((x0 (mem-aref x 'lbfgsfloatval-t 0))
        (x1 (mem-aref x 'lbfgsfloatval-t 1)))
    (setf (mem-aref g 'lbfgsfloatval-t 0) (+ (* -2 (- 1 x0))
                                             (* -400 (- x1 (expt x0 2)) x0))
          (mem-aref g 'lbfgsfloatval-t 1) (* 200 (- x1 (expt x0 2))))
    (+ (expt (- 1 x0) 2)
       (* 100 (expt (- x1 (expt x0 2)) 2)))))
 
;; Define the solver, with initial guess
;; All parameters must be double-floats!
(defparameter solver (make-instance 'lbfgs-solver
                                    :x (make-array 2 :initial-contents (list 0.5d0 3d0))
                                    :evaluation-fn #'rosenbrock-cffi-fn
                                    :n 2
                                    : cffi-evaluation t))
                                    
;; Solve
(lbfgs-solve solver)

(lbfgs-solution solver)
;; => (#(1.0000014280435368d0 1.0000028443341438d0) 2.0531262724834173d-12)
```

## TODOs

- Expose the L-BFGS algorithm parameters, which are currently set to default values.

