; a recursively defined function f( n + 1 ) = phi(n, f( n )) contains its own name
; we can remove the name of the function f from its definition by definition the following functional F(proc)(n + 1) = phi( n , proc( proc ( n - 1 )))
; in this case, f = F(F)

(define factorial
	(lambda (n)
		(if (zero? n) 1
			(* n (factorial (- n 1))))))

(define factorial-maker
	(lambda (procedure)
		(lambda (n)
			(if (zero? n) 1
				(* n ((procedure procedure) (- n 1)))))))

(define factorial (factorial-maker factorial-maker))

(define factorial-maker
	(lambda (procedure)
		(lambda (func-arg)
			(lambda (n)
				(if (zero? n) 1
					(* n (func-arg (- n 1))))))
		(lambda (arg) ((procedure procedure) arg))))

; the part with func-arg are exactly the part defining the factorial function, let's get it out

(define F*
	(lambda (func-arg)
		(lambda (n)
			(if (zero? n) 1
				(* n (func-arg (- n 1)))))))
(define factorial-maker
	(lambda (procedure)
		(F* (lambda (arg) ((procedure procedure) arg)))))

; which make factorial, after expanding the factorial-maker definition, looks as follows

(define factorial
	((lambda (procedure)
		(F* (lambda (arg) ((procedure procedure) arg))))
		(lambda (procedure)
			(F* (lambda (arg) ((procedure procedure) arg))))))

; we can now write the Y-combinator, where X is the procedure that does our computation

(define Y
	(lambda (X)
		((lambda (procedure)
			(X (lamdba (arg) ((procedure procedure) X))))
			(lambda (procedure)
				(X (lamdba (arg) ((procedure procedure) X)))))))

; and factorial becomes

(define factorial (Y F*))

; so f = Ln.phi(n, f(n - 1)) is converted to the functional F* = Lproc.Ln.phi(n, proc(n - 1)) and we can then write f = (Y F*)

; note that we implement the eager Y combinator (due to Turing) Y_eager = (Lfx. f()) rather than the lazy y combinator Y_lazy = Lf. (Lx. f (x x))(Lx. f (x x)) because it results in an infinite loop
; the lazy Y combinator can only be implemented in a language that supports lazy (non-strict) evaluation, it cannot be used in a language with eager (strict) evaluation order, the last lambda expression is the problem
; we can simplify the above definition of Y by recognizing that we don't need to delay the evaluation of the first lambda expression

(define T
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

; note the trick we used to delay the evaluation of the functions above, we replace

(function arg)

; with

((lambda (x) (function x)) arg)

; fixed point combinators are used to show that lambda calculus is Turing complete
; a functional is a function that operates on other functions
; Y-combinator allows one to implement anonymous recursion in the form of a rewrite rule in languages that do not necessarily support it natively
; Y-combinator enables recursion when it is not possible to refer to the function from within itself
; Y-combinator generalizes recursion
; Y-combinator is a functional
; Y = Lf.((Lx.f (x x))(Lx.f (x x)))
; it works in call by name languages, for call by value languages you need the Z-combinator
