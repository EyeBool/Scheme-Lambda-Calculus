; THE LITTLE SCHEMER

; PREFACE
; The goal of this book is to teach the reader to think recursively.
; (When solving a problem, the book goes through the questions that every designer should ask themselves.)

(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))



; CHAPTER 1
; Definitions: atom, list, S-expression, null/empty list
; cdr is pronounced "could-er"
; (cons a l) is read "cons the atom a onto the list l"
; The Law of Car: The primitive car is defined only for non-empty lists.
; The Law of Cdr: The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is another list.
; The Law of Cons: The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list. (In practice, (cons a b) is defined for any a and b, and (car (cons a b)) = a, and (cdr (cons a b)) = b.)
; The Law of Null?: The primitive null? is defined only for lists.
; The Law of Eq: The primitive eq? takes two arguments. Each must be a non-numeric atom. (In practice, eq? can take lists and some numbers as arguments.)



; CHAPTER 2
; cond asks questions, lambda defines a function, define gives it a name
; else is a question whose value is always true
; Ask yourself "how many questions do I need to ask?" That's your number of conds.

(define lat?
	(lambda (l)
		(cond
			((null? l) #t)
			((atom? (car l)) (lat? (cdr l)))
			(else #f))))

(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			((eq? a (car lat)) #t)
			(else (member? a (cdr lat))))))

; The First Commandment (preliminary): Always ask null? as the first question in expressing any function.



; CHAPTER 3

(define rember
	(lambda (a lat)
		(cond
			((null? lat) (quote ()))
			((eq? a (car lat)) (cdr lat))
			(else (cons (car lat) (rember a (cdr lat)))))))

; The Second Commandment: Use cons to build lists.

(define firsts
	(lambda (l)
		(cond
			((null? l) '())
			(else (cons (caar l) (firsts (cdr l)))))))

; The Third Commandment: When building a list, describe the first typical element, and then cons it onto the natural recursion.

(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons old (cons new lat)))
			(else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			(else (cond
				((eq? (car lat) old) (cons new lat))
				(else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			(cond
				((eq? (car lat) old) (cons new (cdr lat)))
				(else (cons (car lat) (subst new old (cdr lat))))))))

(define subst2
	(lambda (new o1 o2 lat)
		(cond
			((null? lat) '())
			(else (cond
				((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
				(else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
	(lambda (a lat)
		(cond
			((null? lat) '())
			(else (cond
				((eq? (car lat) a) (multirember a (cdr lat)))
				(else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			(else (cond
				((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
				(else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; define multiinsertL
; define multisubst
; The Fourth Commandment (preliminary): Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using cdr test termination with null?.



; CHAPTER 4
; This chapter is concerned with natural numbers 0, 1, ...

(define add1
	(lambda (n)
		(+ n 1)))

(define sub1
	(lambda (n)
		(- n 1)))

(define o+
	(lambda (n m)
		(cond
			((zero? n) m)
			(else
				(add1 (o+ (sub1 n) m)))))

; Note that zero? is like null?, and add1 is like cons.

(define o-
	(lambda (n m)
		(cond
			((zero? m) n)
			(else
				(sub1 (o- n (sub1 m)))))))

; A tuple is either an empty list, or its head is a number and its tail is a tuple.

(define addtup
	(lambda (tup)
		(cond
			((null? tup) 0)
			(else
				(o+ (car tup) (addtup (cdr tup)))))))

; The First Commandment (first revision): When recurring on a list of atoms, lat, ask two questions about it (null? lat) and else.When recurring on a number, n, ask two questions about it: (zero? n) and else.
; Something about terminal condition lines and natural recursions.

(define o*
	(lambda (m n)
		(cond
			((zero? n) 0)
			(else
				(o+ m (o* m (sub1 n)))))))

; The Fourth Commandment (first revision): Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using cdr test termination with null? and when using sub1 test termination with zero?.
; The Fifth Commandment: When building a value with +, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition. When building a value with *, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication. When building a value with cons, always use () for the value of the terminating line.

(define tup+
	(lambda (tup1 tup2)
		(cond
			((null? tup1) tup2)
			((null? tup2) tup1)
			(else
				(cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
	(lambda (m n)
		(cond
			((zero? m) #f)
			((zero? n) #t)
			(else (o> (sub1 m) (sub1 n))))))

(define o<
	(lambda (m n)
		(cond
			((zero? n) #f)
			((zero? m) #t)
			(else (o< (sub1 m) (sub1 n))))))

(define o=
	(lambda (n m)
		(cond
			((zero? n) (zero? m))
			((zero? m) #f)
			(else (o= (sub1 n) (sub1 m))))))

(define o^
	(lambda (n m)
		(cond
			((zero? m) 1)
			(else (o* n (o^ n (sub1 m)))))))

(define o/
	(lambda (n m)
		(cond
			((o< n m) 0)
			(else (add1 (o/ (o- n m) m))))))

(define length
	(lambda (lat)
		(cond
			((null? lat) 0)
			(else (add1 (length (cdr lat)))))))

(define pick
	(lambda (n lat)
		(cond
			((o= n 1) (car lat))
			(else (pick (sub1 n) (cdr lat))))))

(define rempick
	(lambda (n lat)
		(cond
			((o= n 1) (cdr lat))
			(else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			(else (cond
				((number? (car lat)) (no-nums (cdr lat)))
				(else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			(else (cond
				((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
				(else (all-nums (cdr lat))))))))

(define occur
	(lambda (a lat)
		(cond
			((null? lat) 0)
			(else (cond
				((eq? (car lat) a) (add1 (occur a (cdr lat))))
				(else (occur a (cdr lat))))))))



; CHAPTER 5

; fakes
(define rember*
	(lambda (a lat)
		(cond
			((null? lat) '())
			(else (cond
				((atom? (car lat)) (cond
					((eq? (car lat) a) (rember* a (cdr lat)))
					(else (cons (car lat) (rember* a (cdr lat))))))
				(cons (rember* a (car lat)) (rember* a (cdr lat))))))))
; fakes
(define insertR*
	(lambda (new old l)
		(cond
			((null? l) '())
			(else (cond
				((atom? (car l)) (cond
					((eq? (car l) old) (cons old (cons new (insertR* new old (cdr lr)))))
					(else (cons (car l) (insertR* new old (cdr l))))))
				(else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))))
				
; The First Commandment (final version): When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else.When recurring on a number, n, ask two questions about it: (zero? n) and else. When recurring on a list of S-expressions, l, ask three questions about it: (null? l), (atom? (car l)), and else.
; The Fourth Commandment (final version): Always change at least one argument while recurring. When recurring on a list of atoms, lat, use (cdr lat). When recurring on a number, n, use (sub1 n). And when recurring on a list of S-expressions, l, use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true. It must be changed towards termination. The changing argument must be tested in the termination condition: when using cdr, test termination with null? and when using sub1, test termination with zero?.

; not fakes
(define occur*
	(lambda (a l)
		(cond
			((null? l) 0)
			((atom? (car l))
				(cond
					((eq? (car l) a) (add1 (occur* a (cdr l))))
					(else (occur* a (cdr l)))))
			(else
				(o+ (occur* a (car l)) (occur* a (cdr l))))))

(define subst*
	(lambda (new old l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((eq? (car l) old) (cons new (subst* new old (cdr l))))
					(else (cons (car l) (subst* new old (cdr l)))))
			(else
				(cons (subst* new old (car l)) (subst* new old (cdr l))))))))
(define member*
	(lambda (a l)
		(cond
			((null? l) #f)
			((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
			(else
				(or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
	(lambda (l)
		(cond
			((atom? (car l)) (car l))
			(else (leftmost (car l))))))

; Note: The evaluation of and and or stop as soon as their value is known (e.g. when and comes across a #f value).

; stopped at p. 90



; CHAPTER 6

; This

(quote +)

; evaluates to the atom +.
; An arithmetic expression is either an atom (including numbers), or two arithmetic expressions combined by +, x, or ^.

(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			(else #f))))

(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
	(lambda (aexp)
		(cond
			((atom? aexp) aexp)
			((eq? (car (cdr aexp)) (quote +)) (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) (quote x)) (x (value (car aexp)) (value (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) (quote ^)) (^ (value (car aexp)) (value (car (cdr (cdr aexp))))))))

(define 1st-sub-exp
	(lambda (aexp)
		(car aexp)))

(define 2nd-sub-exp
	(lambda (aexp)
		(car (cdr (cdr aexp)))))

(define operator
	(lambda (aexp)
		(car (cdr aexp))))

(define value
	(lambda (aexp)
		(cond
			((atom? aexp) aexp)
			((eq? (operator aexp) (quote +)) (+ (value (1st-sub-expression aexp) (value (2nd-sub-expression aexp)))))
			((eq? (operator aexp) (quote x)) (x (value (1st-sub-expression aexp) (value (2nd-sub-expression aexp)))))
			(else (^ (value (1st-sub-expression aexp) (value (2nd-sub-expression aexp))))))))

; The Seventh Commandment: Recur on the subparts that are of the same nature: * On the sublists of a list. * On the subexpressions of an arithmetic expression.
; The Eights Commandment: Use help functions to abstract from representations.
; representations, representations, representations, ...



; CHAPTER - 7
; This chapter is about sets, relations, and functions.

(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat))))))

(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			((member? (car lat) (cdr lat)) (makeset (cdr lat)))
			(else (cons (car lat) (makeset (cdr lat)))))))

(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2) (subset? (cdr set1) set2)) ; this is bad, I didn't cond on the structure of set1
			(else #f))))

(define eqset?
	(lambda (set1 set2)
		(and (subset set1 set2) (subset set2 set1))))

(define intersect?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			((member? (car set1) set2) #t)
			(else (intersect? (cdr set1) set2)))))

(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2)))))

(define union
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2) (union (cdr set1) set2))
			(else (cons (car set1) (union (cdr set1) set2))))))

(define a-pair?
	(lambda (x)
		(cond
			((atom? x) #f)
			((null? x) #f)
			((null? (cdr x)) #f)
			((null? (cdr (cdr x))) #t)
			(else))))

(define first
	(lambda (p)
		(car p)))

(define second
	(lambda (p)
		(car (cdr p))))

(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '()))))

(define rel?
	(lambda (l)
		(cond
			((set? l) #f)
			((null? l) #t)
			((a-pair? (car l)) (rel? (cdr l)))
			(else #f))))

(define fun?
	(lambda (rel)
		(set? (firsts rel))))

(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else
				(cons
					(build (second (car rel)) (first (car rel)))
					(revrel (cdr rel)))))))



; CHAPTER 8
; "replaced eq? with equal? in chapter 5"?
; This chapter is about functions, and how we can pass them to other functions as input, and how functions can return functions as values.
; What is the difference between =, eq?, and equal?.

(define rember-f
	(lambda (test? a l)
		(cond
			((null? l) '())
			((test? (car l) a) (cdr l))
			(else (cons (car l) (rember-f test? a (cdr l)))))))

; A function can return an atom, a list, or another function.

; Breaking down a function that takes multiple arguments into a series of functions that take part of the arguments is called Currying. This allows functions of several arguments to have some of their initial arguments partially applied.

(lambda (a)
	(lambda (x)
		(eq? x a)))

; Currying rember-f

(define rember-f
	(lambda (test?)
		(lambda (a l)
			((null? l) '())
			((test? (car l) a) (cdr l))
			(else (cons (car l) ((rember-f test?) a (cdr l)))))))

; We can do the same with insertL-f, insertR-f, and subst; and abstract all of them as the output of one functional.
; The Ninth Commandment: Abstract common patterns with a new function.
; The Tenth Commandment: Build functions to collect more than one value at a time.
; A collector is sometimes called a continuation.

; This function separates a list of atoms into to lists according to whether the atoms fulfil test? or not, and applies a function to the resulting two lists.
(define collect
	(lambda (lat test? function)
		(cond
			((null? lat)
				(function '() '()))
			((test? (car lat))
				(collect (cdr lat) test?
					(lambda (tobeseen rest)
						(function (cons (car lat) tobeseen) (rest))))))
			(else
				(collect (cdr lat) test?
					(lambda (tobeseen rest)
						(function tobeseen (cons (car lat) rest)))))))

; stopped at p. 141



; CHAPTER 9
; The difference between total and partial functions.
; The halting problem.
; Using the Y combinator to define a recursive function without it refering to itself in its body.

(define keeplooking
	(lambda (a sorn lat)
		(cond
			((number? sorn) (keeplooking a (pick sorn lat) lat))
			(else (eq? sorn a)))))

(define looking
	(lambda (a lat)
		(keeplooking a (pick 1 lat) lat)))

; Defining weight* so that align can be understood in terms of the seventh commandment is interesting.
; This proves that align is a total function.

; Let eternity some function that always enters an infinite loop, say

(define eternity
	(lambda (x)
		(eternity x)))

; We now show that there is not a description of a function will-stop? that determines whether a function will always halt.
; Even though we can describe what will-stop? is, we cannot define it in out language.
; Consider the following function

(define last-try
	(lambda (x)
		(and
			(will-stop? last-try)
			(eternity x))))

; If we assume that (will-stop? last-try) evaluates to #f, then last-try does halt for all inputs, and hence (will-stop? last-try) evaluate to #t; a contradiction.
; If we assume that (will-stop? last-try) evaluates to #t, then (and (will-stop? last-try) (eternity x)), which has the same value as (eternity x), does evalute and give an answer; a contradiction.

(define fact
	(lambda (n)
		(cond
			((zero? n) 1)
			(else
				(* n (fact (- n 1)))))))

; We abstract the internal call of fact as follows

(define make-fact
	(lambda (function)
		(lambda (n)
			(cond
				((zero? n) 1)
				(else
					(* n ((function function) (- n 1))))))))

(define fact*
	(make-fact make-fact))

; We now try to extract (function function) from the definition of make-fact.
; Note that if f is a function of one variable, then so is (lambda (x) (f x))

(define make-fact1
	(lambda (function)
		(lambda (n)
			(cond
				((zero? n) 1)
				(else
					(* n ((lambda (x) ((function function) x)) (- n 1))))))))

; Moving out the new function.

(define make-fact2
	(lambda (function)
		(lambda (fact)
			((lambda (n)
				(cond
					((zero? n) 1)
					(else
						(* n (fact (- n 1))))))
				(lambda (x)
					((function function) x))))))

; We now extract (lambda (fact))

(define make-fact3
	((lambda (f)
		(lambda (function)
			(f (lambda (x)
				(function function) x))))
		[definition of fact]))
