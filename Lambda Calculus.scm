; LAMBDA CALCULUS
; Invented by Alonzo Church in the 1930s.
; Everything is a unary function. There are no other primitive types.
; The order of evaltuation is irrelevant. (Scheme requires eager evaluation where arguments are evaluated before applying the function.)
; No special forms.


; SYNTAX
; Lambda terms E, F, G, ... are defined inductively:
;	1. Variables: Any variable x, y, z, ... is a lambda term.
;	2. Application: If E and F are lambda terms, then (EF) is a lambda term.
;	3. Abstraction: If E is a lambda term and x is a variable, then (Lx.E) is a lambda term.

; We can simplify the notation by agreeing that abstraction associates to the right and abstraction associates to the left, and ommitting brackets.


; SUBSTITUTION
;	1. Alpha-reduction (renaming bound variables): (Lx.E) => (Ly.E[x/y]), where E[x/y] denotes the result of replacing all free occurences of x in E by y, provided y would not be captured by an Ly already in E.
;	2. Beta-reduction (substitution rule): ((Lx.E) F) => E[x/F], where E[x/F] denotes the result of replacing all free occurences of x in E by F. Before doing this, bound variables in E are renamed by alpha-reduction if necessary to avoid capturing free variables in F.
; An alpha- or beta-reduction step can be performed at any time to any subterm of a lambda term at which the rule is applicable. We write E=>F if E goes to F in some finite number of alpha- or beta-reduction steps.

; Church-Rosser Theorem: If E=>F1, and E=>F2, then there exists a G such that F1=> G and F2=>G.

; We say that two terms E and F are equivalent, and we write E==F, if there exists a G such that E=>G and F=>G. The relation == is an equivalence relation.


; CURRYING
; Functions of higher arity are simulated in lambda calculus by currying. For example, f(x, y) is simulated by (Lx.(Ly.f(x, y))).

; ENCODING DATA TYPES

; I. Boolean
; #T := (Lx.(Ly.x)), and #F := (Lx.(Ly.y))
; IF := (Lx.x)
; AND := (Lx.(Ly.((xy)x)))
; OR := (Lx.(Ly.((xx)y)))

; II. Pairs and Lists
; NULL := Lx.#T
; CONS := Lx.Ly.Lc.cxy
; CAR := #T
; CDR := #F
; NULL? := Ld.(d(Lx.Ly.#F))

; III. Numbers and Arithmetic
; Church numerals
; 0 := Lf.Lx.x
; 1 := Lf.Lx.fx
; 2: = Lf.Lx.ffx
; and so on, with n being the result of composing f with itself n times.
; ZERO? := Ln.(n Lz.#F#T)
; SUCC = ADD1 := Ln.Lf.Lx.f(nfx)
; SUB1 := Lx.(CONS (ADD1 (CAR x)) x)
; + := Ln.Lm.(n SUCC m)


; IV. Recursion
; Y-combinator.
