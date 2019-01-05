(defun append (L1 L2)
	"append L1 by L2 Recursively!"
		(if (null L1)
			 L2
		(cons (car L1) (append (cdr L1) L2) )))

(print "ALL FUNCTIONS IN ORDER")
(print "FIRST PRINTING THE CALL THAT IS MADE")
(print "")
(print '(append (1 3 x a) (4 2 b))  )
(print (append '(1 3 x a) '(4 2 b)))

(defun reverse (lst)
    "Reverse the list recursively!"
	    (if (null lst)
	    	nil
	    (append (reverse (cdr lst))
                (list (car lst)))))

(print '(reverse (a b c d)))
(print (reverse '(a b c d)))

(defun mapcar (f lst)
        (if (null lst)
            nil
        (append (list(funcall f(car lst))) (mapcar f (cdr lst))  )))
        
(defun add3 (x) ( + 3 x  ))
(print'(defun (add3 (x) (+ 3 x))))
(print'(mapcar (add3 (1 2 3 4))))
(print (mapcar 'add3 '(1 2 3 4)))

(defun member (a L)
	(cond 
		((null L) nil)
		((eq a(car L)) t)
		(t (member a (cdr L)))
	)

)

(defun nub (L)
  (cond ((null L) L)
        ((member (car L) (cdr L))
         (nub (cdr L)))
        (t (cons (car L) (nub (cdr L))))))

(print '(nub (1 1 2 4 1 2 5)))
(print (nub '(1 1 2 4 1 2 5)))

(defun fold (v f lst)
    (if (null lst)
        v
    (fold (funcall f v (car lst))  f (cdr lst))))

(print '(fold 10 - (1 3 2)))
(print (fold 10 '-'(1 3 2)))

(defun filter(f lst)
    (if (null lst)
        nil
    (cond 
    ((funcall f (car lst)) (cons (car lst) (filter f (cdr lst))))
    (t (filter f (cdr lst)))
    ))
)
        
(print '(defun lessthan3 (x) (< x 3)))
(print '(filter lessthan3 (1 4 5 2 1 6)))
(defun lessthan3 (x) (< x 3))
(print (filter 'lessthan3 '(1 4 5 2 1 6)))

(defun merge(l1 l2)
    (if (null l2)
        l1
        (if (< (car l1) (car l2))
        (append (list (car l1)) (merge l2 (cdr l1)))
        (append (list (car l2)) (merge l1 (cdr l2))) 
        )
    )
)
(print '(merge (1 3 4 7) (2 3 6)))
(print (merge '(1 3 4 7) '(2 3 6)))

(defun addtoend (c lst)
    (reverse(cons c (reverse lst)))
)
(print'(addtoend d (a b c)))
(print(addtoend 'd '(a b c)))


(defun indexHelp (a lst in)
    (cond 
    ((null lst) -1)
    ((eq (car lst) a ) in)
    (t (indexHelp a (cdr lst) (+ in 1)  ))
    )
)

(defun indexof (a lst)
    (indexHelp a lst 0)
)
(print'(indexof a (b c a d)))
(print(indexof 'a '(b c a d)))
(print'(indexof a (b c d f)))
(print(indexof 'a '(b c d f)))

(defun remove-all(a lst)
    (if (null lst)
        nil
    (cond
    ((eq (car lst) a) (remove-all a (cdr lst)))
    (t(cons (car lst) (remove-all a (cdr lst)) ))
    ))
)
(print'(remove-all a (b a c a a d a)))
(print(remove-all 'a '(b a c a a d a)))

(defun member (a L)
	(cond 
		((null L) nil)
		((eq a(car L)) t)
		(t (member a (cdr L)))
	)
)

(print'(member a (b c a d)))
(print(member 'a '(b c a d)))
(print'(member z (b c a d)))
(print(member 'z '(b c a d)))

;Insert Element into Set
;works but assumes that the set already has duplicates removed
(defun insert (a L)
	(cond
		((null L) L)
		((eq a(car L)) (insert a (cdr L)))
		(t (cons a L))
	)
)

(print'(insert a (b c d)))
(print(insert 'a '(b c d)))
(print'(insert a (a b c d)))
(print(insert 'a '(a b c d)))



;Set Intersection - prints list of elements that L1 and L2 have in common removing duplicates
(defun intersection(L1 L2)
	(cond
		((null L1) nil)
		((null L1) nil)
		((member(car L1) L2) (cons(car L1) (intersection (cdr L2) (cdr L1))))
		(t (intersection (cdr L2) L1))
	)	
)

(print '(intersection (a b c) (a c d)))
(print(intersection '(a b c) '(a c d)))

;Set Union - prints list of elements in both L1 and L2 removing duplicates
(defun union(L1 L2)

	(cond
		((null L1) L2)
		((not (member (car L1) L2)) (cons(car L1) (union L2 (cdr L1)) ))
		(t (union L2 (cdr L1)))
	)
)

(print'(union (a b c) (a c d)))
(print(union '(a b c) '(a c d)))

;Set Difference - prints list of elements in L1 that are not in L2
(defun difference(L1 L2)
	(cond
		((null L1) nil)
		((not(member(car L1) L2)) (cons(car L1) (difference (cdr L1) L2)))
		(t (difference (cdr L1) L2))
	)	
)
(print'(difference (a b c) (a c d)))
(print(difference '(a b c) '(a c d)))
(print'(difference (a c d) (a b c)))
(print(difference '(a c d) '(a b c)))

;Symmetric Difference (disjunctive union) - prints list of elements not in both Lists
(defun symdiff(L1 L2)
	(union (difference L1 L2) (difference L2 L1))	
)
(print'(symdiff (a b c) (a c d)))
(print(symdiff '(a b c) '(a c d)))

;Checks if Subset or Equal
(defun subsetp(L1 L2)
	(cond
		((null L1) t)
		((not(member(car L1) L2)) nil)
		(t (subsetp (cdr L1) L2))
		
	)
)
(print'(subsetp (a b) (a b c d)))
(print(subsetp '(a b) '(a b c d)))


;Checks if Superset or equal
(defun supersetp(L1 L2)
	(cond
		((null L2) t)
		((not(member(car L2) L1)) nil)
		(t (supersetp L1 (cdr L2)))
	)
)
(print '(supersetp (a b c d) (a b)))
(print (supersetp '(a b c d) '(a b)))

;Finds Cardinality of a set
(defun cardinality(L)
	(cond
		((null L) 0)
		(t (+ 1 (cardinality(cdr L))))
	)
)

(print'(cardinality (a b c)))
(print(cardinality '(a b c)))


;Gives the Power set
;the formatting isn't exactly what is listed in the project
;prints nil for the empty set and prints every set in the same line separated by (...)
;pretty sure this is fine
(defun powerset(L)
	(cond
		((null L) '(nil))
		
		(t (powersetHelper(car L) (powerset(cdr l))))
	)
)


;Adds an element to each set
(defun powersetHelper(a L)
	(cond
		((null L) nil)
		(t (cons (car L) (cons (cons a (car L)) (powersetHelper a (cdr L)))))
	)
)

(print'(powerset () ))
(print(powerset '()))
(print'(powerset (a b c)))
(print(powerset '(a b c)))

;absolute value
(defun abs (a) 
	(
		if (>= a 0) 
			a
			(- 0 a)
	)
)
(print'(abs 7))
(print(abs 7))
(print'(abs -7))
(print(abs -7))
;I am not sure how to return a number directly
(defun factorial (a) 
	(
	 if (= a 0)
	 	1
	 	(* a (factorial (- a 1)))
	)
)

(print'(factorial 5))
(print(factorial 5))
;Check if 3 integers can be the lengths of the two sides and the hypoteneuse of a
;right triangle (in that order)
(defun right-tri (a b c)
	(
		if (= (+ (* a a) (* b b)) (* c c))
			t
			nil
	)
)
(print'(right-tri 3 4 5))
(print(right-tri 3 4 5))
(print'(right-tri 1 2 3))
(print(right-tri 1 2 3))

;Greatest Common Divisor (GCD)
(defun gcd (a b) 
	(
		if (= a 0)
			b
			(if (= b 0)
				a
				(if (> a b)
					(gcd b (mod a b))
					(gcd a (mod b a)))
			)
	)
)
(print'(gcd 8 12))
(print(gcd 8 12))

;Least Common Multiple (LCM)
(defun lcm(a b)
	(
		/ (* a b) (gcd a b)
	)

)

(print'(lcm 4 6))
(print(lcm 4 6))


;Nth Fibonacci number (the first two Fibonacci numbers are both 1)
(defun nth-fibo (a)
	(
		if (or (= a 1) (= a 2))
			1
			;return 1
			(+ (nth-fibo (- a 1)) (nth-fibo (- a 2)))
	)
)
(print'(nth-fibo 6))
(print(nth-fibo 6))
(print'(nth-fibo 10))
(print(nth-fibo 10))


;Test if a number is prime
(defun primep (a) 
	(
		if (= (primephelper a (floor (/ a 2))) 1)
			t
			nil
	)
)

(defun primephelper (a b) 
	(
		if (= b 1)
			1
			(if (= (mod a b) 0)
				0
				(primephelper a (- b 1))
			)
	)
)

(print'(primep 5))
(print(primep 5))
(print'(primep 6))
(print(primep 6))

;Nth prime number (the first prime number is 2)
(defun nth-prime (a)
	(
		if (<= a 0)
			nil
			(primeFinderHelper 2 a)

	)
)

(defun primeFinderHelper (curNum goal)

	(
		if (= goal 1)
			(if (primep curNum)
				curNum
				(primeFinderHelper (+ curNum 1) 1)
			)
			(if (primep curNum)
				(primeFinderHelper (+ curNum 1) (- goal 1))
				(primeFinderHelper (+ curNum 1) goal)
			)
	)
)
(print'(nth-prime 6))
(print(nth-prime 6))
(print'(nth-prime 26))
(print(nth-prime 26))

(defun perfectHelper (a curSum numPointer)
	(
		if (> (* numPointer 2) a)
			curSum
			(if (= (mod a numPointer) 0)
				(perfectHelper a (+ curSum numPointer) (+ numPointer 1))
				(perfectHelper a curSum (+ numPointer 1))
			)
	)
)

;Check if a number is perfect: a number is perfect if the sum of its factors other than
;itself is equal to itself.
(defun perfectp (a)
	(
		if (= (perfectHelper a 0 1) a)
			t
			nil
	)
)

(print'(perfectp 5))
(print(perfectp 5))
(print'(perfectp 6))
(print(perfectp 6))
;Check if a number is abundant: an abundant number's sum of factors other than
;itself is greater than the number.
(defun abundantp (a)
	(
		if (> (perfectHelper a 0 1) a)
			t
			nil
	)
)
(print'(abundantp 5))
(print(abundantp 5))
(print'(abundantp 12))
(print(abundantp 12))


;Check if a number is deficient: a deficient number's sum of factors is less than
;itself.
(defun deficientp (a)
	(
		if (< (perfectHelper a 0 1) a)
			t
			nil		
	)
)
(print'(deficientp 5))
(print(deficientp 5))
(print'(deficientp 12))
(print(deficientp 12))


(defun repl ()
        (print "Enter the function you want to call: ")
		(finish-output)
        (setq function (read))
        (princ "Enter the first argument: ")
		(finish-output)
        (setq arg1 (read))
        (princ "Enter the second argument (or N for no second argument): ")
		(finish-output)
        (setq arg2 (read))
        (print "Here is the output: ")
		(finish-output)
        (executeFunction function arg1 arg2)
      
        (print "Enter Q to quit or anything else to continue: ")
		(finish-output)
        (setq cont (read))
        
        (if 
            (equalp 'Q cont) nil
            (repl)
        )
)
(defun executeFunction (f l1 l2)
    (if 
        (equalp 'N l2) (print(funcall f l1))
        (print(funcall f l1 l2))
    )
)

(repl)