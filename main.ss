(import (chezscheme))

;; Structure and Interpretation of Computer Programs
;; SICCP


;; ################ 1.9

;; iterative vs recursive
(define (plus a b)
  (if (= a 0)
      b
      (inc (plus (dec a) b))))
;; this one is recursive

(define (plus a b)
  (if (= a 0)
      b
      (plus (dec a) (inc b))))
;; this one is iterative

;; ################ 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;; 1024
(A 2 4)  ;; 65536
(A 3 3)  ;; 65536
;; grows super fast! Ackermann function

(define (f n) (A 0 n))   ;; 2 * n
(define (g n) (A 1 n))   ;; 2 ^ n
(define (h n) (A 2 n))   ;; 2 tetration n
(define (k n) (* 5 n n)) ;; 5n^2

;; 1.2.2 Tree recursion

;; this is a recursive process for fibbonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; iterative proccess for fibbonacci
(define (old-fib-iter a b count)
  (if (= count 0)
      b
      (old-fib-iter (+ a b) a (- count 1))))

(define (old-fib n)
  (old-fib-iter 1 0 n))

;; example: counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;; original first-denomination
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; with a match
(define (first-denomination kinds-of-coins)
  (match kinds-of-coins
    [1 1]
    [2 5]
    [3 10]
    [4 25]
    [5 50]))

;; with list-ref
(define coins '(1 5 10 25 50))
(define (first-denomination n)
  (list-ref coins (- n 1)))

;; 1.11

;; as a recursive process
(define (fun n)
  (cond ((< n 3) n)
        (else (+
               (fun (- n 1))
               (fun (- n 2))
               (fun (- n 3))))))

;; as an iterative process
(define (fun n)
  (define (iter count a b c)
    (if (= count n)
        a
        (iter (+ count 1)
              (+ a (* 2 b) (* 3 c))   ; new a
              a                       ; new b = old a
              b)))                    ; new c = old b
  (if (< n 3)
      n
      (iter 2 2 1 0)))

;; 1.12

(define (pascal-triangle row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+
               (pascal-triangle (- row 1) (- col 1))
               (pascal-triangle (- row 1) col)))))

;; 1.15
(define (cube x) (* x x x))

(define (p x)
  (begin
    (- (* 3 x)
       (* 4 (cube x))))) ;; five times

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 1.16
(define (fast-expt-iter base exponent state)
  (cond ((zero? exponent) state)
        ((even? exponent) (fast-expt-iter (expt base 2) (/ exponent 2) state))
        (else             (fast-expt-iter base (- exponent 1) (* state base)))))

(define (fast-expt base exponent)
  (fast-expr-iter base exponent 1))

;; řešení 2 s named loopem
(define (fast-expt base exponent)
  (let loop ((b base)
             (n exponent)
             (a 1))
    (cond ((zero? n) a)
          ((even? n) (loop (* b b) (/ n 2) a))
          (else      (loop b (- n 1) (* a b))))))

;; 1.17
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))


(define (fast-mul base times)
  (cond ((= times 0) 0)
        ((even? times) (double (fast-mul base (halve times))))
        (else (+ base (fast-mul base (- times 1))))))

;; 1.18
(define (fast-mul-iter base times state)
  (cond ((zero? times) state)
        ((even? times) (fast-mul-iter (double base) (halve times) state))
        (else (fast-mul-iter base (- times 1) (+ state base)))))

(define (fast-mul base times)
  (fast-mul-iter base times 0))

;; 1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (fast-expt p 2) (fast-expt q 2))
                   (+ (fast-expt q 2) (fast-mul 2 (fast-mul p q)))
                   (/ count 2)))
        (else
         (fib-iter (+ (fast-mul b q) (fast-mul a q) (fast-mul a p))
                   (+ (fast-mul b p) (fast-mul a q))
                   p
                   q
                   (- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

;; 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; searching for divisors
(define (square x)
  (* x x))

(define (divides? a b)
  (zero? (remainder b a)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

;; O(sqrt N) primality test
(define (prime? n)
  (= n (smallest-divisor n)))

;; Fermat test
(define (expmod base exp m)
  (cond ((zero? exp) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((zero? times) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; 1.22
(define (timed-prime-test n)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; --solution for 1.22
(define (find-next-prime n)
  (cond ((even? n) (find-next-prime (+ n 1)))
        ((prime? n) n)
        (else (find-next-prime (+ n 2)))))

(define (search-for-primes n count)
  (if (zero? count)
      (void)
      (let ((next-prime (find-next-prime n)))
        (timed-prime-test next-prime)
        (search-for-primes (+ 2 next-prime) (- count 1)))))

;; 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (fast-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (fast-find-divisor n (next test-divisor)))))

(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))

;; 1.24
(define (fast-timed-prime-test n)
  (display n)
  (fast-start-prime-test n (current-time)))

(define (fast-start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (time-difference (current-time) start-time))))

(define (fast-search-for-primes n count)
  (if (zero? count)
      (void)
      (let ((next-prime (find-next-prime n)))
        (fast-timed-prime-test next-prime)
        (fast-search-for-primes (+ 2 next-prime) (- count 1)))))

;; 1.26
(define (orig-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (orig-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (orig-expmod base (- exp 1) m))
                    m))))

(define (louis-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;; issue: expmod called twice!!!!
         (remainder (* (louis-expmod base (/ exp 2) m)
                       (louis-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (louis-expmod base (- exp 1) m))
                    m))))

;; 1.27
(define (carmichael-test n)
  (define (try-it a)
    (= (expmod a n n) (remainder a n)))

  (define (iter a)
    (cond ((= a n) true)                  ; tested all numbers
          ((try-it a) (iter (+ a 1)))     ; this a works, try next
          (else false)))                  ; found counter-example

  (iter 1))

                                        ; Test function
(define (test-number n)
  (display n)
  (display ": ")
  (display (carmichael-test n))
  (display ", prime?: ")
  (display (prime? n))
  (newline))

;; 1.28
(define (nontrivial-square-root? x m)
  (and (not (= x 1))
       (not (= x (- m 1)))
       (= (remainder (square x) m) 1)))

(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (let ((squared (remainder (square x) m)))
      (if (nontrivial-square-root? x m)
          0
          squared)))

  (cond ((= exp 0) 1)
        ((even? exp)
         (squaremod-with-check (miller-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))

  (define (test-iter times)
    (cond ((= times 0) true)
          ((try-it (+ 1 (random (- n 1))))
           (test-iter (- times 1)))
          (else false)))

  (test-iter 100))  ; test 100 random values

(define (test-number n)
  (display n)
  (display ": Miller-Rabin: ")
  (display (miller-rabin-test n))
  (newline))

;; 1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(display (* 8 (pi-sum 1 1000)))
(define pi (* 8 (pi-sum 1 2000)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(newline)
(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))

;; 1.29
(define (simpson f a b n)
  (let* ((h (/ (- b a) n))
         (y (lambda (k) (f (+ a (* k h)))))
         (coeff (lambda (k) (if (or (= k 0) (= k n))
                                1
                                (if (odd? k)
                                    4
                                    2)))))
    (* (/ h 3)
       (sum (lambda (k) (* (coeff k) (y k))) 0 inc n))))

(newline)
(display (simpson cube 0 1 100))

;; 1.30
(define (sum-linear term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; 1.31 a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-wallis k)
  (define (term n) (* (/ (* 2 n) (- (* 2 n) 1)) (/ (* 2 n) (+ (* 2 n) 1))))
  (define (next n) (+ n 1))
  (exact->inexact (* 2 (product term 1 next k))))

;; 1.31 b
(define (product-linear term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; 1.32 a
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

;; 1.32 b
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

;; 1.33 a
(define (filtered-accumulate combiner null-value term a next b pred)
  (define (iter a result)
    (if (> a b)
        result
        (let ((next-a (next a)))
          (if (pred a)
              (iter next-a (combiner result (term a)))
              (iter next-a result)))))
  (iter a null-value))

(define (sum-of-primes-squared a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; 1.33 b
(define (product-of-relative-primes n)
  (define (relatively-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 identity 1 inc (dec n) relatively-prime?))

;; 1.34
(define (f g)
  (g 2))

(f square)

;; (f f) -- we end up applying (2 2)

;; 1.3.3 finding roots of equations
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; 1.3.3 fixed point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; 1.36
(define (loud-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; without average damping
;;(loud-fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;; with average damping
;;(loud-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

;; 1.37 a
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (recur (+ i 1))))))
  (recur 1))

;; 1.37 b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

;; 1.38
(define (d i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

;;(display (exact->inexact (+ 2 (cont-frac (lambda (i) 1.0) d 10))))

;; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

;; 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-dampened x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

;; ((deriv cube) 5)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fpt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; 1.41

;; this trace one isn't in the book
(define (trace x)
  (display x)
  (newline)
  x)

(define (double f)
  (lambda (x) (f (f (trace x)))))

;; (((double (double double)) inc) 5)

;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  (repeated smooth n))

;; 1.45
(define (nth-root x n)
  (fixed-point
   ((repeated average-damp
              (floor (log n 2)))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

;; 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

(define (sqrt-ii x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define (fixed-point-ii f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) 0.00001))
    f)
   first-guess))

;; Part 2 - rational numbers
(define (make-rat n d)
  (cons n d))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; 2.1
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

;; 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-segment s))
         (x-point (end-segment s)))
      2)
   (/ (+ (y-point (start-segment s))
         (y-point (end-segment s)))
      2)))

;; 2.3

;; first representation (using point + width/height)
(define (make-rect1 origin width height)
  (cons origin (cons width height)))

(define (width-rect1 r)
  (car (cdr r)))

(define (height-rect1 r)
  (cdr (cdr r)))

;; second representation (using two points)
(define (make-rect2 p1 p2)
  (cons p1 p2))

(define (width-rect2 r)
  (abs (- (x-point (car r))
          (x-point (cdr r)))))

(define (height-rect2 r)
  (abs (- (y-point (car r))
          (y-point (cdr r)))))

;; common procedures
(define (perimeter-rectangle r width height)
  (* 2 (+ width height)))

(define (area-rectangle r width height)
  (* width height))

;; 2.1.3 pair implementation
(define (cons-fp x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car-fp z) (z 0))
(define (cdr-fp z) (z 1))

;; 2.4
(define (cons-fp2 x y)
  (lambda (m) (m x y)))

(define (car-fp2 z)
  (z (lambda (p q) p)))

(define (cdr-fp2 z)
  (z (lambda (p q) q)))

;; 2.5
(define (cons-2 x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (count-divisions n divisor)
  (if (= (remainder n divisor) 0)
      (+ 1 (count-divisions (/ n divisor) divisor))
      0))

(define (car-2 p)
  (count-divisions p 2))

(define (cdr-2 p)
  (count-divisions p 3))

;; 2.6
(define zero
  (lambda (f) (lambda (x) x)))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; add: compose n applications of f with m applications of f
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; Convert to regular number
(define (church->int n)
  ((n (lambda (x) (+ x 1))) 0))

(newline)
(display (church->int (add one two))) ; => 3

;; 2.1.4 Extended exercise
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 2.10
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (display "Division by interval spanning zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; 2.11
(define (mul-interval x y)
  (let ((xl (lower-bound x)) (xu (upper-bound x))
        (yl (lower-bound y)) (yu (upper-bound y)))
    (cond
     ((and (>= xl 0) (>= xu 0) (>= yl 0) (>= yu 0))
      (make-interval (* xl yl) (* xu yu)))
     ((and (>= xl 0) (>= xu 0) (<= yu 0))
      (make-interval (* xu yl) (* xl yu)))
     ((and (>= xl 0) (>= xu 0) (<= yl 0) (>= yu 0))
      (make-interval (* xu yl) (* xu yu)))
     ((and (<= xu 0) (<= yl 0))
      (make-interval (* xu yu) (* xl yl)))
     ((and (<= xu 0) (>= yu 0))
      (make-interval (* xl yu) (* xu yl)))
     ((and (<= xu 0) (<= yl 0) (>= yu 0))
      (make-interval (* xl yu) (* xl yl)))
     ((and (<= xl 0) (>= xu 0) (<= yu 0))
      (make-interval (* xu yl) (* xl yl)))
     ((and (<= xl 0) (>= xu 0) (>= yl 0))
      (make-interval (* xl yu) (* xu yu)))
     (else
      (make-interval (min (* xl yu) (* xu yl))
                     (max (* xl yl) (* xu yu)))))))

;; shit after
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 2.12
(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (* 100.0
     (/ (width i)
        (center i))))

;; (define i (make-center-percent 100 10)) ; 100 ± 10%
;; (center i)  ; => 100
;; (percent i) ; => 10.0

;; 2.13:
;; For small tolerances, product tolerance ≈ sum of individual tolerances
;; Example: (100±1%) * (200±2%) ≈ 20000±3%
;; Proof: Let a = c1(1±p1), b = c2(1±p2)
;; Then ab ≈ c1c2(1 ± (p1+p2))

;; 2.14:
;; Different because each instance of variable is treated as independent
;; (par1 r1 r2) gives wider intervals than (par2 r1 r2)
;; This happens because we're treating each occurrence of r1,r2 as different intervals
;; Even though they represent same value!

;; 2.15:
;; Eva is right :-)
;; When we use same interval multiple times in computation,
;; we're pretending it can take different values each time
;; Reality: it's same value! So fewer occurrences = tighter bounds
;; This is called "dependency problem" in interval arithmetic

;; 2.16:
;; Implement algebraically equivalent computations that give same answers?
;; This is actually impossible! :-o
;;
;; It's a fundamental problem in interval arithmetic
;; (proven to be NP-hard in general case)
;; Would require tracking dependencies between intervals
;;
;; That's why exercise says it's "beyond scope of this book"
;; and "still a research problem" :-)


;; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

;; 2.18
(define (reverse-list lst)
  (define (rev-iter orig new)
    (if (null? orig)
        new
        (rev-iter (cdr orig) (cons (car orig) new))))
  (rev-iter lst '()))

;; 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (null? coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))

;; (cc 100 us-coins)

;; 2.20
(define (same-parity x . z)
  (define (same? a b)
    (= (remainder a 2) (remainder b 2)))
  (define (filter lst)
    (cond ((null? lst) '())
          ((same? x (car lst))
           (cons (car lst) (filter (cdr lst))))
          (else (filter (cdr lst)))))
  (cons x (filter z)))
