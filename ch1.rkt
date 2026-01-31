#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square a)
  (* a a))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (identity x) x)

(define (avg a b)
  (/ (+ a b) 2))

(define (1+ a)
  (+ a 1))

(define (1- a)
  (- a 1))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.3
;; Procedure to add the two larger nums
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-add a b)
  (+ (square a) (square b)))

(define (larger-square-add a b c)
  (cond ((and (< a b) (< a c)) (square-add b c))
         ((and (< b a) (< b c)) (square-add a c))
         ((and (< c a) (< c b)) (square-add a b))))

(larger-square-add 1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.6
;; What happens if we use new-if?
;; Answer, if new-if is used, then both y and the
;; else clause are evaluated before being passed
;; to the new-if procedure, resulting in a never
;; ending recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt x)
  (define (iter x y)
    (if (good-enough? x y)
        y
        (iter x (improve x y))))
  (define (good-enough? x y)
    (< (abs (- (square y) x)) 0.001))
  (define (improve x y)
    (avg (/ x y) y))
  (iter x 1.0))

(sqrt 4) ;; 2
(sqrt 2) ;; 1.41...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.7
;; What happens for very small or very large nums?
;; Can we implement the better good-enough procedure?
;; Answer, for small numbers it's wrong
;; since anything below 0.001 will pass, for huge numbers
;; it'll hang because it's trying to get to 0.001 precision
;; The sqrt2 good-enough? procedure performs way better since
;; it's relative to the change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt2 x)
  (define (iter x y prev)
    (if (good-enough? prev y)
        y
        (iter x (improve x y) y)))
  (define (good-enough? prev guess)
    (< (/ (abs (- guess prev)) guess) 0.001))
  (define (improve x y)
    (avg (/ x y) y))
  ;; any num will work instead of 2.0 if it's far enough from 1.0
  (iter x 1.0 2.0))

(sqrt2 0.01) ;; 0.1000....
(sqrt2 4) ;; 2
(sqrt2 400000000) ;; 20000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.8
;; Implement cube root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cubrt x)
  (define (iter x y prev)
    (if (good-enough? prev y)
        y
        (iter x (improve x y) y)))
  (define (good-enough? prev guess)
    (< (/ (abs (- guess prev)) guess) 0.001))
  (define (improve x y)
    (/ (+ (/ x (square y))
          (* 2 y))
       3))
  (iter x 2.0 1.0))

(cubrt 8) ;; 2
(cubrt 27) ;; 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture 2A - Abstracting the sum and sqrt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simple-sum a b)
  (sum identity a 1+ b))

(define (square-sum a b)
  (sum square a 1+ b))

(define (fixed-point f start)
  (define (close-enough? old new)
    (< (/ (abs (- old new)) new) 0.001))
  (define (iter old new)
    (if (close-enough? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (easier-sqrt x)
  (define (improve y) (avg (/ x y) y))
  (fixed-point improve 1.0))

(easier-sqrt 9) ;; 3
(easier-sqrt 4) ;; 2

(define (easier-cubrt x)
  (fixed-point
   (lambda (y) (/ (+ (/ x (square y)) (* 2 y)) 3))
   1.0))

(easier-cubrt 8) ;; 2
(easier-cubrt 27) ;; 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.11
;; Implement the function f(n) (reference book)
;; with a recursive and iterative function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f-rec n)
  (if (< n 3)
      n
      (+
       (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(f-rec 1) ;; 1
(f-rec 2) ;; 2
(f-rec 3) ;; 4
(f-rec 4) ;; 11
(f-rec 5) ;; 25

(define (f-it n)
  (define (iter a b c i)
    (if (= i 2)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (- i 1))))
  (if (< n 3)
      n
      (iter 0 1 2 n)))

(f-it 1) ;; 1
(f-it 2) ;; 2
(f-it 3) ;; 4
(f-it 4) ;; 11
(f-it 5) ;; 25

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.12
;; Write a procedure that computes elements of Pascal's triangle
;; by means of a recursive process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pascal r c)
  (cond ((and (= r 0) (= c 0)) 1)
        ((or (< c 0) (> c r)) 0)
        (else (+ (pascal (1- r) (1- c)) (pascal (1- r) c)))))

(pascal 0 0) ;; 1
(pascal 1 0) ;; 1
(pascal 1 1) ;; 1
(pascal 2 0) ;; 1
(pascal 2 1) ;; 2
(pascal 2 2) ;; 1
(pascal 3 0) ;; 1
(pascal 3 1) ;; 3
(pascal 3 2) ;; 3
(pascal 3 3) ;; 1

(define (build-pascal n)
  (define (iter lst i max-i)
    (if (> i max-i)
        lst
        (iter (cons (inner '() 0 i) lst) (1+ i) max-i)))
  (define (inner lst j max-j)
    (if (> j max-j)
        lst
        (inner (cons (pascal max-j j) lst) (1+ j) max-j)))
  (iter '() 0 n))

(build-pascal 0) ;; ((1))
(build-pascal 1) ;; ((1 1) (1))
(build-pascal 2) ;; ((1 2 1) (1 1) (1))
(build-pascal 3) ;; ((1 3 3 1) (1 2 1) (1 1) (1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.16
;; Design a prodedure that evolves an iterative exponentiation process
;; that uses successive squaring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sq-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (sq-expt a (* b b) (/ n 2))) 
        (else (sq-expt (* a b) b (- n 1)))))

(sq-expt 1 2 10) ;; 1024


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.17
;; Design a prodedure that multiplies two numbers and is analogous to fast-exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; when b is 1 (* a b) = a
;; when b is even (* a b) = (* (double a) (halve b))
;; when b is odd (* a b) = (+ a (* a (1- b)))
(define (fast-mult a b)
  (cond ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (1- b))))))

(fast-mult 3 4) ;; 12
(fast-mult 5 5) ;; 25
(fast-mult 8 8) ;; 64
(fast-mult 4 3) ;; 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.18
;; Using the results of 1.16 and 1.18, devise a procedure that
;; multiples two integers iteratively in term of adding, doubling and halving
;; and uses a logarithmic number of steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-mult-iter x y)
  (define (inner a b c)
    (cond ((= b 0) c)
          ((even? b) (inner (double a) (halve b) c))
          (else (inner a (1- b) (+ c a)))))
  (inner x y 0))

(fast-mult-iter 3 4) ;; 12
(fast-mult-iter 5 5) ;; 25
(fast-mult-iter 8 8) ;; 64
(fast-mult-iter 4 3) ;; 12
