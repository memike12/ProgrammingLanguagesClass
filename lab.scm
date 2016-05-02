;;; SI 413 Lab 1
;;; MIDN Moore and Midn LIU

;;; Exercise 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ex1 (+ (* 4.7 (- 34.453 47.728)) 3.7))

;;; Exercise 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ex2 (max (sqrt 5) (+ (sin 1) (sin 2) (sin 3)) (/ 31 13)))
;; when you put (/31 13) into the interpreter, it displays 
;; 31/13 but max converts it to an inexact number

;;; Exercise 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x 2.451)
(define ex3 (- (+ (- (* (expt x 3) 2) (expt x 2)) (* 3 x)) 5))

;;; Exercise 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define root2 (sqrt 2))

;;; Exercise 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (to-celsius x)
  (* (/ 5 9 ) 
     (- x 32)))
(define (to-fahrenheit x)
  (+ (* x 
        (/ 9 5)) 
     32))

;;; Exercise 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-trig x)
  (+ (expt (sin x) 2) 
     (* x (cos x))))

;;; Exercise 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (signed-inc x)
  (if (> x 0) (+ x 1) (+ -1 x)))

;;; Exercise 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (signed-inc-better x)
  (if (= x 0) 
      x 
      (if (> x 0) 
          (+ x 1) 
          (+ -1 x))))

;;; Exercise 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (middle x y z)
  (if (= x (max x y z))
      (if (= y (min x y z))
          z
          y)
      (if (= x (min x y z))
          (if (= y (max x y z))
              z
              y)
          x)))

;;; Exercise 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (middle-better x y z)
  (cond ((and (> x y) (> y z)) y)
        ((and (> x z) (> z y)) z)
        (else x)))

;;; Exercise 11 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (factorial x)
  (if (= x 1)
      x
      (* x (factorial (- x 1)))))
;; the java implementation would give you a number 
;;to the power of 10 and you would lose precision

;;; Exercise 12 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (accrue B r y)
  (if (= y 0)
      B
      (exact->inexact (accrue (accrue-months B r 12) r (- y 1)))))

(define (compound-month B r)
  (* B (+ 1 (/ r 1200))))

(define (accrue-months B r m)
  (if (= m 0)
      B
      (accrue-months (compound-month B r) r (- m 1))))

;;; Exercise 13 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fib x)
  (if (= x 1)
      1
      (if (= x 2)
          1
          (+ (fib (- x 2)) (fib (- x 1))))))

;;; Exercise 14 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (split-inches i)
  (cons (floor (/ i 12)) (modulo i 12)))

;;; Exercise 15 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (shorter? J K)
  (if (< (car J) (car K))
      #t
      (if (and (= (car J) (car K)) (< (cdr J) (cdr K)))
          #t
          #f)
      ))