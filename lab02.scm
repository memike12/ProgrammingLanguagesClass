;;; SI 413 Lab 2
;;; MIDN Moore and Midn Demass

;;; Exercise 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (to-usdollar amt cur)
  (if (number? amt)
      (if (symbol? cur)
          (cond 
            ((eqv? cur 'yen) (/ amt 98.18))
            ((eqv? cur 'euro) (/ amt .76))
            ((eqv? cur 'won) (/ amt 1109.85))
            ((eqv? cur 'usd) amt)
            (else '(please use 'yen 'euro 'won or 'usd)))
          '(please use 'yen 'euro 'won or 'usd))
      '(please provide a number for amt)))


;;; Exercise 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (from-usdollar amt cur)
  (if (number? amt)
      (if (symbol? cur)  
          (cond ( (eqv? cur 'euro) (* amt .76))
                ( (eqv? cur 'yen)  (* amt 98.18))
                ( (eqv? cur 'won)  (* amt 1109.85))
                ( (eqv? cur 'usd) amt)
                (else -1))
          -1)
      -2)
  )

;;; Exercise 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (convert amt fromCur toCur)
  (cond
    ((not(number? amt)) '(please provide a number for amt))
    ((not(positive? amt)) '(enter a positive amt))
    ((not(symbol? fromCur)) '(please use 'yen 'euro 'won or 'usd for first currency))
    ((not(symbol? toCur)) '(please use 'yen 'euro 'won or 'usd for second currency))
    (else (from-usdollar (to-usdollar amt fromCur) toCur))))

;;; Exercise 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exercise 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (longer? L1 L2)
  (if (and (not (null? L1)) (not (null? L2)))
      (longer? (cdr L1) (cdr L2))
      (if (null? L1) #f #t)))

;;; Exercise 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Exercise 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define myList '(34 18 25 23 29 11 28 24 27 29))

(define (std-dev L)
  (/ (numerator L) (- (length L) 1)))

(define (numerator L)
  (apply + (square (subavg L (average L)))))

(define (average L)
  (/ (apply + L) (length L)))

(define (subavg L avg)
  (if (null? L)
      '()
      (cons (- (car L) avg)
            (subavg (cdr L) avg))))

(define (square L)
  (if (null? L)
      '()
      (cons (* (car L) (car L))
            (square (cdr L)))))

;;; Exercise 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exercise 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-sin y)
  (let ((x (+ (sq (sin y)) 1)))
    (+ (/ 1 x) (sqrt x) (sq x))))

(define (sq x)
  (* x x))

;;; Exercise 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exercise 11 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fd-at g n)
  (- (g (+ n 1)) (g n)))

;;; Exercise 12 ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Exercise 13 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (special-nums x)
  (if (= x 0)
      '()
      (if (eqv? 0 (modulo x 3))
          (if (eqv? (round (sqrt x)) (sqrt x))
              (cons x (special-nums (- x 1)))
              (special-nums (- x 1)))
           (special-nums (- x 1)))))