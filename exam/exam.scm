;;; SI 413 Practicum
;;; Michael Moore 164488

(define (sqr x) (* x x))

;;; Problem 1 ;;;;;;;;;;;;;;;;;;;;
(define (root-info L)
  (let ((descriminant (- (sqr (car (cdr L))) (* 4 (car L) (car (cdr (cdr L)))))))
    (if (negative? descriminant)
        (display 0)
        (if (positive? descriminant) 
            (display 2)
            (display 1)))))

;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;
(define (fold A B)
  (if (null? A)
      (if (null? B)
          '()
          (cons (car B) (fold A (cdr B))))
      (if (null? B)
          (cons (car A) (fold (cdr A) B))
          (cons (car A) (cons (car B) (fold (cdr A) (cdr B)))))))

;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;
(define (total-length LoL)
  (apply + (map (lambda (L) (length L)) LoL)))


;;; Problem 4 ;;;;;;;;;;;;;;;;;;;;
(define (mkpiece f g x0)
  (lambda (x) (if (<= x x0) 
                  (f x)
                  (g x))))


;;; Problem 5 ;;;;;;;;;;;;;;;;;;;;
(define (sumpos a . b)
  (sumpos-helper (cons a b) 0))

(define (sumpos-helper a accum)
  (if (null? a)
      accum
      (sumpos-helper (cdr a) (+ (if (positive? (car a))
                                    (car a)
                                    0) accum))))

