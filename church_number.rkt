
;See Also：
;SICP exercise 2.6
;http://en.wikipedia.;org/wiki/Church_encoding#Church_numerals
;http://minus273.eu/mirrors/2001315450/wiki/ChurchNumber.html

(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (inc n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (add m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))

(define (add2 m n)
  ((m inc) n))

(define (mul m n)
  (lambda (f)
    (lambda (x) ((m (n f)) x))))

(define (dec n)
  (lambda (f)
    (lambda (x)
      (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))

(define (sub m n)
  ((n dec) m))

(define (iszero n) ((n (lambda (x) false)) true))

(define (f x) (cons 'x x))
(define (print n) ((n f) '()))
