#lang racket

; Little Schemer Chapter 8
(define mrco
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (mrco a (cdr lat) (lambda (newlat seen)
                           (col newlat (cons (car lat) seen)))))
      (else (mrco a (cdr lat) (lambda (newlat seen)
                                (col (cons (car lat) newlat) seen)))))))

(define af
  (lambda (x y) (null? y)))

(mrco 'a '() af)
(mrco 'a '(a) af)
(mrco 'a '(b c d) af)
(mrco 'a '(b a c d) af)

(define even-only
  (lambda (l)
    (cond
      ((null? l) '())
      ((integer? (car l))
       (if (= (remainder (car l) 2) 0)
           (cons (car l) (even-only (cdr l)))
           (even-only (cdr l))))
      (else (cons (even-only (car l)) (even-only (cdr l)))))))

(even-only (list (list 9 1 2 8) 3 10 (list (list 9 9) 7 6) 2))

(define eoco
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((integer? (car l))
       (if (= (remainder (car l) 2) 0)
           (eoco (cdr l) (lambda (newl p s)
                           (col (cons (car l) newl) (* (car l) p) s)))
           (eoco (cdr l) (lambda (newl p s)
                           (col newl p (+ (car l) s))))))
      (else (eoco (car l) (lambda (newl p s)
                            (eoco (cdr l) (lambda (newl2 p2 s2)
                                            (col
                                             (cons newl newl2)
                                             (* p p2)
                                             (+ s s2))))))))))
                           

(eoco
 (list (list 9 1 2 8) 3 10 (list (list 9 9) 7 6) 2)
 (lambda (l p s) (cons s (cons p l))))
      
       