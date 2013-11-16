#lang racket
(require racket/mpair)

; tree recursion version
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

; iteration version
(define (iter-fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        b
        (fib-iter (+ a b) a (- n 1))))
  (fib-iter 1 0 n))

; log iteration version
(define (log-fib n)
  (define (matrix-iter a b c d p q n)
    (cond ((= n 0) q)
          ((= (remainder n 2) 0)
           (let ((x (* b c))
                 (y (+ a d)))
             (matrix-iter
              (+ (* a a) x)
              (* b y)
              (* c y)
              (+ (* d d) x)
              p q (/ n 2))))
          (else
           (let ((x (* b c))
                 (y (+ a d)))
             (matrix-iter
              (+ (* a a) x)
              (* b y)
              (* c y)
              (+ (* d d) x)
              (+ (* p a) (* q b))
              (+ (* p c) (* q d))
              (quotient n 2))))))
  (matrix-iter 1 1 1 0 1 0 n))
       
; memo version
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key)
      (let ((record (assoc key (mcdr local-table))))
        (if record
            (begin ;(display 'find)
                   ;(display (mcar record))
                   ;(newline)
                   (mcdr record))
            false)))
    (define (insert! key value)
      (let ((record (assoc key (mcdr local-table))))
        (if record
            (begin ;(display 'replace)
                   ;(display (mcar record))
                   ;(newline)
                   (set-mcdr! record value))
            (begin ;(display 'insert)
                   ;(display key)
                   ;(newline)
                   (set-mcdr! local-table
                       (mcons (mcons key value) (mcdr local-table))))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
   
(define count 0)
(define (memoize f)
  (let ((table (make-table)))
    (lambda (n)
      (or ((table 'lookup-proc) n)
          (let ((result (f n)))
            ((table 'insert-proc!) n result)
            result)))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(display 'log=fib) (newline)
(log-fib 100000)
(display 'iter=fib) (newline)
(iter-fib 100000)
(display 'memo=fib) (newline)
(memo-fib 2000)
(display 'fib) (newline)
(fib 30)