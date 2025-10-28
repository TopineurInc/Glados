(define (fibiter a b count)
  (if (eq? count 0)
      a
      (fibiter b (+ a b) (- count 1))))

(define (fib n)
  (fibiter 0 1 n))

(fib 5)
