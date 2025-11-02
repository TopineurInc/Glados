(define (fib_iter a b count)
  (if (eq? count 0)
      a
      (fib_iter b (+ a b) (- count 1))))

(define (fib n)
  (fib_iter 0 1 n))

(fib 10)
