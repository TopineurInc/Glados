(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (power base exp)
  (if (eq? exp 0)
      1
      (if (eq? exp 1)
          base
          (* base (power base (- exp 1))))))

(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))

(define (sum-of-squares n)
  (if (eq? n 0)
      0
      (+ (* n n) (sum-of-squares (- n 1)))))

(define (ackermann m n)
  (if (eq? m 0)
      (+ n 1)
      (if (eq? n 0)
          (ackermann (- m 1) 1)
          (ackermann (- m 1) (ackermann m (- n 1))))))

(define (complex-calc n)
  (+ (* (fib n) (power 2 n)) (gcd n 15)))

(complex-calc 5)
