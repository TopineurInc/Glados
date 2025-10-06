(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (power base exp)
  (if (eq? exp 0)
      1
      (* base (power base (- exp 1)))))

(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))

(define (lcm a b)
  (div (* a b) (gcd a b)))

(define (sum-range start end)
  (if (> start end)
      0
      (+ start (sum-range (+ start 1) end))))

(define (sum-of-squares n)
  (if (eq? n 0)
      0
      (+ (* n n) (sum-of-squares (- n 1)))))

(define (collatz n)
  (if (eq? n 1)
      0
      (if (eq? (mod n 2) 0)
          (+ 1 (collatz (div n 2)))
          (+ 1 (collatz (+ (* 3 n) 1))))))

(define (tri-recursive a b c)
  (if (eq? a 0)
      (+ b c)
      (tri-recursive (- a 1) (+ b 1) (* c 2))))

(define (nested-calc x y)
  (+ (* (fib x) (power 2 y))
     (gcd (* x y) 24)))

(define (ultimate-test n)
  (+ (factorial 4)
     (* (fib 6) 2)
     (collatz 7)
     (sum-of-squares 3)
     (gcd 48 18)
     n))

(ultimate-test 100)
