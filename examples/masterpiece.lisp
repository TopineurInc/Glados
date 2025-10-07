(define (abs n)
  (if (< n 0)
      (* n (- 0 1))
      n))

(define (sqrt-helper x guess)
  (if (< (abs (- (* guess guess) x)) 1)
      guess
      (sqrt-helper x (div (+ guess (div x guess)) 2))))

(define (sqrt x)
  (sqrt-helper x 1))

(define (divides a b)
  (eq? (mod b a) 0))

(define (is-prime-iter n d)
  (if (> (* d d) n)
      1
      (if (divides d n)
          0
          (is-prime-iter n (+ d 1)))))

(define (is-prime n)
  (if (< n 2)
      0
      (is-prime-iter n 2)))

(define (next-prime n)
  (if (is-prime (+ n 1))
      (+ n 1)
      (next-prime (+ n 1))))

(define (nth-prime-helper count current)
  (if (eq? count 0)
      current
      (nth-prime-helper (- count 1) (next-prime current))))

(define (nth-prime n)
  (if (eq? n 0)
      2
      (nth-prime-helper n 1)))

(define (collatz n)
  (if (eq? n 1)
      1
      (if (eq? (mod n 2) 0)
          (+ 1 (collatz (div n 2)))
          (+ 1 (collatz (+ (* 3 n) 1))))))

(define (sum-primes-helper n sum)
  (if (eq? n 0)
      sum
      (if (is-prime n)
          (sum-primes-helper (- n 1) (+ sum n))
          (sum-primes-helper (- n 1) sum))))

(define (sum-primes n)
  (sum-primes-helper n 0))

(define (tower-of-hanoi n)
  (if (eq? n 1)
      1
      (+ (* 2 (tower-of-hanoi (- n 1))) 1)))

(define (perfect-number-helper n sum d)
  (if (> d (div n 2))
      (eq? sum n)
      (if (divides d n)
          (perfect-number-helper n (+ sum d) (+ d 1))
          (perfect-number-helper n sum (+ d 1)))))

(define (is-perfect n)
  (if (< n 2)
      0
      (perfect-number-helper n 0 1)))

(define (mega-computation n)
  (+ (* (nth-prime 3) (collatz n))
     (tower-of-hanoi 4)))

(mega-computation 7)
