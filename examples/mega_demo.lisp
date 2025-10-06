(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))

(define (power base exp)
  (if (eq? exp 0)
      1
      (* base (power base (- exp 1)))))

(define (sum-to n)
  (if (eq? n 0)
      0
      (+ n (sum-to (- n 1)))))

(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))

(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))

(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))

(define (collatz-length n)
  (if (eq? n 1)
      0
      (if (eq? (mod n 2) 0)
          (+ 1 (collatz-length (div n 2)))
          (+ 1 (collatz-length (+ (* 3 n) 1))))))

(define (ackermann m n)
  (if (eq? m 0)
      (+ n 1)
      (if (eq? n 0)
          (ackermann (- m 1) 1)
          (ackermann (- m 1) (ackermann m (- n 1))))))

(define (sum-of-squares n)
  (if (eq? n 0)
      0
      (+ (* n n) (sum-of-squares (- n 1)))))

(define (tower-hanoi n)
  (if (eq? n 1)
      1
      (+ 1 (* 2 (tower-hanoi (- n 1))))))

(define (tri-number n)
  (if (eq? n 0)
      0
      (+ n (tri-number (- n 1)))))

(define (deep-calc n)
  (if (eq? n 0)
      1
      (+ (* 2 n) (deep-calc (- n 1)))))

(+ (factorial 6)
   (power 3 4)
   (gcd 100 45)
   (sum-to 20)
   (* (is-even 42) 10)
   (collatz-length 27)
   (sum-of-squares 5)
   (tower-hanoi 5)
   (tri-number 7)
   (deep-calc 10))
