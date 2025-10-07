(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))

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

(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))

(define (sum-to n)
  (if (eq? n 0)
      0
      (+ n (sum-to (- n 1)))))

(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))

(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))

(+ (+ (+ (+ (+ (+ (+ (factorial 6)
                      (* (fib 8) 2))
                   (power 3 5))
                (gcd 144 60))
             (sum-to 15))
          (* (is-even 100) 10))
       (+ (* 2 3) 1))
   (+ (* 5 10) (* 6 10)))
