(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))

(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))

(define (double x)
  (* x 2))

(define (triple x)
  (* x 3))

(define (apply-twice f x)
  (f (f x)))

(define (compose-result x)
  (+ (apply-twice double x)
     (apply-twice triple x)))

(define (hofstadter-f n)
  (if (eq? n 0)
      1
      (- n (hofstadter-m (hofstadter-f (- n 1))))))

(define (hofstadter-m n)
  (if (eq? n 0)
      0
      (- n (hofstadter-f (hofstadter-m (- n 1))))))

(define (deep-recursion a b c)
  (if (eq? a 0)
      (+ b c)
      (if (< b 10)
          (deep-recursion (- a 1) (+ b 2) (* c 2))
          (deep-recursion (- a 1) (- b 5) (+ c 1)))))

(define (binary-ops n)
  (+ (* (is-even n) 100)
     (* (is-odd n) 50)
     (deep-recursion 3 2 1)))

(binary-ops 42)
