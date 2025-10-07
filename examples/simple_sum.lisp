(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))

(define (double x)
  (* x 2))

(+ (factorial 5) (double 10))
