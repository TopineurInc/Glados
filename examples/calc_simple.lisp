(define (calc-add a b) (+ a b))
(define (calc-sub a b) (- a b))
(define (calc-mul a b) (* a b))
(define (calc-div a b)
  (if (eq? b 0)
      (begin
        (print "Error: division by zero")
        0)
      (div a b)))

(define (calculate a op b)
  (if (eq? op 1)
      (calc-add a b)
      (if (eq? op 2)
          (calc-sub a b)
          (if (eq? op 3)
              (calc-mul a b)
              (if (eq? op 4)
                  (calc-div a b)
                  (begin
                    (print "Unknown operator")
                    0))))))

(define result1 (calculate 3 1 4))
(print result1)

(define result2 (calculate 10 2 5))
(print result2)

(define result3 (calculate 6 3 7))
(print result3)

(define result4 (calculate 20 4 4))
(print result4)

(define result5 (calculate 10 4 0))

result4
