;; Interactive Calculator in Lisp
;; Demonstrates I/O built-in functions

;; Calculator operations
(define (calc a op b)
  (if (eq? op "+")
      (+ a b)
      (if (eq? op "-")
          (- a b)
          (if (eq? op "*")
              (* a b)
              (if (eq? op "/")
                  (if (eq? b 0)
                      (begin
                        (display "Error: division by zero\n")
                        0)
                      (div a b))
                  (begin
                    (display "Unknown operator: ")
                    (display op)
                    (display "\n")
                    0))))))

;; Example: Read two numbers and an operator, then calculate
(define (calculator-demo)
  (begin
    (display "Enter first number: ")
    (define input1 (read-line))
    (define a (string->number input1))

    (display "Enter operator (+, -, *, /): ")
    (define op (read-line))

    (display "Enter second number: ")
    (define input2 (read-line))
    (define b (string->number input2))

    (define result (calc a op b))
    (display "Result: ")
    (display (number->string result))
    (display "\n")
    result))

;; Simple non-interactive example
(define (simple-calc-example)
  (begin
    (display "Calculator Examples:\n")
    (display "5 + 3 = ")
    (print (calc 5 "+" 3))

    (display "10 - 4 = ")
    (print (calc 10 "-" 4))

    (display "6 * 7 = ")
    (print (calc 6 "*" 7))

    (display "20 / 4 = ")
    (print (calc 20 "/" 4))

    (display "10 / 0 = ")
    (calc 10 "/" 0)))

;; Run examples
(simple-calc-example)
