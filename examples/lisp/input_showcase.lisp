(format t "What is your name? ")
(define name (read-line))
(format t "Hello, ~a! Welcome!~%" name)

(format t "Do you like Lisp? (yes/no) ")
(define answer (read-line))
(if (eq? answer "yes")
    (format t "Great! Lisp is awesome!~%")
    (format t "Maybe you'll like it later!~%"))
