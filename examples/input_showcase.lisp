; Demande le nom de l'utilisateur
(format t "What is your name? ")
; Lit l'entrée utilisateur et la stocke dans la variable 'name'
(define name (read-line))
; Affiche un message de bienvenue personnalisé
(format t "Hello, ~a! Welcome!~%" name)

; Pose une question à l'utilisateur
(format t "Do you like Lisp? (yes/no) ")
; Lit la réponse de l'utilisateur
(define answer (read-line))
; Affiche un message différent selon la réponse
(if (eq? answer "yes")
    (format t "Great! Lisp is awesome!~%")
    (format t "Maybe you'll like it later!~%"))
