(loop
  (format t "> ")
  (let ((line (string-trim " " (read-line))))
    (cond
      ((member line '("quit" "exit") :test #'string-equal)
       (return))
      ((string= line "")
       (format t ""))
      (t
       (handler-case
           (let* ((parts (split-sequence:split-sequence #\Space line :remove-empty-subseqs t))
                  (a (parse-number:parse-number (first parts)))
                  (op (second parts))
                  (b (parse-number:parse-number (third parts)))
                  (res
                    (cond
                      ((string= op "+") (+ a b))
                      ((string= op "-") (- a b))
                      ((string= op "*") (* a b))
                      ((string= op "/")
                       (if (zerop b)
                           (progn (format t "Erreur : division par zéro~%") nil)
                           (/ a b)))
                      (t (progn (format t "Opérateur inconnu: ~a~%" op) nil)))))
             (when res
               (if (integerp res)
                   (format t "~d~%" res)
                   (format t "~f~%" res))))
         (error (e)
           (format t "Erreur: ~a~%" e)))))))
