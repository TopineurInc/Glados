(define (reverse-loop s idx acc)
  (if (< idx 0)
      acc
      (reverse-loop s
                    (- idx 1)
                    (string-append acc (substring s idx (+ idx 1))))))

(define (string-reverse s)
  (reverse-loop s (- (string-length s) 1) ""))

(define (is-palindrome-string s)
  (eq? s (string-reverse s)))

(define (is-palindrome-number n)
  (is-palindrome-string (number->string n)))

(define (safe-largest count value)
  (if (eq? count 0) 0 value))

(define (safe-smallest count value)
  (if (eq? count 0) 0 value))

(define (average-of sum count)
  (if (eq? count 0) 0 (div sum count)))

(define (update-largest current largest)
  (if (or (eq? largest -1) (> current largest))
      current
      largest))

(define (update-smallest current smallest)
  (if (or (eq? smallest -1) (< current smallest))
      current
      smallest))

(define (should-display-average count average)
  (and (not (eq? count 0))
       (> average 0)))

(define (display-report start end count smallest largest sum average)
  (begin
    (print (string-append
            (string-append "Analyse des palindromes entre " (number->string start))
            (string-append " et " (number->string end))))
    (cond
      ((eq? count 0)
       (print "Aucun palindrome trouve dans cet intervalle."))
      (else
       (begin
         (print (string-append "Total detecte: " (number->string count)))
         (print (string-append "Plus petit palindrome: " (number->string smallest)))
         (print (string-append "Plus grand palindrome: " (number->string largest)))
         (print (string-append "Somme cumulee: " (number->string sum)))
         (when (should-display-average count average)
           (print (string-append "Moyenne (entier): " (number->string average)))))))
    0))

(define (palindrome-loop start end current sum count largest smallest)
  (if (> current end)
      (begin
        (display-report start
                        end
                        count
                        (safe-smallest count smallest)
                        (safe-largest count largest)
                        sum
                        (average-of sum count))
        (+ sum
           (+ (average-of sum count)
              (+ (* 2 (safe-largest count largest))
                 (* 3 (safe-smallest count smallest))))))
      (if (is-palindrome-number current)
          (palindrome-loop start
                           end
                           (+ current 1)
                           (+ sum current)
                           (+ count 1)
                           (update-largest current largest)
                           (update-smallest current smallest))
          (palindrome-loop start
                           end
                           (+ current 1)
                           sum
                           count
                           largest
                           smallest))))

(define (palindrome-analysis start end)
  (palindrome-loop start end start 0 0 -1 -1))

(palindrome-analysis 100 999)
