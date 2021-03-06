(define yc (lambda (y)
    (let ([h (lambda (x) (y (lambda (n) ((x x) n))))]) (h h))))

(define fact-helper
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1)))))))

(define factorial (lambda (n) ((yc fact-helper) n)))

(factorial 100)
