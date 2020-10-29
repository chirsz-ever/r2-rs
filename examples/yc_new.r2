(define yc (lambda (y)
    (let ([h (lambda (x) (y (lambda (n) ((x x) n))))]) (h h))))

(define fact-helper (lambda (fact) (lambda (n) 
            (((is_zero n)
                (lambda (u) 1))
                (lambda (u) (* n ((fact (- n 1)) 0)))))))

(define factorial (lambda (n) (((yc fact-helper) n) 0)))

(factorial 100)
