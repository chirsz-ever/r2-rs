(let ([yc (lambda (y) (let ([h (lambda (x) (y (lambda (n) ((x x) n))))]) (h h)))])
    (let ([f 
    	(lambda (fact) (lambda (n) 
    		(((is_zero n) (lambda (u) 1)) (lambda (u) (* n ((fact (- n 1)) 0))))))])
        (((yc f) 7) 0)))