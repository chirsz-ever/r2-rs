(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (let ([x 4])
       (f 3))))