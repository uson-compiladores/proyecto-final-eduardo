((rec fact
   (lambda (n)
     (if (zero? n)
         1
         (* n (fact (- n 1))))))
 5)
