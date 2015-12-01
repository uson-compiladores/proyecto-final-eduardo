(begin
  (set! f (rec fact
            (lambda (n)
              (if (zero? n)
                  1
                  (* n (fact (- n 1)))))))
  (cons (f 0)
        (cons (f 1)
              (cons (f 2)
                    (cons (f 3)
                          (cons (f 4)
                                (cons (f 5) '())))))))
