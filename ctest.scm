(define (a)
  (define (b)
    (define consa cons)
    (define (c)
      (write '_)
      (consa (begin
              (write 'c)
              (undef))
            (c)))
    (c))
  (b))
(a)
