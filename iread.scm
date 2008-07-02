(define (iread s)
  (define peek '())
  (define input '())
  (define indent 0)
  (define (get)
    (let ((ret peek))
      (if (equal? ret #\newline)
        (set! indent 0)
        (set! indent (+ 1 indent)))
      (if (null? (cdr input))
        (set! peek '())
        (begin
          (set! input (cdr input))
          (set! peek  (car input))))
      ret))

  (define (tokenise)
    (define (read-token)
      (define (loop)
        (if (or (null? peek)
                (char-whitespace? peek))
          '()
          (cons (get)
                (loop))))
      (let ((i indent)
            (token (loop)))
        (cons (list->string token)
              i)))
    (define (read-whitespace)
      (if (or (null? peek)
              (not (char-whitespace? peek)))
        '()
        (begin (get)
               (read-whitespace))))

    (read-whitespace)
    (read-token))
  (define peekt '())
  (define (gett)
    (write peekt) (newline)
    (let ((ret peekt))
      (set! peekt (tokenise))
      (if (equal? (car peekt) "")
        (set! peekt '(() . 0)))
      ret))
 
  (define (tokens->list i)
    (define (head i)
      (let ((first (car (gett))))
        (if (equal? "group" first)
          (body (cdr peekt))
          (if (< i (cdr peekt)) ;we have childs
            (append (list first)
                    (body (cdr peekt)))
            first))))
    (define (body i)
      (if (= i (cdr peekt))
        (cons (head i)
              (body i))
        '()))
          
    (write (list 'a i))
    (head i))

  (set! input (string->list s))
  (set! peek (car input))
  (gett)
  (tokens->list 0))


;(define input "define fac x\n       if = x\n            0\n          1\n          * x\n            fac - x\n                  1")
(define input1 "
define fac x
       if = x
            0
          1
          * x
            fac - x
                  1
")
(define input2 "
z a
  b c
  (d)
  group e f
        g
        (h)
  i
  quote j
")


(define fac '(define (fac x)
               (if (= x 0)
                 1
                 (* x
                    (fac (- x 1))))))

(display input2) (newline)
(write (iread input2)) (newline)
