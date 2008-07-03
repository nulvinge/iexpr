(define (flatten l)
  (if (not (list? l))
    (list l)
    (if (null? l)
      '()
      (append (flatten (car l))
              (flatten (cdr l))))))

(define (writer thing)
  (write thing) (newline)
  thing)

(define (iread . port)
  (define lparen #\()
  (define rparen #\))
  (define peek '())
  (define indent -1)
  (define (get)
    (let ((ret peek))
      (if (equal? ret #\newline)
        (set! indent 0)
        (set! indent (+ 1 indent)))
      (if (eof-object? (peek-char port))
        (set! peek '())
        (begin
          (set! peek  (read-char port))))
      ret))

  (define (tokenise)
    (define (read-token)
      (define (loop)
        (if (or (null? peek)
                (char-whitespace? peek)
                (equal? #\; peek)
                (equal? lparen peek))
          '()
          (cons (get)
                (loop))))
      (loop))
    (define (read-comment)
      (if (equal? peek #\newline)
        (read-whitespace)
        (begin (get)
               (read-comment))))
    (define (read-whitespace)
      (if (equal? peek #\;) ; ; IS preceded by whitespace
        (read-comment)
        (if (and (not (null? peek))
                 (char-whitespace? peek))
            (begin (get)
                   (read-whitespace)))))
    (define (read-string)
      (define (loop)
        (if (null? peek)
          '()
          (if (equal? peek #\")
            (cons (get)
                  '())
            (if (equal? peek #\\) ;escaped things in strings
              (cons (get)
                    (cons (get)
                          (loop)))
              (cons (get)
                    (loop))))))
      (cons (get) ;becouse of the first #\"
            (loop)))
    (define (read-escaped)
      (define (loop)
        (if (or (null? peek)
                (char-whitespace? peek))
          '()
          (cons (get)
                (loop))))
      (cons (get) ;remove #
            (if (equal? peek #\\)
              (append (list (get)
                            (get))
                      (loop))
              (loop))))

    (define (read-sexpr)
      (define (loop n-open-parens)
        (cond ((null? peek)         '())
              ((equal? peek #\#)    (append (read-escaped)
                                            (loop n-open-parens)))
              ((equal? peek lparen) (cons (get)
                                          (loop (+ n-open-parens 1))))
              ((equal? peek rparen) (if (= 1 n-open-parens)
                                      (cons (get) ;exit loop
                                            '())
                                      (cons (get)
                                            (loop (- n-open-parens 1)))))
              (else (cons (get)
                          (loop n-open-parens)))))
      (loop 0))

    (define (read-unquote)
      (get) ; ,
      (if (equal? #\@ peek)
        (begin (get) (string->list "unquote-splicing"))
        (begin       (string->list "unquote"))))

    (read-whitespace)
    (cons indent
          (list->string
            (cond ((equal? peek lparen) (read-sexpr))
                  ((equal? peek #\")    (read-string))
                  ((equal? peek #\#)    (read-escaped))
                  ((equal? peek #\')    (get) (string->list "quote"))
                  ((equal? peek #\`)    (get) (string->list "quasiquote"))
                  ((equal? peek #\,)    (read-unquote))
                  (else                 (read-token))))))

  (define peekt '())
  (define (gett)
    (let ((ret peekt))
      (set! peekt (tokenise))
      (if (equal? (cdr peekt) "")
        (set! peekt '(-1 . ())))
      ret))
 
  (define (tokens->list)
    (define (head i)
      (let ((first (cdr (gett))))
        (if (equal? "group" first)
          (body (car peekt))
          (if (< i (car peekt)) ;we have childs
            (append (list first)
                    (body (car peekt)))
            first))))
    (define (body i)
      (if (<= i (car peekt))
        (cons (head i)
              (body i))
        '()))
    (body 0))

  (define (to-string l)
    (define (loop l)
      (if (list? l)
        (append (list "(")
                (map loop l)
                (list ") "))
        (list l " ")))
    (apply string-append (flatten (loop l))))

  (if (null? port)
    (set! port (current-input-port))
    (set! port (car port)))

  ;do some initing
  (get)
  (gett)
  ;and start working
  (call-with-input-string (to-string (tokens->list))
                          read))

(define input1 "
define fac x
       if = x
            0
          1
          * x
            fac - x
                  1
fac 4
")
(define input2 "
z a
  b c
  (+ 1 d)
  group e f
        g ;test
        group h
  i
  \"a \\\"string\\\"\"
  quote j
")


(define fac '(define (fac x)
               (if (= x 0)
                 1
                 (* x
                    (fac (- x 1))))))

(define (iexecute . port)
  (map (lambda(e)
         ;(write e) (newline)
         (let ((ret (eval e)))
           (if (not (equal? ret #!void))
             (begin
               (write ret) (newline)))))
       (if (null? port)
         (iread)
         (iread (car port)))))

;(display input1) (newline)
;(iexecute (open-input-string input1))
;(iexecute (open-input-file "test.ism"))


(define (iinteractive)
  (define (read-line)
    (if (char=? #\newline (peek-char))
      (list (read-char))
      (cons (read-char)
            (read-line))))
  (define (read-block)
    (let ((line (list->string (read-line))))
      (if (string=? line "\n")
        (list line)
        (cons line
              (read-block)))))
  (define (loop)
    (iexecute (open-input-string (apply string-append (read-block))))
    (loop))
  (loop))

(iinteractive)
