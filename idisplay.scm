(define (make-loop combiner item the-list)
  (define (loop l)
    (if (null? l)
      '()
      (combiner (item (car l))
                (loop (cdr l)))))
  (loop the-list))

(define (flatten l)
  (if (list? l)
    (make-loop append
               flatten
               l)
    (list l)))

(define (substring? s i)
  (define (loop n)
    (if (zero? n)
      #f
      (if (eq? i (string-ref s (- n 1)))
        #t
        (loop (- n 1)))))
  (loop (string-length s)))

(define (idisplay l)
  (define tab #\space)
  (define space " ")

  (define (indent i)
    (make-string i tab))

  (define (len l)
    (if (null? l)
      0
      (+ (string-length (car l))
         (len (cdr l)))))
  (define (contains-newline? l)
    (if (null? l)
      #f
      (if (equal? (car l) "\n")
        #t
        (contains-newline? (cdr l)))))
  (define (loop l i)
    (write (list i l)) (newline)
    (cond ((list? l) (if (null? (cdr l))
                       (append (list "group ")
                               (loop (car l) (+ 1 i)))
                       (let ((func (loop (car l) (+ 1 i))))
                         (append func
                                 (list " ")
                                 (loop (cadr l) (+ (len func) 1 i))
                                 (flatten (map (lambda(k)
                                                 (list "\n"
                                                       (indent i)
                                                       (loop k (+ 2 i))))
                                               (cddr l)))))))
          ((symbol? l) (list (symbol->string l)))
          ((number? l) (list (number->string l)))
          (else "undef")))

  (define (loop2 l i)
    (cond ((list? l) (flatten
                       (let ((first (loop2 (car l) (+ 2 i))))
                         (if (null? (cdr l))
                           (append (list (indent i) "group ")
                                   first (list "\n"))
                           (append first
                                   (list (indent i))
                                   (loop2 (cadr l) (+ 2 i))
                                   (list "\n")
                                   (map (lambda(k)
                                          (append (list (indent i))
                                                  (loop2 k (+ 2 i))
                                                  (list "\n")))
                                        (cddr l)))))))
          ((symbol? l) (list (symbol->string l)))
          ((number? l) (list (number->string l)))
          (else "undef")))

  (define (parse l func)
    (cond ((list? l)   (func l))
          ((symbol? l) (list (symbol->string l)))
          ((number? l) (list (number->string l)))
          ((null? l)   "()")
          ((string? l) l)
          ((pair? l) (list "(" (parse (car l) func) " . " (parse (cdr l) func) ")"))
          (else "undef")))

  (define (loop3 l i i2)
    (parse l (lambda(l)
               (flatten
                 (let* ((first  (loop3 (car l) i i2))
                        (len    (len first))
                        (newlen (if (contains-newline? first) 0 1))
                        (i      (if (not (zero? newlen))
                                  i
                                  (- 0 len 1))))
                   (write (list i len newlen))
                   (write l) (newline)
                   (list (if (null? (cdr l))
                           (list "group " first)
                           (list first
                                 (indent newlen)
                                 (loop3 (cadr l) (+ i len 1) 0)
                                 (map (lambda(k)
                                        (list "\n"
                                              (indent (+ i len 1))
                                              (loop3 k (+ i len 1) 0)))
                                      (cddr l))))
                         "\n"))))))

  (define (make-single l)
    (define (remove-multiple-newlines l)
      (if (null? (cdr l))
        '()
        (if (equal? (car l) "\n")
          (if (equal? (cadr l) "\n")
            (remove-multiple-newlines (cdr l))
            (cons "\n"
                  (remove-multiple-newlines (cdr l))))
          (cons (car l)
                (remove-multiple-newlines (cdr l))))))
    (apply string-append (remove-multiple-newlines l)))
  (make-single (loop3 l 0 0)))



;(newline)
;(idisplay '(1 2 3 (4 5))) (newline)

(define fac '(define (fac x)
               (if (= x 0)
                 1
                 (* x
                    (fac (- x 1))))))
(define tst1 '(let ((foo (+ 1 2))
                   (bar (+ 3 4)))
               (+ foo bar)))
              
(define tst2 '(a b (c)))

(define tst3 '((foo a)
               (bar (+ 3 4))))
(define tst4 '(foo (a (bar (+ 1 2)))))
(define tst5 '(z a (b c) (d) ((e f) g (h)) i 'j))

;z a
;  b c
;  group d
;  group e f
;        g
;        group h
;  i
;  qoute j

; " Z
;    A
;    B
;      C
;    (D)
;    GROUP
;      E
;        F
;      G
;      (H)
;    I


(let ((l (idisplay tst5)))
  (newline)
  (write l) (newline)(newline)
  (display l) (newline)
  )
;  define
;   fac x
;   if
;    = x 0
;    1
;    * x
;      fac
;       - x 1
;  let
;   group
;    foo
;     + 1 2
;    bar
;     + 3 4
;   + foo bar
;
;let foo + 1
;          2
;    bar + 3
;          4
;  + foo bar
