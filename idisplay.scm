(define (flatten l)
  (if (not (list? l))
    (list l)
    (if (null? l)
      '()
      (append (flatten (car l))
              (flatten (cdr l))))))

(define (idisplay l)
  (define group "group ")

  (define (indent i)
    (make-string i #\space))

  (define (len l)
    (if (null? l)
      0
      (+ (string-length (car l))
         (len (cdr l)))))

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

  (define (parse l i)
    (write (list i l)) (newline)
    (flatten
      (cond ((list? l)   (parse-list l i))
            ((symbol? l) (list (symbol->string l)))
            ((number? l) (list (number->string l)))
            ((null? l)   "'()")
            ((string? l) (list "\"" l "\""))
            ((pair? l) (list "(" (parse (car l)) " . " (parse (cdr l)) ")"))
            (else "undef"))))

  (define (parse-list l i)
      (if (list? (car l))
        (list group
              (parse (car l) (+ i (string-length group)))
              (map (lambda(k)
                     (list "\n"
                           (indent  (+ i (string-length group)))
                           (parse k (+ i (string-length group)))))
                   (cdr l)))
        (let* ((first  (parse (car l) i))
               (len    (len first)))
          (list (if (null? (cdr l))
                  (list group first)
                  (list first
                        (indent 1)
                        (parse (cadr l) (+ i len 1))
                        (map (lambda(k)
                               (list "\n"
                                     (indent  (+ i len 1))
                                     (parse k (+ i len 1))))
                             (cddr l))))
                "\n"))))

  (make-single (parse l 0)))

(define fac '(define (fac x y)
               (if (= x 0)
                 1
                 (* x
                    (fac (- x 1))))))
(define tst1 '(let ((foo (+ 1 2))
                   (bar (+ 3 4)))
               (+ foo bar)))
              
(define tst2 '(a b (c)))

(define tst3 '(let ((foo (+ 1 2))
                    (bar (+ 3 4)))
                (+ foo bar)))
(define tst4 '(foo (a (bar (+ 1 2)))))
(define tst5 '(z a (b c) (d) ((e f) g (h)) i 'j))

;z a
;  b c
;  (d)
;  group e f
;        g
;        (h)
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


(let ((l (idisplay fac )))
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
