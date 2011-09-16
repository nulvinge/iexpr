; Copyright (c) 2008 Niklas Ulvinge
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
;
;------------------------------------------------------------------------------
;
; i-expression tools and command-line utility
;
; i<function> is the same as <function> only that it also handles i-expressions.

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

(define (idisplay l)
  (display (iexpr->string l)))

(define (iexpr->string l)
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
    ;(write (list i l)) (newline)
    (flatten
      (cond ((list? l)   (parse-list l i))
            ((symbol? l) (list (symbol->string l)))
            ((number? l) (list (number->string l)))
            ((null? l)   "'()")
            ((string? l) (list "\"" l "\""))
            ((char? l) (string #\# #\\ l))
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

  ;cons with forced evaluation order, becouse of the side-effects of get
  (define-macro (cans car cdr)
     (let ((car-sym (gensym)))
       `(let ((,car-sym ,car))
          (cons ,car-sym ,cdr))))
  (define (tokenise)
    (define (read-token)
      (define (loop)
        (if (or (null? peek)
                (char-whitespace? peek)
                (equal? #\; peek)
                (equal? lparen peek))
          '()
          (cans (get)
                (loop))))
      (loop))
    (define (read-comment)
      (if (equal? peek #\newline)
        (read-whitespace)
        (begin (get)
               (read-comment))))
    (define (read-whitespace)
      (if (equal? peek #\;) ; ';' IS preceded by whitespace
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
            (cans (get)
                   '())
            (if (equal? peek #\\) ;escaped things in strings
              (cans (get)
                    (cans (get)
                          (loop)))
              (cans (get)
                    (loop))))))
      (cans (get) ;becouse of the first #\"
            (loop)))
    (define (read-escaped)
      (define (loop)
        (if (or (null? peek)
                (char-whitespace? peek))
          '()
          (cans (get)
                (loop))))
      (cans (get) ;remove #
            (if (equal? peek #\\)
              (cans (get)
                    (cans (get)
                          (loop)))
              (loop))))

    (define (read-sexpr)
      (define (loop n-open-parens)
        (cond ((null? peek)         '())
              ((equal? peek #\#)    (append (read-escaped)
                                            (loop n-open-parens)))
              ((equal? peek lparen) (cans (get)
                                          (loop (+ n-open-parens 1))))
              ((equal? peek rparen) (if (= 1 n-open-parens)
                                      (cans (get) ;exit loop
                                            '())
                                      (cans (get)
                                            (loop (- n-open-parens 1)))))
              (else (cans (get)
                          (loop n-open-parens)))))
      (loop 0))

    (define (read-unquote)
      (get) ; ,
      (if (equal? #\@ peek)
        (begin (get) (string->list "unquote-splicing"))
        (begin       (string->list "unquote"))))

    (read-whitespace)
    (cans indent
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
        (if (or (equal? "group" first)
                (equal? "grp" first))
          (if (< i (car peekt))
            (body i)
            "()") ;if group is alone
          (if (< i (car peekt)) ;we have childs
            (cans first
                  (body i))
            first))))
    (define (body i)
      (if (< i (car peekt))
        (cans (head (car peekt))
              (body i))
        '()))
    (body -1))

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
                          read)) ;TODO: handle read-errors more gracefully

(define (iexecute . port)
  (map (lambda(e)
         ;(idisplay e) (newline)
         (let ((ret (eval e)))
           ;(if (not (equal? ret #!void))
           ;  (begin
           ;    (write ret) (newline)))))
           ret
           ))
       (if (null? port)
         (iread)
         (iread (car port)))))

(define (iload filename)
  (iexecute (open-input-file filename))
  filename)

(define (iinteractive)
  (define (read-line)
    (if (eof-object? (peek-char))
      '()
      (if (char=? #\newline (peek-char))
        (list (read-char))
        (cons (read-char)
              (read-line)))))
  (define (read-block)
    (let ((line (list->string (read-line))))
      (if (string=? line "\n")
        (list line)
        (cons line
              (read-block)))))
  (iexecute (open-input-string (apply string-append (read-block))))
  (iinteractive))

;(iexecute (open-input-file "test.ism"))
;(iload "test.ism")
;(iinteractive)

(define (parse-parameters pars)
  (if (not (null? pars))
    (if (string=? (car pars) "load")
      (if (string=? (cadr pars) "-")
        (begin
          (display (list (car (command-line)) ": i-expr interactive enviroment\n"))
          (iinteractive))
        (iload (cadr pars)))
      (begin (display (list "Unrecognized parameter \"" (car pars) "\".\n"))
             (parse-parameters (cdr pars))))))
(parse-parameters (cdr (command-line))) ;first is this
;(parse-parameters (list "load" "test.ism"))

