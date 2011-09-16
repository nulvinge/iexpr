;(load "iexpr.scm")

(define (parse-parameters pars)
  (if (or (null? pars)
          (string=? (car pars) "-"))
    (iinteractive)
    (iload (car pars))))
(parse-parameters (cdr (command-line))) ;first is this

