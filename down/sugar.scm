;  (define-module (sugar))

  (define group 'group)

  (define sugar-read-save read)
;  (define sugar-load-save primitive-load)

  (define (readquote level port qt)
    (read-char port)
    (let ((char (peek-char port)))
      (if (or (eq? char #\space)
	      (eq? char #\newline)
	      (eq? char #\t))
	  (list qt)
	  (list qt (sugar-read-save port)))))

  (define (readitem level port)
    (let ((char (peek-char port)))
      (cond
       ((eq? char #\`)
	(readquote level port 'quasiquote))
       ((eq? char #\')
	(readquote level port 'quote))
       ((eq? char #\,)
	(readquote level port 'unquote))
       (#t
	(sugar-read-save port)))))

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
	  (len2 (string-length indentation2)))
      (and (> len1 len2)
	   (string=? indentation2 (substring indentation1 0 len2)))))

  (define (indentationlevel port)
    (define (indentationlevel)
      (if (or (eq? (peek-char port) #\space)
	      (eq? (peek-char port) #\t))
	  (cons 
	   (read-char port)
	   (indentationlevel))
	  '()))
    (list->string (indentationlevel)))

  (define (clean line)
    (cond
     ((not (pair? line))
      line)
     ((null? line)
      line)
     ((eq? (car line) 'group)
      (cdr line))
     ((null? (car line))
      (cdr line))
     ((list? (car line))
      (if (or (equal? (car line) '(quote))
	      (equal? (car line) '(quasiquote))
	      (equal? (car line) '(unquote)))
	  (if (and (list? (cdr line))
		   (= (length (cdr line)) 1))
	      (cons
	       (car (car line))
	       (cdr line))
	      (list
	       (car (car line))
	       (cdr line)))
	  (cons
	   (clean (car line))
	   (cdr line))))
     (#t
      line)))

  ;; Reads all subblocks of a block
  (define (readblocks level port)
    (let* ((read (readblock-clean level port))
	   (next-level (car read))
	   (block (cdr read)))
      (if (string=? next-level level)
	  (let* ((reads (readblocks level port))
		 (next-next-level (car reads))
		 (next-blocks (cdr reads)))
	    (if (eq? block (string->symbol "."))
		(if (pair? next-blocks)
		    (cons next-next-level (car next-blocks))
		    (cons next-next-level next-blocks))
		(cons next-next-level (cons block next-blocks))))
	  (cons next-level (list block)))))

  ;; Read one block of input
  (define (readblock level port)
    (let ((char (peek-char port)))
      (cond
       ((eof-object? char)
	(cons -1 char))
       ((eq? char #\newline)
	(read-char port)
	(let ((next-level (indentationlevel port)))
	  (if (indentation>? next-level level)
	      (readblocks next-level port)
	      (cons next-level '()))))
       ((or (eq? char #\space)
	    (eq? char #\t))
	(read-char port)
	(readblock level port))
       (#t
	(let* ((first (readitem level port))
	       (rest (readblock level port))
	       (level (car rest))
	       (block (cdr rest)))
	  (if (eq? first (string->symbol "."))
	      (if (pair? block)
		  (cons level (car block))
		  rest)
	      (cons level (cons first block))))))))

  ;; reads a block and handles group, (quote), (unquote) and
  ;; (quasiquote).
  (define (readblock-clean level port)
    (let* ((read (readblock level port))
	   (next-level (car read))
	   (block (cdr read)))
      (if (or (not (list? block)) (> (length block) 1))
	  (cons next-level (clean block))
	  (if (= (length block) 1)
	      (cons next-level (car block))
	      (cons next-level (string->symbol "."))))))

  (define (sugar-read . port)
    (let* ((read (readblock-clean "" (if (null? port)
					(current-input-port)
					(car port))))
	   (level (car read))
	   (block (cdr read)))
      (cond
       ((eq? block (string->symbol "."))
	'())
       (#t
	block))))

  (define (sugar-load filename)
    (define (load port)
      (let ((inp (sugar-read port)))
	(if (eof-object? inp)
	    #t
	    (begin
	      (eval inp)
	      (load port)))))
    (load (open-input-file filename)))

  (define (sugar-enable)
    (set! read sugar-read)
    ;(set! primitive-load sugar-load)
    )

  (define (sugar-disable)
    (set! read sugar-read-save)
    ;(set! primitive-load sugar-load-save)
    )

;  (sugar-enable)

