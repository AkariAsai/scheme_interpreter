(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

(define (square x)
  (* x x))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond ((null? items) '())
    (else (cons (proc (car items)) (map proc (cdr items))))
  )
)

(define (cons-all first rests)
  (cond ((null? rests) nil)
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) '())))
  )
)

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (helper n lst)
    (cond((null? lst) nil)
      (else (cons (list n (car lst)) (helper (+ n 1) (cdr lst)))))
      )
  (helper 0 s))))

  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (add s v)
    (cond ((and (null? s) (null? v)) '())
          ((null? s) (cons (car v) (add s (cdr v))))
          (else (cons (car s) (add (cdr s) v))) ; replace this line
    )
)
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond((or (null? denoms) (< total 0)) '())
        ((> (car denoms) total) (list-change total (cdr denoms)))
        ((= (car denoms) total) (cons (list (car denoms)) (list-change total (cdr denoms))))
        (else (add (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
    )
  )
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (cond
             ((eq? 'define form) (cons 'lambda (cons (map analyze params) (cons (map analyze body) nil))))
             (else (cons form (cons (map analyze params) (map analyze body))))
           )
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
              (define expression (zip values))
              (cons (cons 'lambda (cons (car expression) (map analyze body))) (map analyze (cadr expression)))
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (cons (car expr) (map analyze (cdr expr)))
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21

