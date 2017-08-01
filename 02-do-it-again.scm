;;; ... (01-toys.scm)
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;;
;;; (02-do-it-again.scm)
;;;
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [else (and (atom? (car l))
                (lat? (cdr l)))])))

(let ([l '(Jack Sprat could eat no chicken fat)])
  (lat? l))                             ; => #t

(let ([l '((Jack) Sprat could eat no chicken fat)])
  (lat? l))                             ; => #f

(let ([l '(Jack (Sprat could) eat no chicken fat)])
  (lat? l))                             ; => #f

(let ([l '()])
  (lat? l))                             ; => #t

(let ([l '(bacon and eggs)])
  (lat? l))                             ; => #t

(let ([l1 '()]
      [l2 '(d e f g)])
  (or (null? l1) (atom? l2)))           ; => #t

(let ([l1 '(a b c)]
      [l2 '()])
  (or (null? l1) (null? l2)))           ; => #t

(let ([l1 '(a b c)]
      [l2 '(atom)])
  (or (null? l1) (null? l2)))           ; => #f

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (or (eq? (car lat) a)
               (member? a (cdr lat)))])))

(let ([a 'tea]
      [lat '(coffee tea or milk)])
  (member? a lat))                      ; => #t

(let ([a 'poached]
      [lat '(fried eggs and scrambled eggs)])
  (member? a lat))                      ; => #f

(let ([a 'meat]
      [lat '(mashed potatoes and meat gravy)])
  (member? a lat))                      ; => #t

(let ([a 'liver]
      [lat '(bagels and lox)])
  (member? a lat))                      ; => #f
