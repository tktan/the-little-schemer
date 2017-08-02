;;;
;;; (03-cons-the-magnificent.scm)
;;;

;;; WRONG!
(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) a) (cdr lat)]
            [else (rember a (cdr lat))])])))

(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) a) (cdr lat)]
            [else (cons (car lat)
                        (rember a (cdr lat)))])])))

(let ([a 'mint]
      [lat '(lamb chops and mint jelly)])
  (rember a lat))                       ; => (lamb chops and jelly)

(let ([a 'mint]
      [lat '(lamb chops and mint flavored mint jelly)])
  (rember a lat))                       ; => (lamb chops and flavored mint jelly)

(let ([a 'toast]
      [lat '(bacon lettuce and tomato)])
  (rember a lat))                       ; => (bacon lettuce and tomato)

(let ([a 'cup]
      [lat '(coffee cup tea cup and hick cup)])
  (rember a lat))                       ; => (coffee tea cup and hick cup)

(let ([a 'and]
      [lat '(bacon lettuce and tomato)])
  (rember a lat))                       ; => (bacon lettuce tomato)

(let ([a 'sauce]
      [lat '(soy sauce and tomato sauce)])
  (rember a lat))                       ; => (soy and tomato sauce)

(define firsts
  (lambda (l)
    (cond
     [(null? l) '()]
     [else (cons (caar l) (firsts (cdr l)))])))

(let ([l '((apple peach pumpkin)
           (plum pear cherry)
           (grape raisin pea)
           (bean carrot eggplatn))])
  (firsts l))                           ; => (apple plum grape bean)

(let ([l '((a b) (c d) (e f))])
  (firsts l))                           ; => (a c e)

(let ([l '()])
  (firsts l))                           ; => ()

(let ([l '((five plums)
           (four)
           (eleven green oranges))])
  (firsts l))                           ; => (five four eleven)

(let ([l '(((five plums) four)
           (eleven green oranges)
           ((no) more))])
  (firsts l))                           ; => ((five plums) eleven (no))

;;; WRONG!
(define insertR
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cdr lat)]
          [else (cons (car lat) (insertR new old (cdr lat)))])))

(let ([new 'topping]
      [old 'fudge]
      [lat '(ice cream with fudge for dessert)])
  (insertR new old lat))                ; => (ice cream with for dessert)

;;; WRONG!
(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons new (cdr lat))]
     [else (cons (car lat) (insertR new old (cdr lat)))])))

(let ([new 'topping]
      [old 'fudge]
      [lat '(ice cream with fudge for dessert)])
  (insertR new old lat))                ; => (ice cream with topping for dessert)

(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
     [else (cons (car lat) (insertR new old (cdr lat)))])))

(let ([new 'topping]
      [old 'fudge]
      [lat '(ice cream with fudge for dessert)])
  (insertR new old lat))                ; => (ice cream with fudge topping for dessert)

(let ([new 'jalapeño]
      [old 'and]
      [lat '(tacos tamales and salsa)])
  (insertR new old lat))                ; => (tacos tamales and jalapeño salsa)

(let ([new 'e]
      [old 'd]
      [lat '(a b c d f g d h)])
  (insertR new old lat))                ; => (a b c d e f g d h)

(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons new lat)]
     [else (cons (car lat) (insertL new old (cdr lat)))])))

(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old) (cons new (cdr lat))]
     [else (cons (car lat) (subst new old (cdr lat)))])))

(let ([new 'topping]
      [old 'fudge]
      [lat '(ice cream with fudge for dessert)])
  (subst new old lat))                  ; => (ice cream with topping for dessert)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) '()]
     [(or (eq? (car lat) o1)
          (eq? (car lat) o2))
      (cons new (cdr lat))]
     [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

(let ([new 'vanilla]
      [o1 'chocolate]
      [o2 'banana]
      [lat '(banana ice cream with chocolate topping)])
  (subst2 new o1 o2 lat))               ; => (vanilla ice cream with chocolate topping)

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (multirember a (cdr lat))]
     [else (cons (car lat) (multirember a (cdr lat)))])))

(let ([a 'cup]
      [lat '(coffee cup tea cup and hick cup)])
  (multirember a lat))                  ; => (coffee tea and hick)

(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons old
            (cons new
                  (multiinsertR new old (cdr lat))))]
     [else (cons (car lat)
                 (multiinsertR new old (cdr lat)))])))

;;; WRONG!
(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new
            (cons old (multiinsertL new old lat)))]
     [else (cons (car lat)
                 (multiinsertL new old (cdr lat)))])))

(let ([new 'fried]
      [old 'fish]
      [lat '(chips and fish or fish and fried)])
  (multiinsertL new old lat))           ; bot

(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new
            (cons old (multiinsertL new old (cdr lat))))]
     [else (cons (car lat)
                 (multiinsertL new old (cdr lat)))])))

(define multisubst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new (multisubst new old (cdr lat)))]
     [else (cons (car lat) (multisubst new old (cdr lat)))])))
