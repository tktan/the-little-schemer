;;; ... (01-toys.scm)
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;; ... (02-do-it-again.scm)
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [else (and (atom? (car l))
                (lat? (cdr l)))])))

;;; ... (04-numbers-games.scm)
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o<
  (lambda (n m)
    (cond
     [(zero? m) #f]
     [(zero? n) #t]
     [else (o< (sub1 n) (sub1 m))])))

(define o>
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (o> (sub1 n) (sub1 m))])))

(define o=
  (lambda (n m)
    (cond
     [(or (o> n m)
          (o< n m))
      #f]
     [else #t])))

(define o+
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (add1 (o+ n (sub1 m)))])))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1)
           (number? a2))
      (o= a1 a2)]
     [(or (number? a1)
          (number? a2))
      #f]
     [else (eq? a1 a2)])))

;;;
;;; (05-oh-my-gawd.scm)
;;;

(define rember*
  (lambda (a l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eqan? (car l) a) (rember* a (cdr l))]
       [else (cons (car l) (rember* a (cdr l)))])]
     [else (cons (rember* a (car l))
                 (rember* a (cdr l)))])))

(let ([a 'cup]
      [l '((coffee)
           cup
           ((tea) cup)
           (and (hick)) cup)])
  (rember* a l))                        ; => ((coffee) ((tea)) (and (hick)))

(let ([a 'sauce]
      [l '(((tomato sauce))
           ((bean) sauce)
           (and ((flying)) sauce))])
  (rember* a l))                        ; => (((tomato)) ((bean)) (and ((flying))))

(let ([l '(((tomato sauce))
           ((bean) sauce)
           (and ((flying)) sauce))])
  (lat? l))                             ; => #f

(let ([l '(((tomato sauce))
           ((bean) sauce)
           (and ((flying)) sauce))])
  (atom? (car l)))                      ; => #f

(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eqan? (car l) old)
        (cons old (cons new (insertR* new old (cdr l))))]
       [else (cons (car l) (insertR* new old (cdr l)))])]
     [else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))])))

(let ([new 'roast]
      [old 'chuck]
      [l '((how much (wood))
           could
           ((a (wood) chuck))
           (((chuck)))
           (if (a) ((wood chuck)))
           could
           chuck
           wood)])
  (insertR* new old l))                 ; => ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)

(define occur*
  (lambda (a l)
    (cond
     [(null? l) 0]
     [(atom? (car l))
      (cond
       [(eqan? (car l) a) (add1 (occur* a (cdr l)))]
       [else (occur* a (cdr l))])]
     [else (o+ (occur* a (car l))
               (occur* a (cdr l)))])))

(let ([a 'banana]
      [l '((banana)
           (split ((((banana ice)))
                   (cream (banana))
                   sherbet))
           (banana)
           (bread)
           (banana brandy))])
  (occur* a l))                         ; => 5

(define subst*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eqan? (car l) old) (cons new (subst* new old (cdr l)))]
       [else (cons (car l) (subst* new old (cdr l)))])]
     [else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))])))

(let ([new 'orange]
      [old 'banana]
      [l '((banana)
           (split ((((banana ice)))
                   (cream (banana))
                   sherbet))
           (banana)
           (bread)
           (banana brandy))])
  (subst* new old l))                   ; => ((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy))

(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(eqan? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
       [else (cons (car l) (insertL* new old (cdr l)))])]
     [else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))])))

(let ([new 'pecker]
      [old 'chuck]
      [l '((how much (wood))
           could
           ((a (wood) chuck))
           (((chuck)))
           (if (a) ((wood chuck)))
           could
           chuck
           wood)])
  (insertL* new old l))                 ; => ((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)

(define member*
  (lambda (a l)
    (cond
     [(null? l) #f]
     [(atom? (car l))
      (or (eqan? (car l) a)
          (member* a (cdr l)))]
     [else (or (member* a (car l))
               (member* a (cdr l)))])))

(let ([a 'chips]
      [l '((potato) (chips ((with) fish) (chips)))])
  (member* a l))                        ; => #t

(let ([a 'chips]
      [l '((potato) (chips ((with) fish) (chips)))])
  (member* a l))                        ; => #t

(define leftmost
  (lambda (l)
    (cond
     [(atom? (car l)) (car l)]
     [else (leftmost (car l))])))

(let ([l '((potato) (chips ((with) fish) (chips)))])
  (leftmost l))                         ; => potato

(let ([l '(((hot) (tuna (and))) cheese)])
  (leftmost l))                         ; => hot

(let ([l '(((()  four)) 17 (seventeen))])
  (leftmost l))                         ; ERROR!

(leftmost '())                          ; ERROR!

(let ([x 'pizza]
      [l '(mozzarella pizza)])
  (and (atom? (car l))
       (eqan? (car l) x)))              ; => #f

(let ([x 'pizza]
      [l '((mozzarella mushroom) pizza)])
  (and (atom? (car l))
       (eqan? (car l) x)))              ; => #f

(let ([x 'pizza]
      [l '(pizza (tastes good))])
  (and (atom? (car l))
       (eqan? (car l) x)))              ; => #t

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1)
           (null? l2))
      #t]
     [(and (null? l1)
           (atom? (car l2)))
      #f]
     [(null? l1) #f]
     [(and (atom? (car l1))
           (null? l2))
      #f]
     [(and (atom? (car l1))
           (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))]
     [(atom? (car l1)) #f]
     [(null? l2) #f]
     [(atom? (car l2)) #f]
     [else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1)
           (null? l2))
      #t]
     [(or (null? l1)
          (null? l2))
      #f]
     [(and (atom? (car l1))
           (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1))
          (atom? (car l2)))
      #f]
     [else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

(let ([l1 '(strawberry ice cream)]
      [l2 '(strawberry ice cream)])
  (eqlist? l1 l2))                      ; => #t

(let ([l1 '(strawberry ice cream)]
      [l2 '(strawberry cream ice)])
  (eqlist? l1 l2))                      ; => #f

(let ([l1 '(banana ((split)))]
      [l2 '((banana) (split))])
  (eqlist? l1 l2))                      ; =#f

(let ([l1 '(beef ((sausage)) (and (soda)))]
      [l2 '(beef ((salami)) (and (soda)))])
  (eqlist? l1 l2))                      ; => #f

(let ([l1 '(beef ((sausage)) (and (soda)))]
      [l2 '(beef ((sausage)) (and (soda)))])
  (eqlist? l1 l2))                      ; => #t

(define oequal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1)
           (atom? s2))
      (eqan? s1 s2)]
     [(atom? s1) #f]
     [(atom? s2) #f]
     [else (eqlist? s1 s2)])))

(define oequal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1)
           (atom? s2))
      (eqan? s1 s2)]
     [(or (atom? s1)
          (atom? s2))
      #f]
     [else (eqlist? s1 s2)])))

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1)
           (null? l2))
      #t]
     [(or (null? l1)
          (null? l2))]
     [else (and (oequal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

(define rember
  (lambda (s l)
    (cond
     [(null? l) '()]
     [(atom? (car l))
      (cond
       [(oequal? (car l) s) (cdr l)]
       [else (cons (car l) (rember s (cdr l)))])]
     [else (cond
            [(oequal? (car l) s) (cdr l)]
            [else (cons (car l) (rember s (cdr l)))])])))

(define rember
  (lambda (s l)
    (cond
     [(null? l) '()]
     [(oequal? (car l) s) (cdr l)]
     [else (cons (car l) (rember s (cdr l)))])))
