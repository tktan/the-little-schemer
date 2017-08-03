;;; ... (01-toys.scm)
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;;
;;; (04-numbers-games.scm)
;;;

(atom? 14)                              ; => #t

(let ([n 14])
  (atom? n))                            ; => #t

(number? -3)                            ; => #t

(number? 3.14159)                       ; => #t

(and (number? -3)
     (number? 3.14159))                 ; => #t

(define add1
  (lambda (n)
    (+ n 1)))

(let ([n 67])
  (add1 n))                             ; => 68

(add1 67)                               ; => 68

(define sub1
  (lambda (n)
    (- n 1)))

(let ([n 5])
  (sub1 n))                             ; => 4

(sub1 0)                                ; => -1

(zero? 0)                               ; => #t

(zero? 1492)                            ; #f

(define o+
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (add1 (o+ n (sub1 m)))])))

(o+ 46 12)                              ; => 58

(define o-
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (sub1 (o- n (sub1 m)))])))

(o- 14 3)                               ; => 11

(o- 17 9)                               ; => 8

(o- 18 25)                              ; => -7

(define tup?
  (lambda (l)
    (cond
     [(null? l) #t]
     [else (and (number? (car l))
                (tup? (cdr l)))])))

(tup? '(2 11 3 79 47 6))                ; => #t

(tup? '(8 55 5 555))                    ; => #t

(tup? '(1 2 8 apple 4 3))               ; => #f

(tup? '(3 (7 4) 13 9))                  ; => #f

(tup? '())                              ; => #t

(define addtup
  (lambda (tup)
    (cond
     [(null? tup) 0]
     [else (o+ (car tup) (addtup (cdr tup)))])))

(let ([tup '(3 5 2 8)])
  (addtup tup))                         ; => 18

(let ([tup '(15 6 7 12 3)])
  (addtup tup))                         ; => 43

(define o*
  (lambda (n m)
    (cond
     [(zero? m) 0]
     [else (o+ n (o* n (sub1 m)))])))

(o* 5 3)                                ; => 15

(o* 13 4)                               ; => 52

(o* 12 3)                               ; => 36

;;; (= (length tup1) (length tup2)
(define tup+
  (lambda (tup1 tup2)
    (cond
     [(and (null? tup1)
           (null? tup2))
      '()]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

(let ([tup1 '(3 6 9 11 4)]
      [tup2 '(8 5 2 0 7)])
  (tup+ tup1 tup2))                     ; => (11 11 11 11 11)

(let ([tup1 '(2 3)]
      [tup2 '(4 6)])
  (tup+ tup1 tup2))                     ; => (6 9)

(let ([tup1 '(3 7)]
      [tup2 '(4 6)])
  (tup+ tup1 tup2))                     ; => (7 13)

(let ([tup1 '(3 7)]
      [tup2 '(4 6 8 1)])
  (tup+ tup1 tup2))                     ; ERROR!

;;; WRONG!
(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

(let ([tup1 '(3 7 8 1)]
      [tup2 '(4 6)])
  (tup+ tup1 tup2))                     ; ERROR!

;;; OK, but verbose
(define tup+
  (lambda (tup1 tup2)
    (cond
     [(and (null? tup1)
           (null? tup2))
      '()]
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

(> 12 133)                              ; => #f

(> 120 11)                              ; => #t

;;; WRONG!
(define o>
  (lambda (n m)
    (cond
     [(zero? m) #t]
     [(zero? n) #f]
     [else (o> (sub1 n) (sub1 m))])))

(let ([n 3]
      [m 3])
  (o> n m))                             ; => #t

(define o>
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (o> (sub1 n) (sub1 m))])))

(let ([n 3]
      [m 3])
  (o> n m))                             ; => #f

(define o<
  (lambda (n m)
    (cond
     [(zero? m) #f]
     [(zero? n) #t]
     [else (o< (sub1 n) (sub1 m))])))

(o< 4 6)                                ; => #t

(o< 8 3)                                ; => #f

(o< 6 6)                                ; => #f

(define o=
  (lambda (n m)
    (cond
     [(zero? m) (zero? n)]
     [(zero? n) #f]
     [else (o= (sub1 n) (sub1 m))])))

(define o=
  (lambda (n m)
    (cond
     [(o> n m) #f]
     [(o< n m) #f]
     [else #t])))

(define o=
  (lambda (n m)
    (cond
     [(or (o> n m)
          (o< n m))
      #f]
     [else #t])))

(define oexpt
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (o* n (oexpt n (sub1 m)))])))

(oexpt 1 1)                             ; => 1

(oexpt 2 3)                             ; => 8

(oexpt 5 3)                             ; => 125

(define oquotient
  (lambda (n m)
    (cond
     [(o< n m) 0]
     [else (add1 (oquotient (- n m) m))])))

(oquotient 15 4)                        ; => 3

(define olength
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (add1 (olength (cdr lat)))])))

(let ([lat '(hotdogs with mustard sauerkraut and picklets)])
  (olength lat))                        ; => 6

(let ([lat '(ham and cheese on rye)])
  (olength lat))                        ; => 5

(define pick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

(let ([n 4]
      [lat '(lasagna spaghetti ravioli macaroni meatball)])
  (pick n lat))                         ; => macaroni

(let ([lat '(a)])
  (pick 0 lat))                         ; bot

(define rempick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (cdr lat)]
     [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(let ([n 3]
      [lat '(hotdogs with hot mustard)])
  (rempick n lat))                      ; => (hotdogs with mustard)

(let ([a 'tomato])
  (number? a))                          ; => #f

(number? 76)                            ; => #t

(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(number? (car lat)) (no-nums (cdr lat))]
     [else (cons (car lat) (no-nums (cdr lat)))])))

(let ([lat '(5 pears 6 prunes 9 dates)])
  (no-nums lat))                        ; => (pears prunes dates)

(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
     [else (all-nums (cdr lat))])))

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

(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     [(one? n) (cdr lat)]
     [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(let ([n 3]
      [lat '(lemon meringue salty pie)])
  (rempick n lat))                      ; => (lemon meringue pie)
