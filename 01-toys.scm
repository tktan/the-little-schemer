;;;
;;; (01-toys.scm)
;;;

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(atom? 'atom)                           ; => #t

(atom? 'turkey)                         ; => #t

(atom? 1492)                            ; => #t

(atom? 'u)                              ; => #t

(atom? '*abc$)                          ; => #t

(list? '(atom))                         ; => #t

(list? '(atom turkey or))               ; => #t

(list? '(atom turkey) or)               ; ERROR!

(list? '((atom turkey) or))             ; => #t

'xyz                                    ; => xyz

'((x y) z)                              ; => ((x y) z)

(list? '(how are you doing so far))     ; => #t

(length '(how are you doing so far))    ; => 6

(list? '(((how) are) ((you) (doing so)) far)) ; => #t

(length '(((how) are) ((you) (doing so)) far)) ; => 3

(list? '())                             ; => #t

(atom? '())                             ; => #f

(list? '(() () () ()))                  ; => #t

(let ([l '(a b c)])
  (car l))                              ; => a

(let ([l '((a b c) x y z)])
  (car l))                              ; => (a b c)

(let ([l 'hotdog])
  (car l))                              ; ERROR!

(let ([l '()])
  (car l))                              ; ERROR!

(let ([l '(((hotdogs)) (and) (pickle) relish)])
  (car l))                              ; => ((hotdogs))

(let ([l '(((hotdogs)) (and))])
  (car (car l)))                        ; => (hotdogs)

(let ([l '((a b c) x y z)])
  (cdr l))                              ; => (x y z)

(let ([l '(hamburger)])
  (cdr l))                              ; => ()

(let ([l '((x) t r)])
  (cdr l))                              ; => (t r)

(let ([a 'hotdogs])
  (cdr a))                              ; ERROR!

(let ([l '()])
  (cdr l))                              ; ERROR!

(let ([l '((b) (x y) ((c)))])
  (car (cdr l)))                        ; => (x y)

(let ([l '((b) (x y) ((c)))])
  (cdr (cdr l)))                        ; => (((c)))

(let ([l '(a (b (c)) d)])
  (cdr (car l)))                        ; ERROR!

(let ([a 'peanut]
      [l '(butter and jelly)])
  (cons a l))                           ; => (peanut butter and jelly)

(let ([s '(banana and)]
      [l '(peanut butter and jelly)])
  (cons s l))                           ; => ((banana and) peanut butter and jelly)

(let ([s '((help) this)]
      [l '(is very ((hard) to learn))])
  (cons s l))                           ; => (((help) this) is very ((hard) to learn))

(let ([s '(a b (c))]
      [l '()])
  (cons s l))                           ; => ((a b (c)))

(let ([s 'a]
      [l '()])
  (cons s l))                           ; => (a)

(let ([s '((a b c))]
      [l 'b])
  (cons s l))                           ; => (((a b c)) . b)

(let ([s 'a]
      [l 'b])
  (cons s l))                           ; => (a . b)

(let ([s 'a]
      [l '((b) c d)])
  (cons s (car l)))                     ; => (a b)

(let ([s 'a]
      [l '((b) c d)])
  (cons s (cdr l)))                     ; => (a c d)

(let ([l '()])
  (null? l))                            ; => #t

(null? (quote ()))                      ; => #t

(let ([l '(a b c)])
  (null? l))                            ; => #f

(let ([a 'spaghetti])
  (null? a))                            ; => #f

(let ([s 'Harry])
  (atom? s))                            ; => #t

(let ([s '(Harry had a heap of apples)])
  (atom? s))                            ; => #f

(let ([l '(Harry had a heap of apples)])
  (atom? (car l)))                      ; => #t

(let ([l '(Harry had a heap of apples)])
  (atom? (cdr l)))                      ; => #f

(let ([l '(Harry)])
  (atom? (cdr l)))                      ; => #f

(let ([l '(swing low sweet cherry oat)])
  (atom? (car (cdr l))))                ; => #t

(let ([l '(swing (low sweet) cherry oat)])
  (atom? (car (cdr l))))                ; => #f

(let ([a1 'Harry]
      [a2 'Harry])
  (eq? a1 a2))                          ; => #t

(let ([a1 'margarine]
      [a2 'butter])
  (eq? a1 a2))                          ; => #f

(let ([l1 '()]
      [l2 '(strawberry)])
  (eq? l1 l2))                          ; => #f

(let ([n1 6]
      [n2 7])
  (eq? n1 n2))                          ; => #f

(let ([l '(Mary had a little lamb chop)]
      [a 'Mary])
  (eq? (car l) a))                      ; => #t

(let ([l '(soured milk)]
      [a 'milk])
  (eq? (cdr l) a))                      ; => #f

(let ([l '(beans beans we need jelly beans)])
  (eq? (car l) (car (cdr l))))          ; => #t
