;;;;;;;;;;;;;;;;;;;; List functions ;;;;;;;;;;;;;;;;;;;;;

(define car head)
(define cdr tail)
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define caar (lambda (x) (car (car x))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cadr (lambda (x) (car (cdr x))))

(define map (lambda (fn lst) 
    (if (null? lst) 
        lst
        (cons (fn (head lst)) (map fn (tail lst))))))

(define filter
    (lambda (predicate lst)
        (if (null? lst) 
            lst
            (if (predicate (head lst)) 
                (cons (head lst) (filter predicate (tail lst)))
                (filter predicate (tail lst))))))

(define min (lambda lst 
    (begin
        (define smaller 
            (lambda (a b) 
                (if (< a b) a b)))
        (define minrec 
            (lambda (alst acc)
                (if (null? alst) acc
                    (minrec (tail alst) (smaller acc (head alst)))) 
                ))
        (minrec (tail lst) (head lst)))))
                


;;;;;;;;;;;;;;;;;;;;;;;;;; Numeric functions ;;;;;;;;;;;;;;;;;;;;;;;

(define <  (lambda (x y) (not (or (> x y) (= x y)))))
(define >= (lambda (x y) (not (< x y))))
(define <= (lambda (x y) (not (> x y))))
(define abs (lambda (x) (if (< x 0) (- 0 x) x)))
(define zero? (lambda (x) (= x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

