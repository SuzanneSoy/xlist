#lang typed/racket

(require xlist/untyped
         typed/rackunit)

(test-begin
 "(xlist 1 2 3 4 5)"
 (check-true (match '() [(xlist) #t] [_ #f]))
 (check-true (match '(1) [(xlist 1) #t] [_ #f]))
 (check-true (match '(1 2) [(xlist 1 2) #t] [_ #f]))
 (check-true (match '(1 2 3) [(xlist 1 2 3) #t] [_ #f]))
 (check-true (match '(1 2 3 4) [(xlist 1 2 3 4) #t] [_ #f]))
 (check-true (match '(1 2 3 4 5) [(xlist 1 2 3 4 5) #t] [_ #f]))

 (check-false (match '() [(xlist 1) #t] [_ #f]))
 (check-false (match '(1) [(xlist 1 2) #t] [_ #f]))
 (check-false (match '(1 2) [(xlist 1 2 3) #t] [_ #f]))
 (check-false (match '(1 2 3) [(xlist 1 2 3 4) #t] [_ #f]))
 (check-false (match '(1 2 3 4) [(xlist 1 2 3 4 5) #t] [_ #f]))
 (check-false (match '(1 2 3 4 5) [(xlist 1 2 3 4 5 6) #t] [_ #f]))

 (check-false (match '(1) [(xlist) #t] [_ #f]))
 (check-false (match '(1 2) [(xlist 1) #t] [_ #f]))
 (check-false (match '(1 2 3) [(xlist 1 2) #t] [_ #f]))
 (check-false (match '(1 2 3 4) [(xlist 1 2 3) #t] [_ #f]))
 (check-false (match '(1 2 3 4 5) [(xlist 1 2 3 4) #t] [_ #f]))
 (check-false (match '(1 2 3 4 5 6) [(xlist 1 2 3 4 5) #t] [_ #f]))
 (void))

;; Should fail:
;(xlist ^ 1)
;(xlist ^ 1 +)
;(xlist ^ 1 *)
;(xlist +)
;(xlist *)

(test-begin
 "(xlist 1 *) and (xlist 1 +) with or witout ^"
 (check-true (match '() [(xlist 1 *) #t] [_ #f]))
 (check-true (match '(1) [(xlist 1 *) #t] [_ #f]))
 (check-true (match '(1 1) [(xlist 1 *) #t] [_ #f]))
 (check-true (match '(1 1 1) [(xlist 1 *) #t] [_ #f]))

 ; NOT (check-true '() (xlist 1 +))
 (check-true (match '(1) [(xlist 1 +) #t] [_ #f]))
 (check-true (match '(1 1) [(xlist 1 +) #t] [_ #f]))
 (check-true (match '(1 1 1) [(xlist 1 +) #t] [_ #f]))

 (check-true (match '() [(xlist 1 ^ *) #t] [_ #f]))
 (check-true (match '(1) [(xlist 1 ^ *) #t] [_ #f]))
 (check-true (match '(1 1) [(xlist 1 ^ *) #t] [_ #f]))
 (check-true (match '(1 1 1) [(xlist 1 ^ *) #t] [_ #f]))

 ; NOT (check-true '() (xlist 1 ^ +))
 (check-true (match '(1) [(xlist 1 ^ +) #t] [_ #f]))
 (check-true (match '(1 1) [(xlist 1 ^ +) #t] [_ #f]))
 (check-true (match '(1 1 1) [(xlist 1 ^ +) #t] [_ #f]))
 (void))


(test-begin
 "(xlist (? number? n) *) and (xlist (? number? n) +) with or witout ^"
 (check-equal? (match '()      [(xlist (? number? n) *)   n] [_ #f]) '())
 (check-equal? (match '(1)     [(xlist (? number? n) *)   n] [_ #f]) '(1))
 (check-equal? (match '(1 1)   [(xlist (? number? n) *)   n] [_ #f]) '(1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) *)   n] [_ #f]) '(1 1 1))

 (check-false  (match '()      [(xlist (? number? n) +)   n] [_ #f]))
 (check-equal? (match '(1)     [(xlist (? number? n) +)   n] [_ #f]) '(1))
 (check-equal? (match '(1 1)   [(xlist (? number? n) +)   n] [_ #f]) '(1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) +)   n] [_ #f]) '(1 1 1))

 (check-equal? (match '()      [(xlist (? number? n) ^ *) n] [_ #f]) '())
 (check-equal? (match '(1)     [(xlist (? number? n) ^ *) n] [_ #f]) '(1))
 (check-equal? (match '(1 1)   [(xlist (? number? n) ^ *) n] [_ #f]) '(1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ *) n] [_ #f]) '(1 1 1 ))

 (check-false  (match '()      [(xlist (? number? n) ^ +) n] [_ #f]))
 (check-equal? (match '(1)     [(xlist (? number? n) ^ +) n] [_ #f]) '(1))
 (check-equal? (match '(1 1)   [(xlist (? number? n) ^ +) n] [_ #f]) '(1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ +) n] [_ #f]) '(1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) *) and (xlist (? number? n) +) something after"
 (check-equal? (match '()              [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '(() . ()))
 (check-equal? (match '(1)             [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '((1) . ()))
 (check-equal? (match '("b")           [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '(() . ("b")))
 (check-equal? (match '(1 "b")         [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '((1) . ("b")))
 (check-equal? (match '(1 1 1 "b" "b") [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '((1 1 1) . ("b" "b")))
 (check-equal? (match '(1 1 1)         [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '((1 1 1) . ()))
 (check-equal? (match '("b" "b" "b")   [(xlist (? number? n) *   (? string? s) *)   (cons n s)] [_ #f]) '(() . ("b" "b" "b")))

 (check-false  (match '()              [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]))
 (check-false  (match '(1)             [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]))
 (check-false  (match '("b")           [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]))
 (check-equal? (match '(1 "b")         [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]) '((1) . ("b")))
 (check-equal? (match '(1 1 "b")       [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]) '((1 1) . ("b")))
 (check-equal? (match '(1 "b" "b")     [(xlist (? number? n) +   (? string? s) +)   (cons n s)] [_ #f]) '((1) . ("b" "b")))

 (check-equal? (match '()              [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '(() . ()))
 (check-equal? (match '(1)             [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '((1) . ()))
 (check-equal? (match '("b")           [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '(() . ("b")))
 (check-equal? (match '(1 "b")         [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '((1) . ("b")))
 (check-equal? (match '(1 1 1 "b" "b") [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '((1 1 1) . ("b" "b")))
 (check-equal? (match '(1 1 1)         [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '((1 1 1) . ()))
 (check-equal? (match '("b" "b" "b")   [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] [_ #f]) '(() . ("b" "b" "b")))

 (check-false  (match '()              [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]))
 (check-false  (match '(1)             [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]))
 (check-false  (match '("b")           [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]))
 (check-equal? (match '(1 "b")         [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]) '((1) . ("b")))
 (check-equal? (match '(1 1 "b")       [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]) '((1 1) . ("b")))
 (check-equal? (match '(1 "b" "b")     [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] [_ #f]) '((1) . ("b" "b")))
 (void))

(test-begin
 "(xlist (? number? n) ^ x +)"
 (check-equal? (match '(1 1 1) [(xlist (? number? n) +)     n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ +)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ 0 +) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ 1 +) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ 2 +) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1) [(xlist (? number? n) ^ 3 +) n] [_ #f]) '(1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) ^ x - y)"
 (check-equal? (match '()          [(xlist (? number? n) ^ -)     n] [_ #f]) '())
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ -)     n] [_ #f]) '(1 1 1))
 (check-equal? (match '()          [(xlist (? number? n) ^ 0 -)   n] [_ #f]) '())
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 0 -)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 1 -)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 2 -)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 3 -)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '()          [(xlist (? number? n) ^ - ∞)   n] [_ #f]) '())
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ - ∞)   n] [_ #f]) '(1 1 1))
 (check-equal? (match '()          [(xlist (? number? n) ^ 0 - ∞) n] [_ #f]) '())
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 0 - ∞) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 1 - ∞) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 2 - ∞) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 3 - ∞) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 0 - 5) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1)     [(xlist (? number? n) ^ 3 - 5) n] [_ #f]) '(1 1 1))
 (check-equal? (match '(1 1 1 1)   [(xlist (? number? n) ^ 0 - 5) n] [_ #f]) '(1 1 1 1))
 (check-equal? (match '(1 1 1 1)   [(xlist (? number? n) ^ 3 - 5) n] [_ #f]) '(1 1 1 1))
 (check-equal? (match '(1 1 1 1 1) [(xlist (? number? n) ^ 0 - 5) n] [_ #f]) '(1 1 1 1 1))
 (check-equal? (match '(1 1 1 1 1) [(xlist (? number? n) ^ 3 - 5) n] [_ #f]) '(1 1 1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) ^ x - (? string? s))"
 (check-equal? (match '("b")       [(xlist (? number? n) ^ -   (? string? s)) (cons n s)] [_ #f]) '(() . "b"))
 (check-equal? (match '(1 1 1 "b") [(xlist (? number? n) ^ -   (? string? s)) (cons n s)] [_ #f]) '((1 1 1) . "b"))
 (check-equal? (match '("b")       [(xlist (? number? n) ^ 0 - (? string? s)) (cons n s)] [_ #f]) '(() . "b"))
 (check-equal? (match '(1 1 1 "b") [(xlist (? number? n) ^ 0 - (? string? s)) (cons n s)] [_ #f]) '((1 1 1) . "b"))
 (check-equal? (match '(1 1 1 "b") [(xlist (? number? n) ^ 1 - (? string? s)) (cons n s)] [_ #f]) '((1 1 1) . "b"))
 (check-equal? (match '(1 1 1 "b") [(xlist (? number? n) ^ 2 - (? string? s)) (cons n s)] [_ #f]) '((1 1 1) . "b"))
 (void))
