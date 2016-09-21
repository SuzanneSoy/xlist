#lang racket

(require xlist/untyped
         rackunit)

(define-syntax-rule (check-match v clause result)
  (check-equal? (let ([no-match (gensym 'no-match)])
                  (match v clause [_ no-match]))
                result))

(define-syntax-rule (check-not-match v pat)
  (let ([no-match (gensym 'no-match)])
    (check-equal? (match v pat [_ no-match])
                  no-match)))

(define-syntax-rule (check-match? v pat)
  (check-true (match v [pat #t] [_ #f])))

(define-syntax-rule (check-not-match? v pat)
  (check-false (match v [pat #t] [_ #f])))

(test-begin
 "(xlist 1 2 3 4 5)"
 (check-match?       '()          (xlist))
 (check-match?       '(1)         (xlist 1))
 (check-match?       '(1 2)       (xlist 1 2))
 (check-match?       '(1 2 3)     (xlist 1 2 3))
 (check-match?       '(1 2 3 4)   (xlist 1 2 3 4))
 (check-match?       '(1 2 3 4 5) (xlist 1 2 3 4 5))

 (check-not-match?   '()          (xlist 1))
 (check-not-match?   '(1)         (xlist 1 2))
 (check-not-match?   '(1 2)       (xlist 1 2 3))
 (check-not-match?   '(1 2 3)     (xlist 1 2 3 4))
 (check-not-match?   '(1 2 3 4)   (xlist 1 2 3 4 5))
 (check-not-match?   '(1 2 3 4 5) (xlist 1 2 3 4 5 6))

 (check-not-match?   '(1)           (xlist))
 (check-not-match?   '(1 2)         (xlist 1))
 (check-not-match?   '(1 2 3)       (xlist 1 2))
 (check-not-match?   '(1 2 3 4)     (xlist 1 2 3))
 (check-not-match?   '(1 2 3 4 5)   (xlist 1 2 3 4))
 (check-not-match?   '(1 2 3 4 5 6) (xlist 1 2 3 4 5))
 (void))

;; Should fail:
;(xlist ^ 1)
;(xlist ^ 1 +)
;(xlist ^ 1 *)
;(xlist +)
;(xlist *)

(test-begin
 "(xlist 1 *) and (xlist 1 +) with or witout ^"
 (check-match?     '()      (xlist 1 *))
 (check-match?     '(1)     (xlist 1 *))
 (check-match?     '(1 1)   (xlist 1 *))
 (check-match?     '(1 1 1) (xlist 1 *))

 (check-not-match? '()      (xlist 1 +))
 (check-match?     '(1)     (xlist 1 +))
 (check-match?     '(1 1)   (xlist 1 +))
 (check-match?     '(1 1 1) (xlist 1 +))

 (check-match?     '()      (xlist 1 ^ *))
 (check-match?     '(1)     (xlist 1 ^ *))
 (check-match?     '(1 1)   (xlist 1 ^ *))
 (check-match?     '(1 1 1) (xlist 1 ^ *))

 (check-not-match? '()      (xlist 1 ^ +))
 (check-match?     '(1)     (xlist 1 ^ +))
 (check-match?     '(1 1)   (xlist 1 ^ +))
 (check-match?     '(1 1 1) (xlist 1 ^ +))
 (void))


(test-begin
 "(xlist (? number? n) *) and (xlist (? number? n) +) with or witout ^"
 (check-match     '()      [(xlist (? number? n) *)   n] '())
 (check-match     '(1)     [(xlist (? number? n) *)   n] '(1))
 (check-match     '(1 1)   [(xlist (? number? n) *)   n] '(1 1))
 (check-match     '(1 1 1) [(xlist (? number? n) *)   n] '(1 1 1))

 (check-not-match '()      [(xlist (? number? n) +)   n])
 (check-match     '(1)     [(xlist (? number? n) +)   n] '(1))
 (check-match     '(1 1)   [(xlist (? number? n) +)   n] '(1 1))
 (check-match     '(1 1 1) [(xlist (? number? n) +)   n] '(1 1 1))

 (check-match     '()      [(xlist (? number? n) ^ *) n] '())
 (check-match     '(1)     [(xlist (? number? n) ^ *) n] '(1))
 (check-match     '(1 1)   [(xlist (? number? n) ^ *) n] '(1 1))
 (check-match     '(1 1 1) [(xlist (? number? n) ^ *) n] '(1 1 1 ))

 (check-not-match '()      [(xlist (? number? n) ^ +) n])
 (check-match     '(1)     [(xlist (? number? n) ^ +) n] '(1))
 (check-match     '(1 1)   [(xlist (? number? n) ^ +) n] '(1 1))
 (check-match     '(1 1 1) [(xlist (? number? n) ^ +) n] '(1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) *) and (xlist (? number? n) +) something after"
 (check-match     '()              [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '(() . ()))
 (check-match     '(1)             [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '((1) . ()))
 (check-match     '("b")           [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '(() . ("b")))
 (check-match     '(1 "b")         [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '((1) . ("b")))
 (check-match     '(1 1 1 "b" "b") [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '((1 1 1) . ("b" "b")))
 (check-match     '(1 1 1)         [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '((1 1 1) . ()))
 (check-match     '("b" "b" "b")   [(xlist (? number? n) *   (? string? s) *)   (cons n s)] '(() . ("b" "b" "b")))

 (check-not-match '()              [(xlist (? number? n) +   (? string? s) +)   (cons n s)])
 (check-not-match '(1)             [(xlist (? number? n) +   (? string? s) +)   (cons n s)])
 (check-not-match '("b")           [(xlist (? number? n) +   (? string? s) +)   (cons n s)])
 (check-match     '(1 "b")         [(xlist (? number? n) +   (? string? s) +)   (cons n s)] '((1) . ("b")))
 (check-match     '(1 1 "b")       [(xlist (? number? n) +   (? string? s) +)   (cons n s)] '((1 1) . ("b")))
 (check-match     '(1 "b" "b")     [(xlist (? number? n) +   (? string? s) +)   (cons n s)] '((1) . ("b" "b")))

 (check-match     '()              [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '(() . ()))
 (check-match     '(1)             [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '((1) . ()))
 (check-match     '("b")           [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '(() . ("b")))
 (check-match     '(1 "b")         [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '((1) . ("b")))
 (check-match     '(1 1 1 "b" "b") [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '((1 1 1) . ("b" "b")))
 (check-match     '(1 1 1)         [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '((1 1 1) . ()))
 (check-match     '("b" "b" "b")   [(xlist (? number? n) ^ * (? string? s) ^ *) (cons n s)] '(() . ("b" "b" "b")))

 (check-not-match '()              [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)])
 (check-not-match '(1)             [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)])
 (check-not-match '("b")           [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)])
 (check-match     '(1 "b")         [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] '((1) . ("b")))
 (check-match     '(1 1 "b")       [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] '((1 1) . ("b")))
 (check-match     '(1 "b" "b")     [(xlist (? number? n) ^ + (? string? s) ^ +) (cons n s)] '((1) . ("b" "b")))
 (void))

(test-begin
 "(xlist (? number? n) ^ x +)"
 (check-match   '(1 1 1) [(xlist (? number? n) +)     n] '(1 1 1))
 (check-match   '(1 1 1) [(xlist (? number? n) ^ +)   n] '(1 1 1))
 (check-match   '(1 1 1) [(xlist (? number? n) ^ 0 +) n] '(1 1 1))
 (check-match   '(1 1 1) [(xlist (? number? n) ^ 1 +) n] '(1 1 1))
 (check-match   '(1 1 1) [(xlist (? number? n) ^ 2 +) n] '(1 1 1))
 (check-match   '(1 1 1) [(xlist (? number? n) ^ 3 +) n] '(1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) ^ x - y)"
 (check-match   '()          [(xlist (? number? n) ^ -)     n] '())
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ -)     n] '(1 1 1))
 (check-match   '()          [(xlist (? number? n) ^ 0 -)   n] '())
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 0 -)   n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 1 -)   n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 2 -)   n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 3 -)   n] '(1 1 1))
 (check-match   '()          [(xlist (? number? n) ^ - ∞)   n] '())
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ - ∞)   n] '(1 1 1))
 (check-match   '()          [(xlist (? number? n) ^ 0 - ∞) n] '())
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 0 - ∞) n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 1 - ∞) n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 2 - ∞) n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 3 - ∞) n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 0 - 5) n] '(1 1 1))
 (check-match   '(1 1 1)     [(xlist (? number? n) ^ 3 - 5) n] '(1 1 1))
 (check-match   '(1 1 1 1)   [(xlist (? number? n) ^ 0 - 5) n] '(1 1 1 1))
 (check-match   '(1 1 1 1)   [(xlist (? number? n) ^ 3 - 5) n] '(1 1 1 1))
 (check-match   '(1 1 1 1 1) [(xlist (? number? n) ^ 0 - 5) n] '(1 1 1 1 1))
 (check-match   '(1 1 1 1 1) [(xlist (? number? n) ^ 3 - 5) n] '(1 1 1 1 1))
 (void))

(test-begin
 "(xlist (? number? n) ^ x - (? string? s))"
 (check-match   '("b")       [(xlist (? number? n) ^ -   (? string? s)) (cons n s)] '(() . "b"))
 (check-match   '(1 1 1 "b") [(xlist (? number? n) ^ -   (? string? s)) (cons n s)] '((1 1 1) . "b"))
 (check-match   '("b")       [(xlist (? number? n) ^ 0 - (? string? s)) (cons n s)] '(() . "b"))
 (check-match   '(1 1 1 "b") [(xlist (? number? n) ^ 0 - (? string? s)) (cons n s)] '((1 1 1) . "b"))
 (check-match   '(1 1 1 "b") [(xlist (? number? n) ^ 1 - (? string? s)) (cons n s)] '((1 1 1) . "b"))
 (check-match   '(1 1 1 "b") [(xlist (? number? n) ^ 2 - (? string? s)) (cons n s)] '((1 1 1) . "b"))
 (void))
