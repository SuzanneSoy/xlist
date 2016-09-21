#lang typed/racket

(require xlist
         type-expander
         typed/rackunit)

(test-begin
 "(xlist 1 2 3 4 5)"
 (ann '() (xlist))
 (ann '(1) (xlist 1))
 (ann '(1 2) (xlist 1 2))
 (ann '(1 2 3) (xlist 1 2 3))
 (ann '(1 2 3 4) (xlist 1 2 3 4))
 (ann '(1 2 3 4 5) (xlist 1 2 3 4 5))
 (void))

;; Should fail:
; (xlist ^ 1)
; (xlist ^ 1 +)
; (xlist ^ 1 *)
; (xlist +)
; (xlist *)

(test-begin
 "(xlist 1 *) and (xlist 1 +) with or witout ^"
 (ann '() (xlist 1 *))
 (ann '(1) (xlist 1 *))
 (ann '(1 1) (xlist 1 *))
 (ann '(1 1 1) (xlist 1 *))

 ; NOT (ann '() (xlist 1 +))
 (ann '(1) (xlist 1 +))
 (ann '(1 1) (xlist 1 +))
 (ann '(1 1 1) (xlist 1 +))

 (ann '() (xlist 1 ^ *))
 (ann '(1) (xlist 1 ^ *))
 (ann '(1 1) (xlist 1 ^ *))
 (ann '(1 1 1) (xlist 1 ^ *))

 ; NOT (ann '() (xlist 1 ^ +))
 (ann '(1) (xlist 1 ^ +))
 (ann '(1 1) (xlist 1 ^ +))
 (ann '(1 1 1) (xlist 1 ^ +))
 (void))


(test-begin
 "(xlist Number *) and (xlist Number +) with or witout ^"
 (ann '() (xlist Number *))
 (ann '(1) (xlist Number *))
 (ann '(1 1) (xlist Number *))
 (ann '(1 1 1) (xlist Number *))

 ; NOT (ann '() (xlist Number +))
 (ann '(1) (xlist Number +))
 (ann '(1 1) (xlist Number +))
 (ann '(1 1 1) (xlist Number +))

 (ann '() (xlist Number ^ *))
 (ann '(1) (xlist Number ^ *))
 (ann '(1 1) (xlist Number ^ *))
 (ann '(1 1 1) (xlist Number ^ *))

 ; NOT (ann '() (xlist Number ^ +))
 (ann '(1) (xlist Number ^ +))
 (ann '(1 1) (xlist Number ^ +))
 (ann '(1 1 1) (xlist Number ^ +))
 (void))

(test-begin
 "(xlist Number *) and (xlist Number +) something after"
 (ann '() (xlist Number * String *))
 (ann '(1) (xlist Number * String *))
 (ann '("b") (xlist Number * String *))
 (ann '(1 "b") (xlist Number * String *))
 (ann '(1 1 1 "b" "b") (xlist Number * String *))
 (ann '(1 1 1) (xlist Number * String *))
 (ann '("b" "b" "b") (xlist Number * String *))

 ; NOT (ann '() (xlist Number + String +))
 ; NOT (ann '(1) (xlist Number + String +))
 ; NOT (ann '("b") (xlist Number + String +))
 (ann '(1 "b") (xlist Number + String +))
 (ann '(1 1 "b") (xlist Number + String +))
 (ann '(1 "b" "b") (xlist Number + String +))

 (ann '() (xlist Number ^ * String ^ *))
 (ann '(1) (xlist Number ^ * String ^ *))
 (ann '("b") (xlist Number ^ * String ^ *))
 (ann '(1 "b") (xlist Number ^ * String ^ *))
 (ann '(1 1 1 "b" "b") (xlist Number ^ * String ^ *))
 (ann '(1 1 1) (xlist Number ^ * String ^ *))
 (ann '("b" "b" "b") (xlist Number ^ * String ^ *))

 ; NOT (ann '() (xlist Number ^ + String ^ +))
 ; NOT (ann '(1) (xlist Number ^ + String ^ +))
 ; NOT (ann '("b") (xlist Number ^ + String ^ +))
 (ann '(1 "b") (xlist Number ^ + String ^ +))
 (ann '(1 1 "b") (xlist Number ^ + String ^ +))
 (ann '(1 "b" "b") (xlist Number ^ + String ^ +))
 (void))

(test-begin
 "(xlist Number ^ x +)"
 (ann '(1 1 1) (xlist Number +))
 (ann '(1 1 1) (xlist Number ^ +))
 (ann '(1 1 1) (xlist Number ^ 0 +))
 (ann '(1 1 1) (xlist Number ^ 1 +))
 (ann '(1 1 1) (xlist Number ^ 2 +))
 (ann '(1 1 1) (xlist Number ^ 3 +))
 (void))

(test-begin
 "(xlist Number ^ x - y)"
 (ann '() (xlist Number ^ -))
 (ann '(1 1 1) (xlist Number ^ -))
 (ann '() (xlist Number ^ 0 -))
 (ann '(1 1 1) (xlist Number ^ 0 -))
 (ann '(1 1 1) (xlist Number ^ 1 -))
 (ann '(1 1 1) (xlist Number ^ 2 -))
 (ann '(1 1 1) (xlist Number ^ 3 -))
 (ann '() (xlist Number ^ - ∞))
 (ann '(1 1 1) (xlist Number ^ - ∞))
 (ann '() (xlist Number ^ 0 - ∞))
 (ann '(1 1 1) (xlist Number ^ 0 - ∞))
 (ann '(1 1 1) (xlist Number ^ 1 - ∞))
 (ann '(1 1 1) (xlist Number ^ 2 - ∞))
 (ann '(1 1 1) (xlist Number ^ 3 - ∞))
 (ann '(1 1 1) (xlist Number ^ 0 - 5))
 (ann '(1 1 1) (xlist Number ^ 3 - 5))
 (ann '(1 1 1 1) (xlist Number ^ 0 - 5))
 (ann '(1 1 1 1) (xlist Number ^ 3 - 5))
 (ann '(1 1 1 1 1) (xlist Number ^ 0 - 5))
 (ann '(1 1 1 1 1) (xlist Number ^ 3 - 5))
 (void))

(test-begin
 "(xlist Number ^ x - String)"
 (ann '("b") (xlist Number ^ - String))
 (ann '(1 1 1 "b") (xlist Number ^ - String))
 (ann '("b") (xlist Number ^ 0 - String))
 (ann '(1 1 1 "b") (xlist Number ^ 0 - String))
 (ann '(1 1 1 "b") (xlist Number ^ 1 - String))
 (ann '(1 1 1 "b") (xlist Number ^ 2 - String))
 (void))
