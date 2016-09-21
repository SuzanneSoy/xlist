#lang typed/racket

(require xlist
         type-expander
         typed/rackunit)

;; Should fail (for now)
;(test-begin
; "(xlist 1 2 3 4 5)"
; (ann '() (xlist))
; (ann '(1) (xlist 1¹))
; (ann '(1 2) (xlist 1¹ 2¹))
; (ann '(1 2 3) (xlist 1¹ 2¹ 3¹))
; (ann '(1 2 3 4) (xlist 1¹ 2¹ 3¹ 4¹))
; (ann '(1 2 3 4 5) (xlist 1¹ 2¹ 3¹ 4¹ 5¹))
; (void))

;; Should fail:
; (xlist ^ 1)
; (xlist ^ 1 +)
; (xlist ^ 1 *)
; (xlist +)
; (xlist *)

;(test-begin
; "(xlist 1 *) and (xlist 1 +) with or witout ^"
; (ann '() (xlist 1 *))
; (ann '(1) (xlist 1 *))
; (ann '(1 1) (xlist 1 *))
; (ann '(1 1 1) (xlist 1 *))
;
; ; NOT (ann '() (xlist 1 +))
; (ann '(1) (xlist 1 +))
; (ann '(1 1) (xlist 1 +))
; (ann '(1 1 1) (xlist 1 +))
;
; (ann '() (xlist 1 ^ *))
; (ann '(1) (xlist 1 ^ *))
; (ann '(1 1) (xlist 1 ^ *))
; (ann '(1 1 1) (xlist 1 ^ *))
;
; ; NOT (ann '() (xlist 1 ^ +))
; (ann '(1) (xlist 1 ^ +))
; (ann '(1 1) (xlist 1 ^ +))
; (ann '(1 1 1) (xlist 1 ^ +))
; (void))


(test-begin
 "(xlist Number⃰) and (xlist Number⁺) with or without space"
 (ann '() (xlist Number⃰))
 (ann '(1) (xlist Number⃰))
 (ann '(1 1) (xlist Number⃰))
 (ann '(1 1 1) (xlist Number⃰))

 ; NOT (ann '() (xlist Number⁺))
 (ann '(1) (xlist Number⁺))
 (ann '(1 1) (xlist Number⁺))
 (ann '(1 1 1) (xlist Number⁺))

 (ann '() (xlist Number ⃰))
 (ann '(1) (xlist Number ⃰))
 (ann '(1 1) (xlist Number ⃰))
 (ann '(1 1 1) (xlist Number ⃰))

 ; NOT (ann '() (xlist Number ⁺))
 (ann '(1) (xlist 1 ⁺))
 (ann '(1 1) (xlist 1 ⁺))
 (ann '(1 1 1) (xlist 1 ⁺))
 (void))

(test-begin
 "(xlist Number⃰) and (xlist Number +) something after"
 (ann '() (xlist Number⃰ String⃰))
 (ann '(1) (xlist Number⃰ String⃰))
 (ann '("b") (xlist Number⃰ String⃰))
 (ann '(1 "b") (xlist Number⃰ String⃰))
 (ann '(1 1 1 "b" "b") (xlist Number⃰ String⃰))
 (ann '(1 1 1) (xlist Number⃰ String⃰))
 (ann '("b" "b" "b") (xlist Number⃰ String⃰))

 ; NOT (ann '() (xlist Number⁺ String⁺))
 ; NOT (ann '(1) (xlist Number⁺ String⁺))
 ; NOT (ann '("b") (xlist Number⁺ String⁺))
 (ann '(1 "b") (xlist Number⁺ String⁺))
 (ann '(1 1 "b") (xlist Number⁺ String⁺))
 (ann '(1 "b" "b") (xlist Number⁺ String⁺))

 (ann '() (xlist Number ⃰ String ⃰))
 (ann '(1) (xlist Number ⃰ String ⃰))
 (ann '("b") (xlist Number ⃰ String ⃰))
 (ann '(1 "b") (xlist Number ⃰ String ⃰))
 (ann '(1 1 1 "b" "b") (xlist Number ⃰ String ⃰))
 (ann '(1 1 1) (xlist Number ⃰ String ⃰))
 (ann '("b" "b" "b") (xlist Number ⃰ String ⃰))

 ; NOT (ann '() (xlist Number ⁺ String ⁺))
 ; NOT (ann '(1) (xlist Number ⁺ String ⁺))
 ; NOT (ann '("b") (xlist Number ⁺ String ⁺))
 (ann '(1 "b") (xlist Number ⁺ String ⁺))
 (ann '(1 1 "b") (xlist Number ⁺ String ⁺))
 (ann '(1 "b" "b") (xlist Number ⁺ String ⁺))
 (void))

(test-begin
 "(xlist Numberⁿ⁺) with or without space"
 (ann '(1 1 1) (xlist Number⁺))
 (ann '(1 1 1) (xlist Number⁰⁺))
 (ann '(1 1 1) (xlist Number¹⁺))
 (ann '(1 1 1) (xlist Number²⁺))
 (ann '(1 1 1) (xlist Number³⁺))
 (ann '(1 1 1) (xlist Number ⁺))
 (ann '(1 1 1) (xlist Number ⁰⁺))
 (ann '(1 1 1) (xlist Number ¹⁺))
 (ann '(1 1 1) (xlist Number ²⁺))
 (ann '(1 1 1) (xlist Number ³⁺))
 (void))

(test-begin
 "(xlist Numberⁱ⁻ⁿ) without space"
 (ann '() (xlist Number⁻))
 (ann '(1 1 1) (xlist Number⁻))
 (ann '() (xlist Number⁰⁻))
 (ann '(1 1 1) (xlist Number⁰⁻))
 (ann '(1 1 1) (xlist Number¹⁻))
 (ann '(1 1 1) (xlist Number²⁻))
 (ann '(1 1 1) (xlist Number³⁻))
 ;(ann '(1 1 1) (xlist Number ^ - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 0 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 1 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 2 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 3 - ∞))
 (ann '(1 1 1) (xlist Number⁰⁻⁵))
 (ann '(1 1 1) (xlist Number³⁻⁵))
 (ann '(1 1 1 1) (xlist Number⁰⁻⁵))
 (ann '(1 1 1 1) (xlist Number³⁻⁵))
 (ann '(1 1 1 1 1) (xlist Number⁰⁻⁵))
 (ann '(1 1 1 1 1) (xlist Number⁰⁻⁵))
 (void))

(test-begin
 "(xlist Number ⁱ⁻ⁿ) with space"
 (ann '() (xlist Number ⁻))
 (ann '(1 1 1) (xlist Number ⁻))
 (ann '() (xlist Number ⁰⁻))
 (ann '(1 1 1) (xlist Number ⁰⁻))
 (ann '(1 1 1) (xlist Number ¹⁻))
 (ann '(1 1 1) (xlist Number ²⁻))
 (ann '(1 1 1) (xlist Number ³⁻))
 ;(ann '() (xlist Number ^ - ∞))
 ;(ann '(1 1 1) (xlist Number ^ - ∞))
 ;(ann '() (xlist Number ^ 0 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 0 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 1 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 2 - ∞))
 ;(ann '(1 1 1) (xlist Number ^ 3 - ∞))
 (ann '(1 1 1) (xlist Number ⁰⁻⁵))
 (ann '(1 1 1) (xlist Number ³⁻⁵))
 (ann '(1 1 1 1) (xlist Number ⁰⁻⁵))
 (ann '(1 1 1 1) (xlist Number ³⁻⁵))
 (ann '(1 1 1 1 1) (xlist Number ⁰⁻⁵))
 (ann '(1 1 1 1 1) (xlist Number ⁰⁻⁵))
 (void))

(test-begin
 "(xlist Numberⁿ⁻ String)"
 (ann '("b") (xlist Number⁻ String))
 (ann '(1 1 1 "b") (xlist Number⁻ String))
 (ann '("b") (xlist Number⁰⁻ String))
 (ann '(1 1 1 "b") (xlist Number⁰⁻ String))
 (ann '(1 1 1 "b") (xlist Number¹⁻ String))
 (ann '(1 1 1 "b") (xlist Number²⁻ String))
 (void))
