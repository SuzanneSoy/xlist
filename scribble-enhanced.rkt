#lang racket

(require (for-syntax mutable-match-lambda
                     racket/string
                     racket/match
                     racket/function
                     racket/syntax)
         scribble-enhanced/with-manual)

;; Correctly display xyz⃰, xyzⁿ, xyz⁰, xyz¹, … xyz⁹
(begin-for-syntax
  (mutable-match-lambda-add-overriding-clause!
   mutable-match-element-id-transformer
   #:match-lambda
   [(? identifier?
       whole-id
       (app (compose symbol->string syntax-e)
            (pregexp
             #px"^(.*?)(⃰|⁺|[⁰¹²³⁴⁵⁶⁷⁸⁹]+⁺?|[⁰¹²³⁴⁵⁶⁷⁸⁹]*⁻[⁰¹²³⁴⁵⁶⁷⁸⁹]*)$"
             (list whole base power))))
    (define/with-syntax base-id (format-id whole-id "~a" base))
    (define/with-syntax power-characters
      (string-join
       (map (match-lambda ["⃰" "*"]
                          ["⁺" "+"]
                          ["⁻" "-"]
                          ;["ⁿ" "n"]
                          ["⁰" "0"] ["¹" "1"] ["²" "2"] ["³" "3"] ["⁴" "4"]
                          ["⁵" "5"] ["⁶" "6"] ["⁷" "7"] ["⁸" "8"] ["⁹" "9"])
            (map string (string->list power)))))
    #`(elem (list #,@(if (> (string-length base) 0) #'((racket base-id)) #'())
                  (superscript power-characters)))]))
