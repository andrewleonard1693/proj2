; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2018                              *
; *  Student Version                          *
; *********************************************

;; AUTHOR: ANDREW LEONARD


#lang racket
;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains dictionary definition
;(require "test-dictionary.rkt")
(require "dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;;helper function to reverse the key so the key function works correctly
(define reverseKey
  (lambda (w)
    (if (null? w)
       0
        ;; recursive case making key equal to zero to correctly map each character for the ctv function 
     (+ (* 31 (reverseKey(cdr w)))(ctv (car w))))))

;;creates an appended list of values that each hash function spits out when performed on each word in the passed in dictionary
(define vectorFromHashes
  (lambda (hashFunctionList dict)
    ;;(display hashFunctionList)
    (if (null? hashFunctionList)
        '()
        (append (map (car hashFunctionList) dict) (vectorFromHashes (cdr hashFunctionList) dict))
        )))

;;performs the check on the bitvector and the word
(define performCheck
  (lambda (bitvector word)
    ;;check for empty list
    (if (not(null? word))
        ;;check value of word against the bitvector
        (if (member (car word) bitvector) #t #f) #t)))


;; -----------------------------------------------------
;; KEY FUNCTION
(define key
  ;; w is a list of characters
  (lambda (w)
    ;; call the helper function with the reverse of the word
    (reverseKey(reverse w))
))


;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  ;;assume that size is already prime
  (lambda (size) ;; range of values: 0..size-1
    ;;(println size)
    ;;additional lambda to pass in the word
    (lambda (word)
      (modulo (key word) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    ;;additional lambda to pass in the word
    (lambda (word)
      ;;multiplication hash function
      (floor(* size (- (* (key word) A) (floor (* (key word) A))))))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 701))
(define hash-2 (gen-hash-division-method 899))
(define hash-3 (gen-hash-multiplication-method 700))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))


;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o)) ==> 674
;;  (hash-1 '(d a y))     ==> 395
;;  (hash-1 '(c l a s s)) ==> 360
;;
;;  (hash-2 '(h e l l o)) ==> 139
;;  (hash-2 '(d a y))     ==> 304
;;  (hash-2 '(c l a s s)) ==> 205
;;
;;  (hash-3 '(h e l l o)) ==> 552.0
;;  (hash-3 '(d a y))     ==> 501.0
;;  (hash-3 '(c l a s s)) ==> 247.0
;;
;;  (hash-4 '(h e l l o)) ==> 710.0
;;  (hash-4 '(d a y))     ==> 644.0
;;  (hash-4 '(c l a s s)) ==> 317.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda (word)
      (performCheck (vectorFromHashes hashfunctionlist dict)(vectorFromHashes hashfunctionlist (list word)))
    )))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

