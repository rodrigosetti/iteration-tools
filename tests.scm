(use-modules (itertools) (srfi srfi-1) (srfi srfi-64))

; ***************************************
; ******* USEFUL TEST MACROS ************
; ***************************************

(define-syntax accumulate
  (syntax-rules ()
                ((accumulate lst e) (set! lst (append! lst (list e))))))

; ***************************************
; ************** FIXTURES ***************
; ***************************************

(define simple
  (generator (yield 1)
             (let ((x 2))
               (yield x)
               (yield (+ x 1))
               (yield 4)
               (yield 5)
               (yield (* x 3))
               (yield 7)
               (yield 8)
               (set! x 9)
               (yield x))
             (yield 10)
             "ignored value"))

(define fibonacci
  (generator (let loop ((a 1)
                        (b 1))
               (yield a)
               (loop b (+ a b)))))

; ***************************************
; *************** TESTS *****************
; ***************************************

(test-begin "itertools-test")

(let ((accum (list)))

    (for x in simple
         (accumulate accum x))

    (test-equal "simple for" '(1 2 3 4 5 6 7 8 9 10) accum))


(let ((accum (list)))

    (for x in (iter-take 10 fibonacci)
         (accumulate accum x))

    (test-equal "take 10 from infinite iterator"
                '(1 1 2 3 5 8 13 21 34 55)
                accum))


(let ((accum (list)))

(for x in (iter-take 10 (iter-drop 10 fibonacci))
     (accumulate accum x))

    (test-equal "take 10 of drop 10 from infinite iterator"
                '(89 144 233 377 610 987 1597 2584 4181 6765)
                accum))

(let ((accum (list)))

  (for x in (iter-count 0 2 50)
       (accumulate accum x))

  (test-equal "count from 0 to 50 (step 2)"
              '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48)
              accum))

(let ((accum (list)))

  (for x in (iter-zip simple fibonacci)
       (accumulate accum x))

  (test-equal "zip two generators"
              '((1 1) (2 1) (3 2) (4 3) (5 5) (6 8) (7 13) (8 21) (9 34) (10 55))
              accum))

(test-end "itertools-test")

