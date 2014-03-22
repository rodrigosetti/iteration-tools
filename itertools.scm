(define-module (itertools))
(use-modules (srfi srfi-1))
(export generator iter next end? for iter-take iter-drop iter-count)

; ***************************************
; *************** CORE ******************
; ***************************************

(define-syntax generator
  (lambda (x)
    (syntax-case x ()
                 ((generator . body)
                  (with-syntax ([yield (datum->syntax #'generator 'yield)])
                               #'(lambda (cont)
                                   (define (yield arg)
                                     (set! cont (call-with-current-continuation
                                                  (lambda (c) (cont (cons c arg))))))
                                   (begin . body)
                                   'done))))))

(define (iter gen)
  (define state 'alive)
  (lambda (command)
    (case command
      ((next)
       (case state
         ((alive)
          (let ((result (call-with-current-continuation gen)))
            (if (eq? result 'done)
              (set! state 'dead)
              (let ((gen-breakpoint (car result))
                    (value (cdr result)))
                (set! gen gen-breakpoint)
                value))))
         ((dead) '())))
      ((end?) (eq? state 'dead)))))

(define (next iterator)
  (iterator 'next))

(define (end? iterator)
  (iterator 'end?))

; ***************************************
; ************** HELPERS  ***************
; ***************************************

(define-syntax for
  (syntax-rules (in)
                ((_ var in gen e1 e2 ...) (let ((it (iter gen)))
                                            (let loop ((var (next it)))
                                              (when (not (end? it))
                                                e1 e2 ... (loop (next it))))))))

; http://legacy.python.org/dev/peps/pep-0380/
(define-syntax yield-from
  (lambda (x)
    (syntax-case x ()
                 ((yield-from gen)
                  (with-syntax ([yield (datum->syntax #'yield-from 'yield)])
                               #'(for e in gen (yield e)))))))

(define (iter-take n gen)
  (define it (iter gen))
  (generator (let loop ((val (next it))
                        (m n))
               (when (and (not (end? it)) (> m 0))
                 (yield val)
                 (loop (next it) (- m 1))))))

(define (iter-drop n gen)
  (define it (iter gen))
  (generator (let loop ((val (next it))
                        (m n))
               (when (not (end? it))
                 (when (<= m 0)
                   (yield val))
                 (loop (next it) (- m 1))))))

(define iter-count (case-lambda
                (() (iter-count 0 1 '()))
                ((start) (iter-count start 1 '()))
                ((start step) (iter-count start step '()))
                ((start step stop) (generator (when (or (null? stop) (< start stop))
                                                (yield start)
                                                (yield-from (iter-count (+ start step) step stop)))))))

