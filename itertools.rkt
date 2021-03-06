(module itertools racket
  (require srfi/1)
  (provide generator
           end?
           for
           iter
           next
           yield-from
           iter-chain
           iter-count
           iter-cycle
           iter-drop
           iter-filter
           iter-list
           iter-map
           iter-repeat
           iter-take
           iter-take-until
           iter-zip)

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
      ((for var in gen e1 e2 ...) (let ((it (iter gen)))
                                    (let loop ((var (next it)))
                                      (unless (end? it)
                                        e1 e2 ...
                                        (loop (next it))))))))

  ; http://legacy.python.org/dev/peps/pep-0380/
  (define-syntax yield-from
    (lambda (x)
      (syntax-case x ()
        ((yield-from gen)
         (with-syntax ([yield (datum->syntax #'yield-from 'yield)])
           #'(for e in gen (yield e)))))))

  (define (iter-take n gen)
    (generator
      (let ((it (iter gen)))
        (let loop ((val (next it))
                   (m n))
          (unless (or (end? it) (<= m 0))
            (yield val)
            (loop (next it) (- m 1)))))))

  (define (iter-take-until test gen)
    (generator
      (let ((it (iter gen)))
        (let loop ((val (next it)))
          (unless (or (end? it) (test val))
            (yield val)
            (loop (next it)))))))

  (define (iter-filter pred gen)
    (generator
      (let ((it (iter gen)))
        (let loop ((val (next it)))
          (unless (end? it)
            (when (pred val)
              (yield val))
            (loop (next it)))))))

  (define (iter-map f gen)
    (generator
      (let ((it (iter gen)))
        (let loop ((val (next it)))
          (unless (end? it)
            (yield (f val))
            (loop (next it)))))))

  (define (iter-drop n gen)
    (generator
      (let ((it (iter gen)))
        (let loop ((val (next it))
                   (m n))
          (unless (end? it)
            (when (<= m 0)
              (yield val))
            (loop (next it) (- m 1)))))))

  (define iter-count (case-lambda
                       (() (iter-count 0 1 '()))
                       ((start) (iter-count start 1 '()))
                       ((start step) (iter-count start step '()))
                       ((start step stop) (generator (when (or (null? stop) (< start stop))
                                                       (yield start)
                                                       (yield-from (iter-count (+ start step) step stop)))))))

  (define (iter-zip . gens)
    (define its (map iter gens))
    (generator (let loop ((vals (map next its)))
                 (unless (any end? its)
                   (yield vals)
                   (loop (map next its))))))

  (define (iter-cycle gen)
    (generator (let loop ()
                 (yield-from gen)
                 (loop))))

  (define iter-repeat (case-lambda
                        ((value) (generator (let loop () (yield value) (loop))))
                        ((value times) (iter-take times (iter-repeat value)))))

  (define (iter-chain . gens)
    (generator
      (for-each (lambda (g) (yield-from g)) gens)))

  (define (iter-list lst)
    (generator
      (for-each yield lst)))
  )
