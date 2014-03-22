# Itertools for scheme

This is a implementation in [Guile Scheme](https://www.gnu.org/software/guile/)
of [Python's itertools library](http://docs.python.org/2/library/itertools.html),
including syntax for [generators](https://wiki.python.org/moin/Generators),
and [generator delegation](http://legacy.python.org/dev/peps/pep-0380/).

## Syntax

It introduces the following syntactic constructs:

 * `(generator <expression>)` - evaluates to a new generator, than can be
   composed and used in `for` forms.
 * `(for <var> in <generator> . <body>)` - iterate over generator binding
   `<var>` and evaluating `<body>` at each value.
 * `(yield <value>)` - (actually a procedure) used inside the `generator`
   `<expression>` form. Yield a
   new value for iteration, same semantics as Python.
 * `(yield-from <generator>)` - expands to `(for x in <generator> (yield x))`.

It also introduces a bunch of helper procedures, inspired in Python's itertools
library.

## Combinators and Generators

 * `iter-chain` - chain generators.
 * `iter-count` - generator that counts from start up to stop or infinity.
 * `iter-cycle` - cycle infinitely a generator.
 * `iter-drop` - drop a number of values from the start of generator.
 * `iter-filter` - filter out a generator based on a predicate.
 * `iter-list` - transforms a list into a generator.
 * `iter-map` - transforms a generator values through a filter function.
 * `iter-repeat` - creates a generator that repeats a value up to a limit or
   infinitely.
 * `iter-take` - takes up to some number from the start of a generator.
 * `iter-take-until` - takes from a generator until a predicate is true.
 * `iter-zip` - combine multiple generators into a generator of lists.

## Example

```scheme
(define fibonacci
  (generator (let loop ((a 1)
                        (b 1))
               (yield a)
               (loop b (+ a b)))))

(for x in (iter-take 10 fibonacci)
     (display x)
     (newline))
```

Will output:

    1
    1
    2
    3
    5
    8
    13
    21
    34
    55

