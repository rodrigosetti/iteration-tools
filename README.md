# Iteration-tools

This is a implementation in [Racket](http://racket-lang.org) (a fine scheme dialect)
of [Python's itertools library](http://docs.python.org/2/library/itertools.html),
including syntax for [generators](https://wiki.python.org/moin/Generators),
and [generator delegation](http://legacy.python.org/dev/peps/pep-0380/).

## Generators

Generators are "producers" of a sequence of zero or more values in a certain
order. To access those values, one can "iterate" over a generator and evaluate
an expression with a variable binded to the generator produced value each time.

To iterate over a generator, the library provides the `for` construct:

    (for <var> in <generator> <expression> ...)

### Constructors

In order to build a generator, one needs to use the form `generator` and then,
for each value one needs to produce, use the `yield` or `yield-from`:

    (generator <expression> ...)

Inside expression, zero or more occurrences of `yield` can appear:

    (yield <expression>)

`yield-from` is just a expander for `(for x in <generator> (yield x))`, when
it's necessary to use "sub-generators".

Some procedures are available for convenient construction of generators:

 * `iter-count` - generator that counts from start up to stop or infinity.
 * `iter-repeat` - creates a generator that repeats a value up to a limit or
   infinitely.
 * `iter-list` - transforms a list into a generator.

### Combinators

Some convenient combinators on generators are available:

 * `iter-chain` - chain generators.
 * `iter-cycle` - cycle infinitely a generator.
 * `iter-drop` - drop a number of values from the start of generator.
 * `iter-filter` - filter out a generator based on a predicate.
 * `iter-map` - transforms a generator values through a filter function.
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

