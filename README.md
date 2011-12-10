scheme-collections
==================

Some data structures intended to be used in a functional manner
in scheme. Inspired by Clojure. Written and tested with Chicken
Scheme.

Lazy Sequences
--------------

Lazy sequences in Scheme from borrowed and extended from here:
http://www.shido.info/lisp/scheme_lazy_e.html

    ; function to create infinite lazy sequence of integers
    ; using lazy-cons
    (define numbers
      (lambda (x)
        (lazy-cons
          x
          (numbers (+ x 1)))))

    ; take
    (seq->list (take 5 (numbers 0)))
    ; -> (0 1 2 3 4)

    ; lazy-map
    (seq->list (take 5 (lazy-map (lambda (x) (* x x)) (numbers 0))))
    ; -> (0 1 4 9 16)

Normal lists may be treated as lazy sequences.
Vectors can be trivially converted with `vector->seq`.

    ;
    (seq->list (take 3 (vector-seq '#(1 2 3 4 5))))
    ; -> (1 2 3)

Hash Tables
-----------

A slightly more convenient way to interact with hash-tables from
SRFI 69 functionally.

    ; create a hash-table
    (hash-map a: 10 b: 20)

    ; create a copy with updated values (no mutation)
    (hash-assoc old-table a: 15)

TODO: This way of dealing with hash-tables is less efficient than
it could be, making complete, shallow copies instead of sharing
structure.

Stephen A. Goss
