;;; my attempt at making a useful hash table

(require "srfi-69")

(define hash-build
  (lambda args
    (let ((hm (car args))
          (key (cadr args))
          (val (caddr args)))
      (hash-table-set! hm key val)
      (if (pair? (cdddr args))
        (apply hash-build hm (cdddr args))
        hm))))

;; hash table constructor
;; usage: (hash-map a: 10 b: 20)
(define hash-map
  (lambda elems
    (let ((hm (make-hash-table)))
      (if (pair? elems)
        (apply hash-build hm elems)
        hm))))

;; makes a shallow copy of hash table
;; and updates with supplied values
;; usage: (hash-assoc h a: 15 b: 20)
(define hash-assoc
  (lambda args
    (let ((hm (hash-table-copy (car args))))
      (if (pair? (cdr args))
        (apply hash-build hm (cdr args))
        (car args)))))
