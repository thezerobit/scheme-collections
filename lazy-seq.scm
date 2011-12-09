;; some scheme lazy sequences

;; adapted from:
;; http://www.shido.info/lisp/scheme_lazy_e.html

;;;;; basic functions and a macro

;;; car for lazy evaluation
(define lazy-car car)

;;; cdr for lazy evaluation
(define (lazy-cdr ls)
  (force (cdr ls)))

;;; lazy cons
(define-syntax lazy-cons
   (syntax-rules ()
      ((_ a b) (cons a (delay b)))))

;;; lazy map
(define (lazy-map fn . lss)
  (if (memq '() lss)
      '()
    (lazy-cons (apply fn (map lazy-car lss))
               (apply lazy-map fn (map lazy-cdr lss)))))

;;; lazy filter
(define (lazy-filter pred ls)
  (if (null? ls)
      '()
    (let ((obj (lazy-car ls)))
      (if (pred obj)
          (lazy-cons obj  (lazy-filter pred (lazy-cdr ls)))
        (lazy-filter pred (lazy-cdr ls))))))

;;; returns n-th item of the lazy list
(define (lazy-ref ls n)
  (if (= n 0)
      (lazy-car ls)
    (lazy-ref (lazy-cdr ls) (- n 1))))

;;; returns first n items of the ls
(define (head ls n)
  (if (= n 0)
      '()
     (cons (lazy-car ls) (head (lazy-cdr ls) (- n 1)))))

;; my own additions

;; it's just easier to type
(define nil '())

;; returns a lazy sequence from a vector
;; dropping the first n elements
(define vector-drop
  (lambda (n vec)
    (if (>= 0 (- (vector-length vec) n))
      nil
      (lazy-cons
        (vector-ref vec n)
        (vector-drop (+ n 1) vec)))))

;; lazy sequence from a vector
(define vector->seq
  (lambda (vec)
    (vector-drop 0 vec)))

(define first car)

;; cdr for lists or lazy sequences
(define rest
  (lambda (coll)
    (let ((r (cdr coll)))
      (if (promise? r)
        (force r)
        r))))

;; returns the nth element of sequence
;; O(n) complexity
(define nth
  (lambda (coll n)
    (if (= n 0)
      (first coll)
      (nth (rest coll) (- n 1)))))

;; lazy sequence of first n elements of sequence
(define take
  (lambda (n coll)
    (if (= n 0)
      nil
      (lazy-cons
        (first coll)
        (take (- n 1) (lazy-cdr coll))))))

;; lazy sequence after first n elements
(define drop
  (lambda (n coll)
    (if (= n 0)
      coll
      (drop (- n 1) (lazy-cdr coll)))))
