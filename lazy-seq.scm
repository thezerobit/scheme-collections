;; some scheme lazy sequences

;; adapted from:
;; http://www.shido.info/lisp/scheme_lazy_e.html

;; it's just easier to type
(define nil '())

;; returns first element of list or lazy sequence
(define first car)

;; returns the rest of of the list or lazy sequence
(define (rest ls)
  (force (cdr ls)))

;;; lazy cons
(define-syntax lazy-cons
   (syntax-rules ()
      ((_ a b) (cons a (delay b)))))

;;; lazy map
(define (lazy-map fn . lss)
  (if (memq '() lss)
      '()
    (lazy-cons (apply fn (map first lss))
               (apply lazy-map fn (map rest lss)))))

;;; lazy filter
(define (lazy-filter pred ls)
  (if (null? ls)
      '()
    (let ((obj (first ls)))
      (if (pred obj)
          (lazy-cons obj  (lazy-filter pred (rest ls)))
        (lazy-filter pred (rest ls))))))

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
        (take (- n 1) (rest coll))))))

;; lazy sequence after first n elements
(define drop
  (lambda (n coll)
    (if (= n 0)
      coll
      (drop (- n 1) (rest coll)))))

;; iterate
(define iterate
  (lambda (f v)
    (let ((vp (f v)))
      (lazy-cons
        vp
        (iterate f vp)))))

;; materializes a lazy sequence into a list
(define seq->list
  (lambda (coll)
    (if (pair? coll)
      (cons (first coll)
            (seq->list (rest coll)))
      coll)))

;; vector support

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

;; comparison
(define sequal?
  (lambda (c1 c2)
    (if (pair? c1)
      (if (pair? c2)
        (if (sequal? (first c1) (first c2))
          (sequal? (rest c1) (rest c2))
          #f)
        #f)
      (equal? c1 c2))))

