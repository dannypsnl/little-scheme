(load "stdlib.scm")

(let ((x 2) (y 3))
  (write (* x y)))
;;; result: 6

(let ((x 2) (y 3))
  (let ((foo (lambda (z) (+ x y z)))
      (x 7))
    (write (foo 4))))
;;; result: 9

(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (write (* z x))))
;;; result: 70

(letrec (
  (even?
    (lambda (n)
      (if (zero? n)
        #t
        (odd? (- n 1)))))
  (odd?
    (lambda (n)
      (if (zero? n)
        #f
        (even? (- n 1)))))
)
  (write (even? 88)))
;;; result: #t
