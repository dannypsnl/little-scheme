(write (cond ((> 3 2) 'greater)
  ((< 3 2) 'less)))
;;; result: greater

(write (case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite)))
;;; result: composite
;;; use else clause
(write (case (* 2 3)
  ((2 3 5 7) 'prime)
  (else 'else-is-composite)))
