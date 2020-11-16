;;;6.946 PS0 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;;9.2: Computing Derivative

(define (f v)

  (let ((x (ref v 0))

	(y (ref v 1)))

    (* (square x) (cube y))))



(define (g v)

  (let ((x (ref v 0))

	(y (ref v 1)))

    (up (f v) y)))



(define h (compose f g))



;;a.

(((partial 0) f) (up 'x 'y))
;; (* 2 x (expt y 3))

(((partial 1) f) (up 'x 'y))
;; (* 3 (expt x 2) (expt y 2))


;;b.

(((partial 0) f) (up (f '(x y)) 'y))
;; (* 2 (expt x 2) (expt y 6))


(((partial 1) f) (up (f '(x y)) 'y))
;; (* 3 (expt x 4) (expt y 8))
 

;;c.

(((partial 0) g) (up 'x 'y))
;; (up (* 2 x (expt y 3)) 0)


(((partial 1) g) (up 'x 'y))
;; (up (* 3 (expt x 2) (expt y 2)) 1)


;;d.

((D f) (up 'a 'b))
;; (down (* 2 a (expt b 3)) (* 3 (expt a 2) (expt b 2)))

((D g) (up 3 5))
;; (down (up 750 0) (up 675 1))

((D h) (up (* 3 (square 'a)) (* 5 (cube 'b))))
;; (down (* 210937500 (expt a 6) (expt b 27))  (* 284765625 (expt a 8) (expt b 24)))


