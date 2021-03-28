;;;6.946 PS8 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;3.13
(define ((xy-map alpha) x y return failure)
  (let ((xp (- (* x (cos alpha))
	       (* (- y (square x))(sin alpha))))
	(yp (+ (* x (sin alpha))
	       (* (- y (square x)) (cos alpha)))))
    (if (or (> (abs x) 1) 
	    (> (abs y) 1))
	(failure)
	(return xp yp))))
    
(define window (frame -1 1 -1 1))

(explore-map window (xy-map 1.35) 2000)

;;More fun with standard maps
(define ((standard-map K) theta I return failure)
  (let ((nI (+ I (* K (sin theta)))))
    (return ((principal-value :2pi) (+ theta nI))
	    ((principal-value :2pi) nI))))

(define window (frame 0.0 :2pi 0.0 :2pi))

(explore-map window (standard-map 1.4) 2000)
