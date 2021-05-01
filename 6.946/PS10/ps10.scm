;;;6.946 PS10 Code
;;;Manushaqe Muco (manjola@mit.edu)


;;given pset code
(define (HToda s)
  (let ((q (coordinate s)) (p (momentum s)))
    (let ((q1 (ref q 0)) (q2 (ref q 1)) (q3 (ref q 2))
			 (p1 (ref p 0)) (p2 (ref p 1)) (p3 (ref p 2)))
      (+ (* 4 (+ (square p1) (square p2) (square p3)))
	 (* 1/24 (+ (exp (- q1 q2))
		    (exp (- q2 q3))
		    (exp (- q3 q1))))))))

(define (I2 s)
  (let ((p (momentum s)))
    (let ((p1 (ref p 0))(p2 (ref p 1))(p3 (ref p 2)))
      (+ p1 p2 p3))))

(define (I3 s)
  (let ((q (coordinate s))(p (momentum s)))
    (let ((q1 (ref q 0))
	  (q2 (ref q 1))
	  (q3 (ref q 2))
	  (p1 (ref p 0))
	  (p2 (ref p 1))
	  (p3 (ref p 2)))
      (- (+ (* (- (* 2 p2) (+ p1 p3)) (exp (* (- q3 q1))))
	    (* (- (* 2 p3) (+ p1 p2)) (exp (* (- q1 q2))))
	    (* (- (* 2 p1) (+ p2 p3)) (exp (* (- q2 q3)))))
	 (* 3 (+ (cube (* 4/3 (- (* 2 p2) (+ p1 p3))))
		 (cube (* 4/3 (- (* 2 p3) (+ p1 p2))))
		 (cube (* 4/3 (- (* 2 p1) (+ p2 p3))))))))))

(define (F s)
  (let ((q (coordinate s)))
    (let ((x (ref q 0)) (y (ref q 1)) (cm (ref q 2)))
      (up (+ (* (* 2/3 'root-3 'root-2) cm)
	     (* 2/3 'root-3 x)(* 2 y))
	  (+ (* (* 2/3 'root-3 'root-2) cm)
	     (* -4/3 'root-3 x))
	  (+ (* (* 2/3 'root-3 'root-2) cm)
	     (* 2/3 'root-3 x)
	     (* -2 y))))))


;;I2 and I3 are conserved quantities
((Poisson-bracket (Poisson-bracket I2 I3) HToda)
 (up 't (up 'q1 'q2 'q3) (down 'p1 'p2 'p3)))
#| 0 |#


;;H', I3', I3' conserved still
(define HToda-prime
  (compose HToda (F->CH F)))

(print-expression
 (HToda-prime
  (up 't (up 'x 'y 'cm) (down 'p_x 'p_y 'p_cm))))

(define I3-prime (compose I3 (F->CH F)))

(print-expression
 (I3-prime
  (up 't (up 'x 'y 'cm) (down 'p_x 'p_y 'p_cm))))

;;I3-prime is conserved
((Poisson-bracket I3-prime HToda-prime) (up 't (up 'x 'y 'cm) (down 'p_x 'p_y 'p_cm)))
#| 0 |#


		   
     

