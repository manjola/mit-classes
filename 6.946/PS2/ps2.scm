;;;6.946 PS2 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;1.16
(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
      (- (* 1/2 m (square v))
       (U (sqrt (square q))))))  

(se
 (((Lagrange-equations
    (L-central-rectangular 'm (literal-function 'U)))
   (up (literal-function 'x)
       (literal-function 'y)
       (literal-function 'z)))
   't))

(define (p->r local)
  (let ((spherical-tuple (coordinate local)))
    (let ((r (ref spherical-tuple 0))
	  (theta (ref spherical-tuple 1))
	  (phi (ref spherical-tuple 2)))
      (let ((x (* r (cos phi) (sin theta)))
	    (y (* r (sin phi) (sin theta)))
	    (z (* r (cos theta))))
(up x y z)))))

(define (L-central-spherical m U)
  (compose (L-central-rectangular m U) (F->C p->r)))

(se
 (((Lagrange-equations
    (L-central-spherical 'm (literal-function 'U)))
   (up (literal-function 'r)
       (literal-function 'theta)
       (literal-function 'phi)))
  't))


;;Velocity Dependence 
(define ((rotate Omega) s)
  (let ((t (time s))
	(q (coordinate s)))
    (let ((x (ref q 0))
	  (y (ref q 1)))
      (up (- (* (cos (* Omega t)) x)
	     (* (sin (* Omega t)) y))
	  (+ (* (cos (* Omega t)) y)
	     (* (sin (* Omega t)) x))))))

;;the inverse of rotate matrix 
(define ((derotate Omega) s)
  (let ((t (time s))
	(q (coordinate s)))
    (let ((x (ref q 0))
	  (y (ref q 1)))
      (up (+ (* (cos (* Omega t)) x)
	     (* (sin (* Omega t)) y))
	  (- (* (cos (* Omega t)) y)
	     (* (sin (* Omega t)) x))))))
		  
(define ((Lfree m) s)
  (let ((t (time s))
	(q (coordinate s))
	(v (velocity s)))
    (* 1/2 m 
       (dot-product v v))))

(define (Lrot m Omega)
  (compose (Lfree m)(F->C (rotate Omega))))

(se 
 ((Lrot 'm 'Omega)
  (up 't (up 'xprime 'yprime) (up 'xprimedot 'yprimedot))))

(se 
 (((Lagrange-equations (Lrot 'm 'Omega))
   (up (literal-function 'xprime)
       (literal-function 'yprime))) 't))

(define (V s)
  (let ((q (coordinate s)))
    ((literal-function 'Vprime 
		      (-> (UP Real Real)  Real)) q)))
   
(define (Vfree m Omega)
  (compose V (F->C (derotate Omega))))

(se 
 ((Vfree 'm 'Omega)
  (up 't (up 'x 'y) (up 'xdot 'ydot))))

(define (Lfree-new m Omega) (- (Lfree m) (Vfree m Omega)))

(se 
 ((Lfree-new 'm  'Omega)
  (up 't (up 'x 'y) (up 'xdot 'ydot))))

(se 
 (((Lagrange-equations 
   (Lfree-new 'm 'Omega))
   (up (literal-function 'x)(literal-function 'y))) 't))
