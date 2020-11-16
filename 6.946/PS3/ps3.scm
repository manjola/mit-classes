;;;6.946 PS3 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;1.30
(define ((T m) local)
  (let ((q (coordinate local))
	(v (velocity local)))
    (let ((r (ref  q 0)) (theta (ref q 1))
	  (rdot (ref v 0)) (thetadot (ref v 1)))
      (* 1/2  m 
	 (+ (square rdot) (square (* r thetadot)))))))

(define ((U alpha beta) local)
  (let ((q (coordinate local)))
    (let ((r (ref q 0)))
      (- (* beta (expt r alpha))))))

(define (L-central-potential m alpha beta)
  (- (T m) (U alpha beta)))

(se 
 ((L-central-potential 'm 'alpha 'beta)
  (up 't (up 'r 'theta) (up 'rdot 'thetadot))))

(define (central-potential-state-derivative m alpha beta)
  (Lagrangian->state-derivative (L-central-potential m alpha beta)))

(se
 ((central-potential-state-derivative 'm 'alpha 'beta)
  (up 't (up 'r 'theta) (up 'rdot 'thetadot))))

(define ((monitor win) state)
  (let ((r (ref (coordinate  state) 0)) (theta (ref (coordinate state) 1)))
    (let ((x (* r (sin theta)))
	  (y (* r (cos theta))))
    (plot-point win x y))))

(define plot-win (frame -7 7 -7 7))

((evolve central-potential-state-derivative 1.0 1/4 -1.0)
 (up 0.0  (up 1.0 .0) (up 1.0 0.5))
 (monitor  plot-win)
0.01
200
1.0e-13) 

;;1.36
(define ((L-free m) state)
  (let ((q (coordinate state))
	(v (velocity state)))
    (* 1/2 m (square v))))

(define ((ellipsoid-coord a b) state)
  (let ((q (coordinate state)))
    (let ((theta (ref q 0))
	  (phi (ref q 1)))
      (let ((x (* a (sin theta) (cos phi)))
	    (y (* b (sin theta) (sin phi)))
	    (z (* b (cos theta))))
	(up x y z)))))

(define (L-ellipsoidal m a b)
  (compose (L-free m)
	   (F->C (ellipsoid-coord a b))))

(se
 ((L-ellipsoidal 'm 'a 'b)
  (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))

;;rotate about x-axis
(define (rotate-x s)
  (compose (Rx s) coordinate))

(define (L-rot m s) 
  (compose (L-free m) (F->C (rotate-x s))))

(se
 ((L-rot 'm 's) 
  (up 't (up 'x 'y 'z) (up 'xdot 'ydot 'zdot))))
;;same Lagrangian as L-free

(define (L-ellipsoidal-rot m a b s)
  (compose (L-rot m s)
	   (F->C (ellipsoid-coord a b))))

(se
 ((L-ellipsoidal-rot 'm 'a 'b 's)
  (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))
;;same Lagrangian as L-ellipsoidal 

(define the-Noether-integral
  (let ((L (L-free 'm)))
    (* ((partial 2) L) ((D rotate-x) 0))))

(define (Noether-angle a b)
  (compose
   the-Noether-integral (F->C (ellipsoid-coord a b))))

(se 
 ((Noether-angle 'a 'b)
 (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))

(se
 (the-Noether-integral
 (up 't (up 'x 'y 'z) (up 'xdot 'ydot 'zdot))))

;;Velocity-dependence continued
;ported over from pset2
(define ((rotate Omega) s)
  (let ((t (time s))
	(q (coordinate s)))
    (let ((x (ref q 0))
	  (y (ref q 1)))
      (up (- (* (cos (* Omega t)) x)
	     (* (sin (* Omega t)) y))
	  (+ (* (cos (* Omega t)) y)
	     (* (sin (* Omega t)) x))))))
		  
(define ((Lfree m) s)
  (let ((t (time s))
	(q (coordinate s))
	(v (velocity s)))
    (* 1/2 m 
       (dot-product v v))))

(define (Lrot m Omega)
  (compose (Lfree m)(F->C (rotate Omega))))

(define (V s)
  (let ((q (coordinate s)))
    ((literal-function 'V_r 
		      (-> (UP Real Real)  Real)) q)))
   
;;rotate by -omega
(define (Vfree m Omega)
  (compose V (F->C (rotate (- Omega)))))

(se 
 ((Vfree 'm 'Omega)
  (up 't (up 'x 'y) (up 'xdot 'ydot))))

(define (Lfree-new m Omega) (- (Lfree m) (Vfree m Omega)))

(se 
 ((Lfree-new 'm  'Omega)
  (up 't (up 'x 'y) (up 'xdot 'ydot))))

;;1.
(define (L-rot-rect m Omega)
  (compose (Lfree-new m Omega) (F->C (rotate Omega))))

(se
 ((L-rot-rect 'm 'Omega)
  (up 't (up 'x_r 'y_r) (up 'xdot_r 'ydot_r))))

;;2.
(se
 ((Lagrangian->energy (L-rot-rect 'm 'Omega))
  (up 't (up 'x_r 'y_r) (up 'xdot_r 'ydot_r))))


