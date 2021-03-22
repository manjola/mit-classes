;;;6.946 PS7 Code
;;;Manushaqe Muco (manjola@mit.edu)


;;An effective Lagrangian
(define ((L-axisymmetric-top A C gMR) local)
  (let ((q (coordinate local)) (qdot (velocity local)))
    (let ((theta (ref q 0)) (thetadot (ref qdot 0))
			    (phidot (ref qdot 1)) (psidot (ref qdot 2)))
      (+ (* 1/2 A
	    (+ (square thetadot) (square (* phidot (sin theta)))))
	 (* 1/2 C
	    (square (+ psidot (* phidot (cos theta)))))
	 (* -1 gMR (cos theta))))))

(define ((Lagrangian->Routhian Lagrangian) R-state)
  (let ((t (time R-state))
	(q (coordinate R-state))
	(vp (ref R-state 2)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (vx (ref vp 0))
	  (py (ref vp 1)))
      (define (L vy)
	(Lagrangian (up t q (up vx vy))))
      ((Legendre-transform-procedure L) py))))

;;Part 1
(define ((newcoordinates) local)
  (let ((xy (coordinate local)))
    (let ((x (ref xy 0))
	  (y (ref xy 1)))
      (let ((theta x)
	     (phi (ref y 0))
	     (psi (ref y 1)))
	(up theta phi psi)))))

(define (L A C gMR)
  (compose (L-axisymmetric-top A C gMR)
	   (F->C (newcoordinates))))
(se
 ((L 'A 'C 'gMR)
  (up 't (up 'theta (up 'phi 'psi)) (up 'thetadot (up 'phidot 'psidot)))))

(se
 ((Lagrangian->Routhian (L 'A 'C 'gMR))
  (up 't (up 'theta (up 'phi 'psi)) (up 'thetadot (down 'p_phi 'p_psi)))))

;;Part 2 
(define (((Routh-equations Routhian) x y py) t)
  (define (L s)
    (let ((tau (time s))
	  (q (coordinate s))
	  (v (velocity s)))
      (Routhian (up tau (up q (y tau)) (up v (py tau))))))
  (define (H s)
    (let ((tau (time s))
	  (q (coordinate s))
	  (p (momentum s)))
      (Routhian (up tau (up (x tau) q) (up ((D x) tau) p)))))
  (up
   (((Lagrange-equations L) x) t)
   (((Hamilton-equations H) y py) t)))

(se
 (((Routh-equations (Lagrangian->Routhian (L 'A 'C 'gMR)))
   (literal-function 'theta)
   (up (literal-function 'phi) (literal-function 'psi))
   (down (literal-function 'p_phi) (literal-function 'p_psi))) 't))


;;Horizontally-driven pendulum
(define ((L-free m g) local)
  (let ((q (coordinate local))
	(v (velocity local)))
    (let ((y (ref q 1)))
      (- (* 1/2 m (square v)) (* m g y)))))

(define ((dp-coordinates l x_s) local)
  (let ((t (time local))
	(theta (coordinate local)))
    (let ((x (+ (x_s t) (* l (sin theta))))
	  (y (- (* l (cos theta)))))
      (up x y))))

(define (L-horizontal m l g x_s)
  (compose (L-free m g)
	   (F->C (dp-coordinates l x_s))))n
	 
(define ((periodic-drive a omega phi) t)
  (* a (cos (+ (* omega t) phi))))

(define (L-horizontal-driven m l g a omega)
  (L-horizontal m l g (periodic-drive a omega 0)))

(se
 (((Lagrange-equations 
    (L-horizontal-driven 'm 'l 'g 'A 'omega))
   (literal-function 'theta))
 't))

(define (H-pend-sysder m l g a omega)
  (Hamiltonian->state-derivative
   (Lagrangian->Hamiltonian
    (L-horizontal-driven m l g a omega))))

(define (driven-pendulum-map m l g A omega)
  (let ((advance (state-advancer H-pend-sysder m l g A omega))
	(map-period (/ :2pi omega)))
    (lambda (theta ptheta return fail)
      (let ((ns (advance
		 (up 0 theta ptheta)
		 map-period)))
	(return ((principal-value :pi)
		 (coordinate ns))
		(momentum ns))))))

(define win (frame :-pi :pi -20 20))

(let ((m 1.)
     (l 1.)
     (g 9.8)
     (A .4))
  (let ((omega0 (sqrt (/ g l))))
    (let ((omega (* 10 omega0)))
      (explore-map
       win
       (driven-pendulum-map m l g A omega)
       1000))))
