;;;Project 3 Code
;;;Manushaqe Muco (manjola@mit.edu)


;;A.
;;setting up the pendulum
(define ((L-uniform-acceleration m g) local)
  (let* ((q (coordinate local))(v (velocity local))(y (ref q 1)))
    (- (* 1/2 m (square v)) (* m g y))))

(define ((dp-coordinates l y_s) local)
  (let* ((t (time local))
	 (theta (coordinate local))
	 (x (* l (sin theta)))
	 (y (- (y_s t) (* l (cos theta)))))
    (up x y)))

(define (L-pend m l g y_s)
  (compose (L-uniform-acceleration m g)
	   (F->C (dp-coordinates l y_s))))

(define ((periodic-drive amplitude frequency phase) t)
  (* amplitude (cos (+ (* frequency t) phase))))

(define (L-periodically-driven-pendulum m l g a omega)
  (let ((ys (periodic-drive a omega 0)))(L-pend m l g ys)))

(define (H-pend-sysder m l g a omega)
  (Hamiltonian->state-derivative
   (Lagrangian->Hamiltonian
    (L-periodically-driven-pendulum m l g a omega))))

(define (driven-pendulum-map m l g A omega)
  (let ((advance (state-advancer H-pend-sysder m l g A omega))
	(map-period (/ :2pi omega)))
    (lambda (theta ptheta return fail)
      (let ((ns (advance(up 0 theta ptheta)
			map-period)))
	(return ((principal-value :pi)
		 (coordinate ns))(momentum ns))))))

;;for given (A, omega) finds if it results in a stable inverted equilibrium
(define ((vert-stable? m l g A omega) theta ptheta n)
  (let* ((next ((driven-pendulum-map m l g A omega) theta ptheta cons #f))
	 (ntheta (car next))
	 (nptheta (cdr next)))
    (if (< (abs nptheta) (* 1.1 0.01)) 
	(if (= n 0)
	    #t
	    ((vert-stable? m l g A omega) ntheta nptheta (- n 1)))
	#f)))

(define win (frame 0 5 0 1 600 600))

(do ((A 0.01 (+ A 0.01)))
    ((> A 1))
  (do ((omega 0.05 (+ omega 0.05)))
      ((> omega 5))
    (if ((vert-stable? 1 1 9.8 A (* omega (sqrt 9.8))) :pi 0.01 100)
	(plot-point win omega A))))
    

;;B.
(define win (frame 1.3 2.1 0 200))

(define collected-points (list))

(define (save-point point)
  (let ((t (time point))
	(theta (coordinate point))
	(p (momentum point)))
    (set! collected-points
	  (append collected-points
		  (list (list t theta p))))))

;;following two functions to get the average of highest and lowest momenta
(define (get-center A w p)  ;;start with a guess momentum p
  (let ()
    (set! collected-points (list))
    ((evolve H-pend-sysder 1 9.8 9.8 A w)
     (up 0 pi p)          ;;change to (0 :pi p) for unstable equilibrium
     save-point 
     (/ :2pi w) 
     (* 50 :2pi) 1.0e-12)
    (extract-center-helper collected-points)))

(define (extract-center-helper cp)
  (let ((pts (filter (lambda (x) (> x 0)) (map caddr cp))))
    (/ (+ (fold-right max 0 pts) (fold-right min 10000 pts)) 2.0)))

(do ((omega 1 (+ omega 0.001)))
    ((> omega 2.1))
  (plot-point win omega (get-center 0.4 omega 10)))
 ;;guess p cannot be too large or we end up in the chaotic region
   

;;C
(define (get-square-list xstart xend xdelta ystart yend ydelta)
  (if (>= xstart xend)
      (get-line-set-x ystart yend ydelta xstart)
      (append (get-line-set-x ystart yend ydelta xstart)
	      (get-square-list (+ xstart xdelta) xend xdelta ystart yend ydelta))))

(define (get-line-set-x ystart yend ydelta x)
  (glsx-helper ystart yend ydelta x))

(define (glsx-helper cury endy stepy x)
  (if (> (+ cury stepy) endy)
      (list (list x cury))
      (cons (list x cury) 
	    (glsx-helper (+ cury stepy) endy stepy x))))

;;generate max and min momentum values for a trajectory that starts around (:-pi 0)
(define (max-min-p-from-separatrix A w)
  (let ()
    (set! collected-points (list))
    ((evolve H-pend-sysder 1 9.8 9.8 A w)
     (up 0 (+ :-pi .01) 0) save-point (/ :2pi w) (* 250 :2pi) 1.0e-12)
    (extract-max-and-min-p collected-points)))

(define (extract-max-and-min-p cp)
  (let ((pts (map caddr cp)))
    (list (fold-right max 0 pts) (fold-right min 10000 pts))))

(define win (frame .1 2 4 10))

(map
 (lambda (x) (plot-point win (caar x) (cadar x)))
 (filter (lambda (x) (> (caadr x) 250))
	(map 
	 (lambda (x) (list x (max-min-p-from-separatrix (car x) (cadr x))))
	 (get-square-list .1 2 .05 4 10 .2))))
;;takes a bit to compute

























