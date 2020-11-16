;;;6.946 PS1 Code
;;;Manushaqe Muco (manjola@mit.edu)


;;;1.5 Solution process 
(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
	(v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(define win2 (frame 0.0 :pi/2 0.0 1.2))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
	intermediate-qs)
     (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
	;; display path
	(graphics-clear win2)
	(plot-function win2 path t0 t1 (/ (- t1 t0) 100))
	;; compute action
	(Lagrangian-action Lagrangian path t0 t1)))

(find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 2)


;;;1.8 Implementation of delta
;;;a.
(define (((delta eta) f) q)
  (define (g eps) (f (+ q (* eps eta))))
    ((D g) 0))
	
;;;b.
(define (f q)
  (compose (literal-function 'F
			     (-> (UP Real (UP* Real) (UP* Real)) Real))
(Gamma q)))

(define (g q)
  (compose
   (literal-function 'G
		     (-> (UP Real (UP* Real) (UP* Real)) Real))
(Gamma q)))

(define q (literal-function 'q))

(define eta (literal-function 'eta))

(define F (literal-function 'F))

(define (h q) (compose F (g q)))

;1.23
(- ((((delta eta) (* f g)) q) 't) 
   (+ (* ((((delta eta) f) q) 't) ((g q) 't)) 
      (* ((f q) 't) ((((delta eta) g) q) 't))))
;;;0 

;1.24
(- ((((delta eta) (+ f g)) q) 't)
   (+ ((((delta eta) f) q) 't) ((((delta eta) g) q) 't)))
;;;0

;1.25
(- ((((delta eta) (* 'c f)) q) 't)
   (* 'c ((((delta eta) f) q) 't)))
;;;0
  
;1.26
(- ((((delta eta) h) q) 't)
   (* ((compose (D F) (g q)) 't) ((((delta eta) g) q) 't)))
;;;0

;1.27
(define (g q) (D (f q)))

(- ((D (((delta eta) f) q)) 't)
   ((((delta eta) g) q) 't))
;;;0


;;;1.9 Langrange's Equations
;;a.
(define ((L-pendulum m g l) local)
  (let ((theta (coordinate local))
	(thetadot (velocity local)))
    (+ (* 0.5 m l l thetadot thetadot)
       (* m g l (cos theta)))))

(se
 (((Lagrange-equations (L-pendulum 'm 'g 'l))
   (literal-function 'theta))
   't))

;;b.
(define q
  (up (literal-function 'x)
      (literal-function 'y)))

(define (potential x y)
  (+ (* 0.5 (+ (* x x) (* y y)))
     (* x x y)
     (- (* 1/3 y y y))))

(define ((L-particle m V) local)
  (let ((q (coordinate local))
	(qdot (velocity local)))
    (let ((x (ref q 0)) (y (ref q 1))
	  (vx (ref qdot 0)) (vy (ref qdot 1)))
      (- (* 0.5 m (+ (* vx vx) (* vy vy)))
	 (V x y)))))
			
(se
 (((Lagrange-equations (L-particle 'm potential)) q) 't))


;;c.
(define ((L-sphere m R) local)
  (let ((q (coordinate local)) (qdot (velocity local)))
    (let ((theta (ref q 0)) (phi (ref q 1))
	  (alpha (ref qdot 0)) (beta (ref qdot 1)))
      (* 0.5 m R R 
	 (+ (square alpha) (square (* beta (sin theta))))))))

(se 
 (((Lagrange-equations (L-sphere 'm 'R))
(up (literal-function 'theta) (literal-function 'varphi))) 't))
