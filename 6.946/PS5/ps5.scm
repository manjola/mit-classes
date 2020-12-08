;;;6.946 PS5 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;2.16,
(define ((T-rigid-body A B C) local)
  ((T-body A B C)
   (Euler-state->omega-body local)))

(define ((V-rigid-body gMR) local)
  (let ((theta (ref (coordinate local) 0)))
    (* gMR (cos theta))))

(define ((L-top gMR A B C) local)
  (- ((T-rigid-body A B C) local) ((V-rigid-body gMR) local)))

(se
 ((L-top 'gmR 'A 'A 'C)
  (up 't
      (up 'theta 'phi 'psi)
      (up 'thetadot 'phidot 'psidot))))
 
(define (top->state-derivative)
  (Lagrangian->state-derivative (L-top 0.112 6.96e-4 6.96e-4 1.32e-4)))

(define ((monitor-theta win) state)
  (let ((theta (ref (coordinate state) 0)))
    (plot-point win (time state) theta)))

(define plot-win (frame 0. 2. 0. :pi))

;theta is constant here 
((evolve top->state-derivative)
 (up 0.0
     (up 1.52  0. 0. )
     (up 0. 0.879 1000.))
 (monitor-theta plot-win)
 0.001
 2.0
 1.0e-14)

;;the value should be 0.848 but I only got down to 0.832
;;I couldn't quite get the values of theta and phidot so thetadot was very close to zero, 
;;just around 0.02
(/ 0.112
   (* 1.32e-4 1000.0))
