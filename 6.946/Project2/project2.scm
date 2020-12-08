;;;Project 2 Code
;;;Manushaqe Muco (manjola@mit.edu)

;;A.
;;define state derivative explicitly
;;state: (tau, theta, thetadot, f) -> state-deriv: (1, thetadot, D^2theta, fdot)
(define ((mercury-state-deriv e epsilon) state)
  (let ((theta (ref state 1))
	(thetadot (ref state 2))
	(f (ref state 3)))
    (let ((aoverR (/ (+ 1 (* e (cos f))) (- 1 (square e)))))
      (up 1
	  thetadot
	  (* -1/2 
	     (square epsilon)
	     (cube aoverR)
	     (sin (* 2 (- theta f))))
	  (* (sqrt (- 1 (square e)))
	     (square aoverR))))))

;;B.
(define plot-win (frame 0. 2000. :-pi pi))

(define ((monitor-diff win) state)
  (let* ((tau (ref state 0))
	 (theta (ref state 1)))
    (plot-point win tau ((principal-value :pi) (- theta (* 1.5 tau))))))

((evolve mercury-state-deriv 0.2 0.026)
 (up 0.0 0.0 1.51 0.0)    ;;Init conds
 (monitor-diff plot-win)
 0.25    ;;step size
 2000.0    ;;final tau
 1.0e-14)

;;C.
((evolve mercury-state-deriv 0.2 0.026)
 (up 0.0 0.0 1.52093252  0.0)    ;;Init conds
 (monitor-diff plot-win)
 0.25    ;;step size
 2000.0    ;;final tau
 1.0e-14)

((evolve mercury-state-deriv 0.2 0.026)
 (up 0.0 0.0 1.47937647 0.0)    ;;Init conds
 (monitor-diff plot-win)
 0.25    ;;step size
 2000.0    ;;final tau
 1.0e-14)

