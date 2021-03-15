;;;6.946 PS6 Code
;;;Manushaqe Muco (manjola@mit.edu)

(load "Desktop/6.946/HW/PS6/3vector-operators")

;;3.3
(define ((H-a m g l) H-state)
  (let ((theta (coordinate H-state))
	(p (momentum H-state)))
    (- (/ (square p) (* 2 m (square l))) (* m g l (cos theta)))))

(se
 (((Hamilton-equations (H-a 'm 'g 'l))
   (literal-function 'theta) (literal-function 'p))
 't))

(define ((H-b m V) H-state)
  (let ((q (coordinate H-state))
	(p (momentum H-state)))
    (+ (/ (square p) (* 2 m))
       (V (ref q 0) (ref q 1)))))

(define (V x y)
  (+ (/ (+ (square x) (square y)) 2)
     (* (square x) y)
     (/ (cube y) -3)))

(se
 (((Hamilton-equations (H-b 'm V))
   (up (literal-function 'x) (literal-function 'y))
    (down (literal-function 'p_x) (literal-function 'p_y)))
  't))

(define ((H-c m R) H-state)
  (let ((q (coordinate H-state))
	(p (momentum H-state)))
    (/ (+ (square (ref p 0))
	  (/ (square (ref p 1))
	     (square (sin (ref q 0)))))
       (* 2 m (square R)))))
		  
(se
 (((Hamilton-equations (H-c 'm 'R))
   (up (literal-function 'theta) (literal-function 'phi))
   (down (literal-function 'p_theta) (literal-function 'p_phi)))
  't))
		 
;;Non-book Exercise		       
(define A
  (literal-function 'A
		    (-> (UP Real Real Real) (UP Real Real Real))))

(define phi
  (literal-function 'phi
		    (-> (UP Real Real Real) Real)))
      
(define ((L-em c m q) s)
  (let ((xyz (coordinate s))
	(xyzdot (velocity s)))
    (- (* 1/2 m (square xyzdot))
       (* q
	  (- (phi xyz)
	     (dot-product (/ xyzdot c)
			  (A xyz)))))))
(se
 (((Lagrange-equations (L-em 'c 'm 'q))
   (up (literal-function 'x)
       (literal-function 'y)
       (literal-function 'z)))
  't))

(define ((Lorenz c q) s)
  (let ((E (- (Grad phi)))
	(B (Curl A)))
    (* q 
       (+ (E (coordinate s))
	  (cross-product (/ (velocity s) c)
			 (B (coordinate s)))))))

(se ((Lorenz 'c 'q) (up 't (up 'x 'y 'z) (up 'xdot 'ydot 'zdot))))

(define (Raise d)
  (up (ref d 0) (ref d 1) (ref d 2)))

(let ((s (up 't (up 'x 'y 'z) (down 'p_x 'p_y 'p_z))))
  (let ((xyz (coordinates s))
	(pxyz (momenta s)))
    (- ((Lagrangian->Hamiltonian (L-em 'c 'm 'q)) s)
       (+ (/ (square (- (Raise pxyz)
			(* (/ 'q 'c) (A xyz))))
	     (* 2 'm))
	  (* 'q (phi xyz))))))




