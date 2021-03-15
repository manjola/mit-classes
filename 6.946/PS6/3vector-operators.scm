;;; Traditional Grad, Div, Curl, Lap in 3d rectangular coords

(define Grad
  (make-operator
   (lambda (f)				; function on 3d space
     (vector ((partial 0) f) ((partial 1) f) ((partial 2) f)))
   'Grad))

(define Div
  (make-operator
   (lambda (f3)				;3d vector of functions on 3d space
     (g:+ ((partial 0) (g:ref f3 0))
	  ((partial 1) (g:ref f3 1))
	  ((partial 2) (g:ref f3 2))))
   'Div))

(define Curl
  (make-operator
   (lambda (f3)				;3d vector of functions on 3d space
     (let ((Dx (partial 0)) (Dy (partial 1)) (Dz (partial 2))
	   (fx (g:ref f3 0)) (fy (g:ref f3 1)) (fz (g:ref f3 2)))
       (vector (g:- (Dy fz) (Dz fy))
	       (g:- (Dz fx) (Dx fz))
	       (g:- (Dx fy) (Dy fx)))) )
   'Curl))

(define Lap
  (make-operator
   (lambda (f)				; function on 3d space
     (g:+ ((expt (partial 0) 2) f)
	  ((expt (partial 1) 2) f)
	  ((expt (partial 2) 2) f)))
   'Lap))

#|
(define F (literal-function 'F (-> (UP Real Real Real) Real)))

(define A
  (literal-function 'A (-> (UP Real Real Real) (UP Real Real Real))))

((Grad F) (up 'x 'y 'z))
#|
(up (((partial 0) F) (up x y z))
    (((partial 1) F) (up x y z))
    (((partial 2) F) (up x y z)))
|#

((Div A) (up 'x 'y 'z))
#|
(+ (((partial 0) A^0) (up x y z))
   (((partial 1) A^1) (up x y z))
   (((partial 2) A^2) (up x y z)))
|#

((Curl A) (up 'x 'y 'z))
#|
(up
 (+ (((partial 1) A^2) (up x y z)) (* -1 (((partial 2) A^1) (up x y z))))
 (+ (((partial 2) A^0) (up x y z)) (* -1 (((partial 0) A^2) (up x y z))))
 (+ (((partial 0) A^1) (up x y z)) (* -1 (((partial 1) A^0) (up x y z)))))
|#

((Lap F) (up 'x 'y 'z))
#|
(+ (((partial 0) ((partial 0) F)) (up x y z))
   (((partial 1) ((partial 1) F)) (up x y z))
   (((partial 2) ((partial 2) F)) (up x y z)))
|#

;;; Vector version of Laplacian also works.
((Lap A) (up 'x 'y 'z))
#|
(up
 (+ (((partial 1) ((partial 1) A^0)) (up x y z))
    (((partial 0) ((partial 0) A^0)) (up x y z))
    (((partial 2) ((partial 2) A^0)) (up x y z)))
 (+ (((partial 2) ((partial 2) A^1)) (up x y z))
    (((partial 0) ((partial 0) A^1)) (up x y z))
    (((partial 1) ((partial 1) A^1)) (up x y z)))
 (+ (((partial 0) ((partial 0) A^2)) (up x y z))
    (((partial 1) ((partial 1) A^2)) (up x y z))
    (((partial 2) ((partial 2) A^2)) (up x y z))))
|#

;;; Identities

((Curl (Grad F)) (up 'x 'y 'z))
#| (up 0 0 0) |#


((Div (Curl A)) (up 'x 'y 'z))
#| 0 |#

((- (Div (Grad F))
    (Lap F))
 (up 'x 'y 'z))
#| 0 |#

((- (Curl (Curl A))
    (- (Grad (Div A)) (Lap A)))
 (up 'x 'y 'z))
#| (up 0 0 0) |#

(define G (literal-function 'G (-> (UP Real Real Real) Real)))

((- (Div (* F (Grad G)))
    (+ (* F (Lap G))
       (dot-product (Grad F) (Grad G))))
 (up 'x 'y 'z))
#| 0 |#

|#

