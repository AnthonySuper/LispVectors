(define pi 3.14159265)
    
; Get the X component of a magnitude vector.
(define (vector-x vec)
 (* (cos (cdr vec)) (car vec)))

; Get the Y component of a magnitude vector.
(define (vector-y vec)
 (* (sin (cdr vec)) (car vec)))

; Change a Magnitude/Angle vector to a Component Vector
(define (xy-vector vec)
 (cons (vector-x vec) (vector-y vec)))

; Add two XY vectors together
(define (add-xy-vectors veca vecb)
 (cons
  (+ (car veca) (car vecb))
  (+ (cdr veca) (cdr vecb))))

; Change degrees to radians
(define (make-rad x)
 (* (/ pi 180) x))

; Change Radians to degrees
(define (make-deg x)
 (* (/ 180 pi) x))

; Change a magnitude/radian vector to a magnitdue/Degree vector
(define (to-deg-vec vec)
 (cons (car vec) (make-deg (cdr vec))))

; Change a degree vector to a radian vector
(define (to-rad-vec vec)
 (cons (car vec) (make-rad (cdr vec))))
; Gets the hypotnuse of a triangle. 
(define (get-hyp tri)
 (sqrt (+
		(* (car tri) (car tri)) 
		(* (cdr tri) (cdr tri)))))
 
; Changes a component vector to the equivalent mag/Rad vector
(define (comp-to-magrad vec)
 (cons
  (get-hyp vec)
  (if (<=  (car vec) 0)
   (+ pi  (atan (/
                   (cdr vec)
                   (car vec))))
   (atan (/
            (cdr vec)
            (car vec))))))

; Adds two magnatude/radian vectors
(define (add-magrad-vecs veca vecb)
 (comp-to-magrad (add-xy-vectors (xy-vector veca) (xy-vector vecb))))

; Adds two magnatude/degree vectors
(define (add-magdeg-vecs veca vecb)
 (to-deg-vec(
  add-magrad-vecs (to-rad-vec veca) (to-rad-vec vecb))))


