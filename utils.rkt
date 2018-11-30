#lang racket
(require pict3d)

;;; Ширина и высота инициализируемого окна игры
(current-pict3d-width 800) 
(current-pict3d-height 800) 

(provide cube-scale
         rotate-angle
         coef
         z-step
         create-cube
         vec3d-ref
         vec3d-set!
)

;;; 
(define cube-scale 0.6)
(define rotate-angle 5.0)

(define z-step 0.2)

(define coef 0.1)

(define (create-cube x y z c)
    (set-color (cube (pos x y z) cube-scale) (rgba c))
)

(define (vec3d-ref v x y z)
    (vector-ref (vector-ref (vector-ref v x) y) z)
)

(define (vec3d-set! v x y z value)
    (vector-set! (vector-ref (vector-ref v x) y) z value)
)