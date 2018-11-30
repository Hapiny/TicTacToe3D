#lang racket
(require pict3d pict3d/universe "utils.rkt")
(current-pict3d-legacy? #t) 

(define window-name "4x4x4 Tic-Tac-Toe")

(define z-angle 45.0)
(define current-z-pos 0.0)
(define scale-coef 1.0)
(define cube-distance 5)
(define current-player "x")

(struct board (x o))
(define x-pos board-x)
(define o-pos board-o)
(define empty-board (board (set) (set)))
;;; m это вектор из трех чисел
(define (x-move b m) 
    (cond ((not (or (set-member? (x-pos b) m) (set-member? (o-pos b) m)))
        (begin 
            (vec3d-set! game-vec3d (vector-ref m 0) (vector-ref m 1) (vector-ref m 2) "green")
            (set! current-player "o")
            (board (set-add (x-pos b) m) (o-pos b))
        ))
        (else b)
    )
)

(define (o-move b m) 
    (cond ((not (or (set-member? (x-pos b) m) (set-member? (o-pos b) m)))
        (begin
            (vec3d-set! game-vec3d (vector-ref m 0) (vector-ref m 1) (vector-ref m 2) "blue")
            (set! current-player "x")
            (board (x-pos b) (set-add (o-pos b) m))
        ))
        (else b)
    )
)

(define current-pos (vector 0 0 0))

(define game-field empty-pict3d)
(define lights empty-pict3d)

(define game-vec3d
    (vector
        (vector (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white")) 
        (vector (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white")) 
        (vector (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white")) 
        (vector (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white") (vector "white" "white" "white" "white")) 
    )
)

(define (redraw-field)
    (begin 
        (set! game-field empty-pict3d)
        (for ([i '(0 1 2 3)])
            (for ([j '(0 1 2 3)])
                (for ([k '(0 1 2 3)])
                    (set! game-field (combine game-field (create-cube (* cube-distance j) 
                                                                      (* cube-distance k) 
                                                                      (* cube-distance i) 
                                                                      (vec3d-ref game-vec3d k j i )
                                                                      (if (equal? (vector k j i) current-pos) 1.0 default-cube-scale)))
                    )
                )
            )
        )
    )
)


(define (redraw-light)
    (begin
        (set! lights empty-pict3d)
        (for ([i '(0 1 2 3)])
            (for ([j '(0 1 2 3)])
                (for ([k '(0 1 2 3)])
                    (set! lights (combine lights  (light (pos (+ 1.5 (* cube-distance j)) (+ 1.5 (* cube-distance k)) (+ 1.5 (* cube-distance i))) (emitted "white"))))
                )
            )
        )
    )
)

(define (move-cursor new-pos)
    (begin
        (vector-set! new-pos 0 (modulo (vector-ref new-pos 0) 4))
        (vector-set! new-pos 1 (modulo (vector-ref new-pos 1) 4))
        (vector-set! new-pos 2 (modulo (vector-ref new-pos 2) 4))
        (set! current-pos new-pos)
        (redraw-field)           
    )
)

(define camera
    (basis 'camera (point-at (pos 15 15 9) origin))
)

(define (on-draw s n t)
    (combine 
            (move-z (scale (rotate-z/center game-field z-angle) scale-coef) current-z-pos)
            (move-z (scale lights scale-coef) current-z-pos)  
            camera
    )
)

(define (on-key s n t k)
    (begin
        (case k
            [("a") (set! z-angle (+ z-angle rotate-angle))]
            [("d") (set! z-angle (- z-angle rotate-angle))]

            [("w") (set! scale-coef (+ scale-coef coef))]
            [("s") (set! scale-coef (- scale-coef coef))]

            [("z") (set! current-z-pos (- current-z-pos z-step))]
            [("x") (set! current-z-pos (+ current-z-pos z-step))]

            [("right")   (move-cursor (vector (add1 (vector-ref current-pos 0)) (vector-ref current-pos 1) (vector-ref current-pos 2)))]
            [("left")    (move-cursor (vector (sub1 (vector-ref current-pos 0)) (vector-ref current-pos 1) (vector-ref current-pos 2)))]
            [("down")    (move-cursor (vector (vector-ref current-pos 0) (add1 (vector-ref current-pos 1)) (vector-ref current-pos 2)))]
            [("up")      (move-cursor (vector (vector-ref current-pos 0) (sub1 (vector-ref current-pos 1)) (vector-ref current-pos 2)))]
            [("shift")   (move-cursor (vector (vector-ref current-pos 0) (vector-ref current-pos 1) (add1 (vector-ref current-pos 2))))]
            [("control") (move-cursor (vector (vector-ref current-pos 0) (vector-ref current-pos 1) (sub1 (vector-ref current-pos 2))))]
            [("f") (begin
                        (if (eq? current-player "x")   
                            (set! empty-board (x-move empty-board current-pos))
                            (set! empty-board (o-move empty-board current-pos))
                        )
                        (redraw-field)
                   )]         
        )
    )
)            

(redraw-light)
(redraw-field)
(big-bang3d 0  #:name window-name #:width 1000 #:height 800 #:on-key on-key #:on-draw on-draw)
