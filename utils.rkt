#lang racket
(require racket/mpair lazy/force pict3d)


;;; ширина и высота инициализируемого окна игры
(current-pict3d-width 800) 
(current-pict3d-height 800) 

;;; необходимо для использования в файле main_window.rkt
(provide default-cube-scale rotate-angle coef z-step
         create-cube vec3d-ref vec3d-set! make-vec3d
         minimax
         game% interactive-player
         board x-pos o-pos x-move o-move x% o%
         free-cells wins? winning-positions
)

;;; размер кубика по умолчанию
(define default-cube-scale 0.6)
;;; угол, на который поворачиваются объекты за одно нажатие кнопок A/D
(define rotate-angle 5.0)
;;; изменение высоты, происходящее за одно нажатие клавиш Z/X
(define z-step 0.2)
;;; коэффициент приближения/удаление объктов при нажатии W/S
(define coef 0.05)

;;; структура игровой доски, аналогично шаблону tictac.rkt
(struct board (x o))
(define x-pos board-x)
(define o-pos board-o)

(define (x-move b m) (board (set-add (x-pos b) m) (o-pos b)))
(define (o-move b m) (board (x-pos b) (set-add (o-pos b) m)))


;;; --------------------------------------------------------------------
;;; функции для удобства создания игрового поля и объектов для отрисовки поля
;;; функция, создающая объект класс Pict3d (кубик)
;;; x,y,z - координаты
;;; c - цвет кубика
;;; scale-coef - размер кубика
(define (create-cube x y z c [scale-coef default-cube-scale])
    (set-color (cube (pos x y z) scale-coef) (rgba c)))

;;; геттер для vec3d 
(define (vec3d-ref v x y z)
    (vector-ref (vector-ref (vector-ref v x) y) z))

;;; сеттер для vec3d
(define (vec3d-set! v x y z value)
    (vector-set! (vector-ref (vector-ref v x) y) z value))

;;; создание vec3d
;;; size - размер трехмерного вектора
;;; вектор имеет одинаковые размерности по трем осям
;;; init-val - начальное значение для инициализации
(define (make-vec3d size [init-val 0]) 
    (define (helper size [init-val 0])
        (for/vector ((i size)) (make-vector size init-val))
    )
    (for/vector ((i size)) (helper size init-val)))
;;; --------------------------------------------------------------------
;;; минимаксный алгоритм
(define (minimax tree)
    (define (minimax-h node alpha beta max-player)
        (define (next-max x v)
            (if (or (null? x) (<= beta v)) 
                v
                (next-max (cdr x)
                      (max v (minimax-h (car x) v beta (not max-player)))))
        )
        (define (next-min x v)
            (if (or (null? x) (<= v alpha)) 
                v
                (next-min (cdr x)
                      (min v (minimax-h (car x) alpha v (not max-player)))))
        )
        (cond 
            ((number? node) node)
            ((null? node) 0.0)
            (max-player (next-max node alpha))
            (else (next-min node beta))
        )
    )
    (!(minimax-h tree -inf.0 +inf.0 #f))
)


;;; --------------------------------------------------------------------
;;; описание класса, реализующего логику и оптимальную стратегию произвольной игры 
;;; с нулевой суммой и полной информацией
(define game%
  (class object%
    (super-new)
 
    ;; виртуальные методы для задания правил игры
    (init-field my-win?         ; State -> Bool
                my-loss?        ; State -> Bool
                draw-game?      ; State -> Bool
                my-move         ; State Move -> State
                opponent-move   ; State Move -> State
                possible-moves  ; State -> (list Move)
    )
 
    ;; выбор оптимального хода по минимаксу 
    ;; из нескольких оптимальных выбирается один случайно
    (define/public ((optimal-move look-ahead) S)
        (let*   ((opt-move (!(argmax 
                              (lambda (m) (!(minimax (game-tree S m look-ahead)))) 
                              (shuffle (possible-moves S))
                            ))
                ))
                (begin
                    (displayln "\nOPTIMAL MOVE:")
                    (displayln opt-move)
                    opt-move
                )
        )
    )
 
;;; построение дерева с оценками
    (define (game-tree St m look-ahead)
	   ;; вспомогательная функция, строящая закольцованный список из пары элементов
        (define (help a b) (begin (define l (mlist a b a)) (set-mcdr! l (mcons b l)) l))
        (define (new-ply moves i s)	  
            (cond
                ((my-win? s) +inf.0) ; в выигрышной позиции оценка = + бесконечность
                ((my-loss? s) -inf.0) ; в проигрышной = - бесконечность
                ((draw-game? s)     0) ; при ничье = 0
                ((>= i look-ahead)  (f-h s)) ; если исчерпана глубина, 
                                             ; то используется эвристическая оценка 
                (else 
                    (map 
                        (lambda (x) (new-ply (mcdr moves) (+ 1 i) ((mcar moves) s x)))
                        (possible-moves s)
                    )
                ) ; рассматриваем все возможные ходы и строим их оценки
	    	)
        )
        (new-ply (help opponent-move my-move) 1 (my-move St m))
    )
 
    (define/public (make-move S move-method)
        (cond
            ((my-loss? S)   (values '() S 'loss))         
            ((draw-game? S) (values '() S 'draw))   
            (else   (let* (
                            (p1 (display "Before:\n"))
                            (t1 (display (x-pos S)))
                            (t2 (displayln (o-pos S)))
                            (m* (! (move-method S)))
                            ;;; (print-move (displayln m*))
                            (S* (my-move S m*))
                            (p3 (display "\nAfter:\n"))
                            (t3 (displayln (x-pos S*)))
                            (t4 (displayln (o-pos S*)))
                          )
                    (cond
                        ((my-win? S*)    (values m* S* 'win)) 
                        ((draw-game? S*) (values m* S* 'draw))
                        (else            (values m* S* 'next))
                    ))
            )
        )
	)
  )
)
 
;;; --------------------------------------------------------------------
;;; Реализация класса игрока
;;; Параметр `game` указывает, какая игра решается.
(define (interactive-player game)
    (class game
        (super-new)
        (inherit make-move optimal-move)
    
        (init-field name
                   [last-move 'undefined]
                   [look-ahead 4]
                   [opponent 'undefined]
                   [move-method (optimal-move look-ahead)])
 
        (define/public (get-last-move) last-move)
        (define/public (set-last-move! m) (set! last-move m))


        (define/public (your-turn S)
            (define-values (m S* status) (make-move S move-method))
            (set-last-move! m)
            (!(case status
                ['win  (list "WIN" S*)]
                ['loss (list "LOSS" S*)]
                ['draw (list "DRAW" S*)]
                [else  S*])
            )
        )
    )
)

;;; Все возможные ячейки игрового поля
(define all-cells
    (set 
        #(0 0 0) #(0 0 1) #(0 0 2) #(0 0 3)
        #(1 0 0) #(1 0 1) #(1 0 2) #(1 0 3)
        #(2 0 0) #(2 0 1) #(2 0 2) #(2 0 3)
        #(3 0 0) #(3 0 1) #(3 0 2) #(3 0 3)
        
        #(0 1 0) #(0 1 1) #(0 1 2) #(0 1 3)
        #(1 1 0) #(1 1 1) #(1 1 2) #(0 1 3)
        #(2 1 0) #(2 1 1) #(2 1 2) #(0 1 3)
        #(3 1 0) #(3 1 1) #(3 1 2) #(0 1 3)

        #(0 2 0) #(0 2 1) #(0 2 2) #(0 2 3)
        #(1 2 0) #(1 2 1) #(1 2 2) #(1 2 3)
        #(2 2 0) #(2 2 1) #(2 2 2) #(2 2 3)
        #(3 2 0) #(3 2 1) #(3 2 2) #(3 2 3)

        #(0 3 0) #(0 3 1) #(0 3 2) #(0 3 3)
        #(1 3 0) #(1 3 1) #(1 3 2) #(1 3 3)
        #(2 3 0) #(2 3 1) #(2 3 2) #(2 3 3)
        #(3 3 0) #(3 3 1) #(3 3 2) #(3 3 3)
    )
)

;; Все возможные "линии победы" игрового поля
(define winning-positions
    (list 
        ;;; Самая нижняя плоскость - 4 горизонтальных, 4 вертикальных линии и 2 диагонали
        (set #(0 0 0) #(0 1 0) #(0 2 0) #(0 3 0))
        (set #(1 0 0) #(1 1 0) #(1 2 0) #(1 3 0))
        (set #(2 0 0) #(2 1 0) #(2 2 0) #(2 3 0))
        (set #(3 0 0) #(3 1 0) #(3 2 0) #(3 3 0))

        (set #(0 0 0) #(1 0 0) #(2 0 0) #(3 0 0))
        (set #(0 1 0) #(1 1 0) #(2 1 0) #(3 1 0))
        (set #(0 2 0) #(1 2 0) #(2 2 0) #(3 2 0))
        (set #(0 3 0) #(1 3 0) #(2 3 0) #(3 3 0))

        (set #(0 0 0) #(1 1 0) #(2 2 0) #(3 3 0))
        (set #(3 0 0) #(2 1 0) #(1 2 0) #(0 3 0))

        ;;; Вторая снизу плоскость - аналогично по линиям
        (set #(0 0 1) #(0 1 1) #(0 2 1) #(0 3 1))
        (set #(1 0 1) #(1 1 1) #(1 2 1) #(1 3 1))
        (set #(2 0 1) #(2 1 1) #(2 2 1) #(2 3 1))
        (set #(3 0 1) #(3 1 1) #(3 2 1) #(3 3 1))

        (set #(0 0 1) #(1 0 1) #(2 0 1) #(3 0 1))
        (set #(0 1 1) #(1 1 1) #(2 1 1) #(3 1 1))
        (set #(0 2 1) #(1 2 1) #(2 2 1) #(3 2 1))
        (set #(0 3 1) #(1 3 1) #(2 3 1) #(3 3 1))

        (set #(0 0 1) #(1 1 1) #(2 2 1) #(3 3 1))
        (set #(3 0 1) #(2 1 1) #(1 2 1) #(0 3 1))

        ;;; Вторя сверху плоскость - снова 10 линий
        (set #(0 0 2) #(0 1 2) #(0 2 2) #(0 3 2))
        (set #(1 0 2) #(1 1 2) #(1 2 2) #(1 3 2))
        (set #(2 0 2) #(2 1 2) #(2 2 2) #(2 3 2))
        (set #(3 0 2) #(3 1 2) #(3 2 2) #(3 3 2))

        (set #(0 0 2) #(1 0 2) #(2 0 2) #(3 0 2))
        (set #(0 1 2) #(1 1 2) #(2 1 2) #(3 1 2))
        (set #(0 2 2) #(1 2 2) #(2 2 2) #(3 2 2))
        (set #(0 3 2) #(1 3 2) #(2 3 2) #(3 3 2))

        (set #(0 0 2) #(1 1 2) #(2 2 2) #(3 3 2))
        (set #(3 0 2) #(2 1 2) #(1 2 2) #(0 3 2))

        ;;; Самая верхняя плоскость 
        (set #(0 0 3) #(0 1 3) #(0 2 3) #(0 3 3))
        (set #(1 0 3) #(1 1 3) #(1 2 3) #(1 3 3))
        (set #(2 0 3) #(2 1 3) #(2 2 3) #(2 3 3))
        (set #(3 0 3) #(3 1 3) #(3 2 3) #(3 3 3))

        (set #(0 0 3) #(1 0 3) #(2 0 3) #(3 0 3))
        (set #(0 1 3) #(1 1 3) #(2 1 3) #(3 1 3))
        (set #(0 2 3) #(1 2 3) #(2 2 3) #(3 2 3))
        (set #(0 3 3) #(1 3 3) #(2 3 3) #(3 3 3))

        (set #(0 0 3) #(1 1 3) #(2 2 3) #(3 3 3))
        (set #(3 0 3) #(2 1 3) #(1 2 3) #(0 3 3))

        ;;; 16 столбцов высоты 4 
        (set #(0 0 0) #(0 0 1) #(0 0 2) #(0 0 3))
        (set #(1 0 0) #(1 0 1) #(1 0 2) #(1 0 3))
        (set #(2 0 0) #(2 0 1) #(2 0 2) #(2 0 3))
        (set #(3 0 0) #(3 0 1) #(3 0 2) #(3 0 3))
        
        (set #(0 1 0) #(0 1 1) #(0 1 2) #(0 1 3))
        (set #(1 1 0) #(1 1 1) #(1 1 2) #(0 1 3))
        (set #(2 1 0) #(2 1 1) #(2 1 2) #(0 1 3))
        (set #(3 1 0) #(3 1 1) #(3 1 2) #(0 1 3))

        (set #(0 2 0) #(0 2 1) #(0 2 2) #(0 2 3))
        (set #(1 2 0) #(1 2 1) #(1 2 2) #(1 2 3))
        (set #(2 2 0) #(2 2 1) #(2 2 2) #(2 2 3))
        (set #(3 2 0) #(3 2 1) #(3 2 2) #(3 2 3))

        (set #(0 3 0) #(0 3 1) #(0 3 2) #(0 3 3))
        (set #(1 3 0) #(1 3 1) #(1 3 2) #(1 3 3))
        (set #(2 3 0) #(2 3 1) #(2 3 2) #(2 3 3))
        (set #(3 3 0) #(3 3 1) #(3 3 2) #(3 3 3))

        ;;; 4 главных диагонали
        (set #(0 0 0) #(1 1 1) #(2 2 2) #(3 3 3))
        (set #(3 3 0) #(2 2 1) #(1 1 2) #(0 0 3))
        (set #(0 3 0) #(1 2 1) #(2 1 2) #(3 0 3))
        (set #(3 0 0) #(2 1 1) #(1 2 2) #(0 3 3))

        ;;; 16 неглавных диагоналей
        (set #(0 0 0) #(1 0 1) #(2 0 2) #(3 0 3))        
        (set #(0 1 0) #(1 1 1) #(2 1 2) #(3 1 3))        
        (set #(0 2 0) #(1 2 1) #(2 2 2) #(3 2 3))        
        (set #(0 3 0) #(1 3 1) #(2 3 2) #(3 3 3))

        (set #(3 0 0) #(2 0 1) #(1 0 2) #(0 0 3))        
        (set #(3 1 0) #(2 1 1) #(1 1 2) #(0 1 3))        
        (set #(3 2 0) #(2 2 1) #(1 2 2) #(0 2 3))        
        (set #(3 3 0) #(2 3 1) #(1 3 2) #(0 3 3))

        (set #(0 0 0) #(0 1 1) #(0 2 2) #(0 3 3))        
        (set #(1 0 0) #(1 1 1) #(1 2 2) #(1 3 3))        
        (set #(2 0 0) #(2 1 1) #(2 2 2) #(2 3 3))        
        (set #(3 0 0) #(3 1 1) #(3 2 2) #(3 3 3))

        (set #(0 3 0) #(0 2 1) #(0 1 2) #(0 0 3))        
        (set #(1 3 0) #(1 2 1) #(1 1 2) #(1 0 3))        
        (set #(2 3 0) #(2 2 1) #(2 1 2) #(2 0 3))        
        (set #(3 3 0) #(3 2 1) #(3 1 2) #(3 0 3))        
    )
)

;;; получить все свободные ячейки игрового поля
(define (free-cells b)
    (set-subtract all-cells (x-pos b) (o-pos b))
)

;; получить все ячейки игрового поля, не занятые ходами одного из игроков 
(define (open-4-cells os/xs b)
    (set-subtract all-cells (os/xs b)))

;; получить количество линий игрового поля, открытых для одного из игроков
(define (count-open-4 l)
    (foldl (lambda (x y) (if (subset? x l) (+ 1 y) y)) 0 winning-positions))
 
;; проверка, является ли ситуация на игровой доске выигрышной
(define ((wins? s) b)
    (ormap (lambda (x) (subset? x (s b))) winning-positions))

;; функция эвристической оценки позиции
;; из количества линий, открытых для крестиков, 
;; вычитается количество линий, открытых для ноликов
(define (f-h s)
    (- (count-open-4 (open-4-cells o-pos s)) (count-open-4 (open-4-cells x-pos s))))  

;;--------------------------------------------------------------------
;; макрос для описания партнеров в игре
(define-syntax-rule 
  (define-partners game (A #:win A-wins #:move A-move) 
                        (B #:win B-wins #:move B-move))
  (begin
    (define A (class game 
                (super-new 
                 [my-win?  A-wins]
                 [my-loss? B-wins]
                 [my-move  A-move]
                 [opponent-move B-move])))
    (define B (class game 
                (super-new 
                 [my-win?  B-wins]
                 [my-loss? A-wins]
                 [my-move  B-move]
                 [opponent-move A-move])))))


(define tic-tac%
    (class game%
        (super-new
            [draw-game?       (compose set-empty? free-cells)]
            [possible-moves   (compose set->list free-cells)]
        )
    )
)

;; описания партнеров для крестиков-ноликов
(define-partners tic-tac%
    (x% #:win (wins? x-pos) #:move x-move)
    (o% #:win (wins? o-pos) #:move o-move)
)

