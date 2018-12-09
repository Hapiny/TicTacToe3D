#lang racket
;;; используемые библиотеки 
;;; чтобы установить pict3d нужно ввести команду в терминле:
;;; raco pkg install pict3d
(require pict3d pict3d/universe racket/class lazy/force "utils.rkt")
(current-pict3d-legacy? #t) 

;;; текст в заголовке окна 
(define window-name "4x4x4 Tic-Tac-Toe")

;;; игровое поле представляет собой 64 кубика, которые отрисовываются в виде 3D куба со стороной в 4 кубика
(define z-angle 45.0)       ;;; переменная, определяющая на какой угол относительно оси Z повернуты объекты игрового поля
(define current-z-pos 0.0)  ;;; переменная, определяющая высоту, на которой находится игровое поле
(define scale-coef 1.0)     ;;; переменная, определяющая размер объектов игрового поля
(define cube-distance 5)    ;;; расстояние между кубиками игрового поля

(define current-player "x")          ;;; переменная, определяющая игрока, который делает текущий ход
(define current-pos (vector 3 3 2))  ;;; переменаня, задающая изначальное положение курсора для выбора хода

(define empty-board (board (set) (set)))   ;;; 
(define game-field empty-pict3d)           ;;; объекты игрового поля (кубики типа Pict3d) (нужно для GUI)
(define lights empty-pict3d)               ;;; освещение игрового поля (GUI)
(define game-vec3d (make-vec3d 4 "white")) ;;; переменная для хранения цветов кубиков игрового поля (нужно для GUI)

;;; объект камеры, которая расположена в точке (25,25,15) и смотрит в начало координат
(define camera (basis 'camera (point-at (pos 25 25 15) origin)))

;;; функции хода игрока 
;;; m это вектор из трех чисел
;;; b текущее состояние игрового поля
(define (x-player-move b) 
    (begin 
        (vec3d-set! game-vec3d (vector-ref current-pos 0) (vector-ref current-pos 1) (vector-ref current-pos 2) "green")
        (set! current-player "o")
        (redraw-light "blue")
        current-pos
    )
)

(define (o-player-move b) 
    (begin
        (vec3d-set! game-vec3d (vector-ref current-pos 0) (vector-ref current-pos 1) (vector-ref current-pos 2) "blue")
        (set! current-player "x")
        (redraw-light "green")
        current-pos
    )
)

;;; функция для отрисовки (перерисовки) объектов игрового поля
(define (redraw-field)
    (begin 
        (set! game-field empty-pict3d) ;;; инициализируем пустым объектов (очищает от старого)
        (for ([i '(0 1 2 3)])          ;;; циклом проходимся по vec3d, который содержит цвета кубиков поля
            (for ([j '(0 1 2 3)])      ;;; и создаем новые объекты кубики с соответсвующими цветами
                (for ([k '(0 1 2 3)])  ;;; функция combine комбинирует объекты типа pict3d
                    (set! game-field (combine game-field (create-cube (* cube-distance j) 
                                                                      (* cube-distance k) 
                                                                      (* cube-distance i) 
                                                                      (vec3d-ref game-vec3d k j i )     
                                                                      ;;; если кубик- это курсор для указания хода, то отрисовываем его большим
                                                                      (if (equal? (vector k j i) current-pos) 1.5 default-cube-scale)))
                    )
                )
            )
        )
    )
)
;;; функция для отрисовки (перерисовки) света
;;; аналогично фунции выше
(define (redraw-light clr)
    (begin
        (set! lights empty-pict3d)
        (for ([i '(0 1 2 3)])
            (for ([j '(0 1 2 3)])
                (for ([k '(0 1 2 3)])
                    (set! lights (combine lights  (light (pos (+ 1.5 (* cube-distance j)) (+ 1.5 (* cube-distance k)) (+ 1.5 (* cube-distance i))) (emitted clr 2))))
                )
            )
        )
    )
)

;;; фукнция отрисовки эффекта движения курсора при помощи клавиш стрелок
(define (move-cursor new-pos)
    (begin
        ;;; если курсор вышел за пределы поля, то нужно вернуть его на противоположную сторону
        (vector-set! new-pos 0 (modulo (vector-ref new-pos 0) 4)) 
        (vector-set! new-pos 1 (modulo (vector-ref new-pos 1) 4))
        (vector-set! new-pos 2 (modulo (vector-ref new-pos 2) 4))
        (set! current-pos new-pos)
        ;;; при перерисовке current-pos (положение курсора) будет другое, поэтому другой кубик будет большим
        ;;; имитация движения курсора
        (redraw-field)           
    )
)

;;; функция, отвечающая за отрисовку объектов игрового поля
;;; примеры функции и пояснение аргументов было в документации на сайте racket.org
(define (on-draw s n t)
    (combine 
            ;;; здесь указываются какие трансформации нужно пременять к объектам
            (move-z (scale (rotate-z/center game-field z-angle) scale-coef) current-z-pos)
            (move-z (scale lights scale-coef) current-z-pos)  
            camera
    )
)

;;; функция диспетчеризации клавиш клавиатуры
;;; примеры функции и пояснение аргументов было в документации на сайте racket.org
(define (on-key s n t k)
    (begin
        ;;; (display k)
        (case k
            ;;; вращение поля от-но оси OZ
            [("a") (set! z-angle (+ z-angle rotate-angle))]
            [("d") (set! z-angle (- z-angle rotate-angle))]

            ;;; приближение поля (эффект залетания внутрь)
            [("w") (set! scale-coef (+ scale-coef coef))]
            [("s") (set! scale-coef (- scale-coef coef))]

            ;;; подъем или опускание игрового поля
            [("z") (set! current-z-pos (- current-z-pos z-step))]
            [("x") (set! current-z-pos (+ current-z-pos z-step))]

            ;;; управление курсором для ввода хода игрока
            ;;; стрелоки для хождения по плоскости 4 на 4
            ;;; клавиши left shift и left ctrl для подъема или спуска на другие плоскости
            [("right")   (move-cursor (vector (add1 (vector-ref current-pos 0)) (vector-ref current-pos 1) (vector-ref current-pos 2)))]
            [("left")    (move-cursor (vector (sub1 (vector-ref current-pos 0)) (vector-ref current-pos 1) (vector-ref current-pos 2)))]
            [("down")    (move-cursor (vector (vector-ref current-pos 0) (add1 (vector-ref current-pos 1)) (vector-ref current-pos 2)))]
            [("up")      (move-cursor (vector (vector-ref current-pos 0) (sub1 (vector-ref current-pos 1)) (vector-ref current-pos 2)))]
            [("shift")   (move-cursor (vector (vector-ref current-pos 0) (vector-ref current-pos 1) (add1 (vector-ref current-pos 2))))]
            [("control") (move-cursor (vector (vector-ref current-pos 0) (vector-ref current-pos 1) (sub1 (vector-ref current-pos 2))))]
            
            ;;; ввод хода осуществляется по нажатию клавиши F
            [("f") 
                    ;;; проверка того, что кубик не занят
                    (cond ((not (or (set-member? (x-pos empty-board) current-pos) (set-member? (o-pos empty-board) current-pos)))
                        (begin
                            (if (eq? current-player "x")   
                                ;;; если ходят крестики
                                ;;; делаем ход, а после смотрим на результат
                                (let ((move-result (send user-x your-turn empty-board)))
                                    ;;; если строка, то дальше разбирается что за строка
                                    (if (string? move-result) 
                                        (begin
                                            (case move-result
                                                ;;; если победили, то отрисовываем всё своим цветом
                                                [("WIN") (set! game-vec3d (make-vec3d 4 "green"))]
                                                [("LOSS") (set! game-vec3d (make-vec3d 4 "blue"))]
                                                ;;; если ничья, то отрисовываем всё красным
                                                [("DRAW") (set! game-vec3d (make-vec3d 4 "red"))]
                                            ) 
                                            (redraw-light "green")
                                        )
                                        ;;; если вернулся объект поля (board)
                                        (begin
                                            ;;; мутирование игрового поля
                                            (set! empty-board move-result)
                                            ;;; вывод для дебага в консоли
                                            (display "X-player moves:\n")
                                            (display (x-pos empty-board))
                                            (display "\n")
                                        )
                                    )
                                )
                                ;;; если ходят нолики
                                ;;; делаем ход, а после смотрим на результат
                                (let ((move-result (send user-o your-turn empty-board)))
                                    (if (string? move-result) 
                                        (begin
                                            (case move-result
                                                ;;; если победили, то отрисовываем всё своим цветом
                                                [("WIN") (set! game-vec3d (make-vec3d 4 "blue"))]
                                                [("LOSS") (set! game-vec3d (make-vec3d 4 "green"))]
                                                ;;; если ничья, то отрисовываем всё красным
                                                [("DRAW") (set! game-vec3d (make-vec3d 4 "red"))]
                                            ) 
                                            (redraw-light "blue")
                                        )
                                        (begin
                                            ;;; мутирование игрового поля
                                            (set! empty-board move-result)
                                            ;;; вывод для дебага в консоли
                                            (display "O-player moves:\n")
                                            (display (x-pos empty-board))
                                            (display "\n")
                                        )
                                    )
                                )
                            )
                            ;;; после изменения цвета одного кубика (в который сейчас сходили) нужно перерисовать поле
                            (redraw-field)
                        ))
                    )]         
        )
    )
)            

;;; взято из шаблрна tictak
(define user-x 
    (new (force (interactive-player x%)) 
        [name "User X"]
        [move-method x-player-move]
	)
)
(define user-o 
    (new (force (interactive-player o%)) 
        [name "User O"]
        [move-method o-player-move]
	)
 )

;;; функция старта игры
(define (start-game p1 p2)
    (set-field! opponent p1 p2)
    (set-field! opponent p2 p1)
    ;;; начальная отрисовка
    (redraw-light "green")
    (redraw-field)
    ;;; главный цикл отрисовки и создание окна игры, сюда и передаются все диспетчеризующие фунции и функции отрисовки
    (big-bang3d 0  #:name window-name #:width 1000 #:height 800 #:on-key on-key #:on-draw on-draw)
)

(! (start-game user-x user-o))

