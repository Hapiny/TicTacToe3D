Что вообще за GUI?
    Для написания интерфейса использовалась библиотека pic3d (для установки raco pkg install pict3d).
    Игровое поле - это 4 на 4 на 4, поэтому изображаем в виде 3D куба со стороной в 4 кубика.
    Игровое поле можно вращать, подлетать поближе, опускать и поднимать (всё, что можно было удобно разглядывать ходы).

Как играть и делать ходы?
    Для ввода хода ничего не нужно печатать, нужно просто стрелочками (LShift LCTRL для движения вверх, вниз) выбирать кубик, куда хотим сходить.
    Этот КУРСОР легко увидеть, потому что этот кубик заметно больше других.
    Чтобы понять, кто сейчас ходит нужно посмотреть на цвет освещения.
    В игре крестики - зеленые кубики, а нолики - синие.
    Так вот если освещение зеленое, значитс сейчас ход крестиков, иначе должны ходить нолики.
    Когда кто-то побеждает, все кубики загораются его цветом, после этого окно игры следует закрыть.
    Если же ничья, то все кубики станут красными.

Как задавать игроков и ИИ?
    При запуске выводится сообщение с инструкцией о вводе игрока для кретстиков/ноликов.

Управление:
    A - поворот влево
    D - поворот вправо
    W - приблизить поле (подлететь к нему)
    S - отдалить поле (отлететь от него)
    Z - поднять игровое поле 
    X - опустить игровое поле
    Стрелочки - переместить курсор вдоль горизонтальной плоскости
    Left Shift - поднять курсор на плоскость выше
    Left Ctrl - опустить курсор на плоскость ниже
    F - сделать ход (то есть кубик, где сейчас курсор становится ходом игрока и добавляется в его набор ходов)