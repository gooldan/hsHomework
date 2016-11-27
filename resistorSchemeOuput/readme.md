# Output Resistor home task

Для типа Scheme определите функцию show так, чтобы она выводила схему наглядно, с помощью резисторов, соединенных линиями. Как то так
S(P (R 3) (R 5)) (R 7):

..+-- 3 --+
..|.......|
--+.......+-- 7 --
..|.......|
..+-- 5 --+

аккуратный рисунок: http://msimuni.wikidot.com/scheme )

тип Scheme - data Scheme a = R Int | S (Scheme a) (Scheme a) | P (Scheme a) (Scheme a)
