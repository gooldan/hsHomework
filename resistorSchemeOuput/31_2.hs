{--}
numWidth 0 res =res
numWidth a res= numWidth (a`div` 10) (res+1)
--ширина элемента схемы
calcWidth (R a)=  2+ numWidth a 0
calcWidth (S a b)=15 + calcWidth a + calcWidth b
calcWidth (P a b) = 10 + max(calcWidth a) (calcWidth b)
--высота элемента схемы
calcHeight (R a)=  1
calcHeight (S a b)= max (calcHeight a) (calcHeight b)
calcHeight (P a b) = calcTopWallCount (P a b) + 1 +calcBotWallCount(P a b)
--количество символов сверху от '+' включая потомков
calcTopWallCount (R a) = 0
calcTopWallCount (S a b) = max (calcTopWallCount a) (calcTopWallCount b)
calcTopWallCount (P a b) = 1 +getWallHeight 0 (calcParallelCon (P a b))+
                        calcTopWallCount a
--количество символов снизу от '+' включая потомков
calcBotWallCount (R a) = 0
calcBotWallCount (S a b) = max (calcTopWallCount a) (calcTopWallCount b)
calcBotWallCount (P a b) = 1 +getWallHeight 0 (calcParallelCon (P a b))+
                        calcBotWallCount b
--количество параллельных соединений (необходимо для расчета высоты стен в схемах)
calcParallelCon (R a) = 0
calcParallelCon (S a b)= max (calcParallelCon a) (calcParallelCon b)
calcParallelCon (P a b) = 1 + max (calcParallelCon a) (calcParallelCon b)
--количество символов стен (не совсем точно вычисляется, но всегда больше необходимого с небольшим запасом)
getWallHeight 0 0 = -1
getWallHeight val 0 = val
getWallHeight val n = getWallHeight (2*val+1) (n-1)
--сгенерировать пустое поле размера w на h
generateField w h = [[' ' |x<-[0..w-1]]|y<-[0..h-1]]
--получить лист строк для определенного типа элемента
getElement (R a) = [" "++show a ++" "]
--получить лист строк для параллельного соединения, здесь будет находиться каркас
--с пропусками на местах сопротивлений (все размеры учитываются)
getElement (P a b) = let
  halfTopHeight=calcTopWallCount (P a b)
  halfBotHeight=calcBotWallCount (P a b)
  wallHeight=getWallHeight 0 (calcParallelCon (P a b))
  widthMax = max (calcWidth a) (calcWidth b)
  widthThis=calcWidth(P a b)
  topElement = generateField widthThis (halfTopHeight-wallHeight-1)
  topMidElement = "  +--"++[' '|x<-[0..widthMax-1]]++"--+  "
  wallElements = ["  |  "++[' '|x<-[0..widthMax-1]]++"  |  "|y<-[0..wallHeight-1]]
  midElement = "--+  "++[' '|x<-[0..widthMax-1]]++"  +--"
  botElement = generateField widthThis (halfBotHeight-wallHeight-1)
  in topElement++[topMidElement]++wallElements++[midElement]++wallElements++[topMidElement]++botElement
  --получить лист строк для последовательного соединения, здесь будет находиться каркас
  --с пропусками на местах сопротивлений (все размеры учитываются)
getElement (S a b)= let
  offTopHeight=max (calcTopWallCount a) (calcTopWallCount b)
  offBotHeight=max (calcBotWallCount a) (calcBotWallCount b)
  widthA = calcWidth a
  widthB = calcWidth b
  widthThis=calcWidth(S a b)
  topElement = generateField widthThis offTopHeight
  midElement = "--+--"++[' '|x<-[0..widthA-1]]++"--+--"++[' '|x<-[0..widthB-1]]++"--+--"
  botElement = generateField widthThis offBotHeight
  in topElement++[midElement]++botElement
--соединить две структуры в одну и отдать результат (обычно происходить вставка matrImport в matrBase)
connectStructs matrBase matrImport x y =let
  baseWidth = length (head matrBase)
  baseHeight = length matrBase
  importWidth = length (head matrImport)
  importHeight = length matrImport
  in map(\j->map(\i->if i>=x && j>=y && (i-x<importWidth) && (j-y<importHeight) then matrImport !! (j-y) !! (i-x) else  matrBase !! j !! i)[0..baseWidth-1])[0..baseHeight-1]

--нарисовать провод длины len
getWire len = [['-'|x<-[0..len-1]]]
--присоединить структуру одиночного сопротивления к текущей матрице, текущие координаты posBaseX posBaseY (верхний левый угол matr)
build (R a) matr posBaseX posBaseY = connectStructs matr (getElement (R a)) posBaseX posBaseY
--присоединить структуру параллельного соединения к текущей матрице, текущие координаты posBaseX posBaseY (верхний левый угол matr)
build (P a b) matr posBaseX posBaseY = let
  schA = getElement a
  schB = getElement b
  heightA=calcHeight a
  heightB=calcHeight b
  widthA = calcWidth a
  widthB = calcWidth b

  wireY
    | (widthA>widthB)=
        posBaseY+calcHeight(P a b)-calcBotWallCount(P a b)+ getWallHeight 0 (calcParallelCon (P a b))
    | widthA<widthB =
        posBaseY+calcTopWallCount(P a b) - getWallHeight 0 (calcParallelCon (P a b))-1
    | otherwise = 0
  wireLen
    | (widthB>widthA)=
      (widthB `div` 2 - 1)-(widthA `div` 2 -1)
    | (widthA>widthB)=
      (widthA `div` 2-1)-(widthB `div` 2 -1)
    | otherwise = 0
  matrWithWire=connectStructs matr (getWire wireLen) (posBaseX+5) wireY
  matrWithWires=connectStructs
    matrWithWire
    (getWire wireLen)
    (posBaseX+5+wireLen+if widthA>widthB
      then widthB
      else if widthB>widthA
         then widthA
         else 0)
    wireY

  posAy = posBaseY
  posAx = posBaseX+5+if widthB>widthA
     then (widthB `div` 2 - 1)-(widthA `div` 2 -1)
     else 0
  matrWithA = connectStructs matrWithWires schA posAx posAy
  posBy = posBaseY+calcHeight(P a b)-heightB
  posBx = posBaseX+5 + if widthA>widthB
     then (widthA `div` 2-1)-(widthB `div` 2 -1)
     else 0
  res = connectStructs matrWithA schB posBx posBy
  matrResA = build a res posAx posAy

  in build b matrResA posBx posBy
--присоединить структуру последовательного соединения к текущей матрице, текущие координаты posBaseX posBaseY (верхний левый угол matr)
build (S a b) matr posBaseX posBaseY = let
  schA = getElement a
  schB = getElement b
  heightA=calcHeight a
  heightB=calcHeight b
  widthA=calcWidth a
  widthB=calcWidth b
  halfTopHeightA=calcTopWallCount a
  halfTopHeightB=calcTopWallCount b
  posAy = posBaseY+ if halfTopHeightA>=halfTopHeightB
    then 0
    else halfTopHeightB-halfTopHeightA
  posAx = posBaseX+5
  matrWithA = connectStructs matr schA posAx posAy
  posBy = posBaseY+ if halfTopHeightB>=halfTopHeightA
    then 0
    else halfTopHeightA-halfTopHeightB
  posBx = posAx+widthA+5
  res = connectStructs matrWithA schB posBx posBy
  matrResA = build a res posAx posAy
  in build b matrResA posBx posBy
--вызывается чтобы передать первый вызов в build с подготовленным пустым полем
firstcall sch = build sch (getElement sch) 0 0

data Scheme a = R Int | S (Scheme a) (Scheme a) | P (Scheme a) (Scheme a)
instance Show (Scheme a) where
  show (S a b ) = let
                  res=firstcall (S a b)
                  in unlines res
  show (P a b ) = let
                  res=firstcall (P a b)
                  in unlines res
  show (R a ) = let
                res=firstcall (R a)
                in unlines res
--вывести на печать лист строк
prettyPrint schms=putStr (unlines schms)

--самое большое соединение, занимает у меня весь экран 1920-1080
exmpTitan=S(P (P (P (R 1) (R 3)) (P (P (R 1) (P (R 1) (R 3))) (R 5))) (P (S( S(P(P(R 5)(R 5))(R 2)) (P(R 1)(R 2)))(R 1)) (P (P (R 1) (R 3)) (P (R 1) (R 3)))))(S(P (P (R 1) (R 3)) (P (R 1) (R 3)))(R 2))

--ну и дальше не такие выдающиеся примеры идут
exmp1=P (R 1) (P (P (R 1) (R 3)) (P (R 1) (R 3)))
exmp2=S(P (P (R 1) (R 2)) (P (R 1) (R 2)))(P (P (P (R 1) (R 2)) (R 2)) (P (R 1) (R 2)))
exmp3=S(P (P (R 1) (R 3)) (P (P (R 1) (P (R 1) (R 3))) (R 5))) (R 1)
--тут больше упор на последовательное соединение, но в итоге картинка тоже большая
exmpMultPosledovat=P(S( S(P(S(P(S(P(S(S(S(R 1)(P (R 1) (P (P (R 1) (R 3)) (P (R 1) (R 3)))))(R 2))(R 2))(R 2)) (P(R 1)(R 2)))(R 2)) (P(R 1)(R 2)))(R 2)) (P(R 1)(R 2)))(R 1)) (P(R 1)(P(R 1)(R 2)))
