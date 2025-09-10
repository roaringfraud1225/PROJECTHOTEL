---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Типы данных и алгебраические типы
## Лекция 14: Система типов Haskell

**Преподаватель:** [Ваше имя]  
**Группа:** 203  
**Семестр:** Осенний 2024

---

# План лекции

1. **Система типов Haskell**
2. **Базовые типы**
3. **Алгебраические типы данных**
4. **Pattern matching**
5. **Type classes**
6. **Практический пример: Игровые типы**

---

# Система типов Haskell

## Особенности:
- **Статическая типизация** — типы проверяются на этапе компиляции
- **Вывод типов** — компилятор автоматически определяет типы
- **Полиморфизм** — функции могут работать с разными типами
- **Type classes** — интерфейсы для типов

## Преимущества:
- **Безопасность** — ошибки типов обнаруживаются на этапе компиляции
- **Читаемость** — типы служат документацией
- **Оптимизация** — компилятор может оптимизировать код
- **Рефакторинг** — изменения типов безопасны

---

# Базовые типы

## Примитивные типы:
```haskell
-- Числовые типы
42          :: Int        -- 32-битные целые
3.14        :: Double     -- числа с плавающей точкой
42          :: Integer    -- произвольной точности
3.14        :: Float      -- одинарная точность

-- Символьные типы
'a'         :: Char       -- символ
"Hello"     :: String     -- строка (список символов)

-- Логический тип
True        :: Bool       -- логическое значение
False       :: Bool

-- Функциональный тип
(+)         :: Num a => a -> a -> a  -- функция сложения
```

## Списки:
```haskell
[1, 2, 3]           :: [Int]        -- список целых
['a', 'b', 'c']     :: [Char]       -- список символов
[[1, 2], [3, 4]]    :: [[Int]]      -- список списков
[]                   :: [a]          -- пустой список любого типа
```

---

# Функции и типы

## Аннотации типов:
```haskell
-- Явное указание типа функции
add :: Int -> Int -> Int
add x y = x + y

-- Функция с полиморфным типом
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- Функция высшего порядка
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- Функция с ограничением типа
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
```

## Вывод типов:
```haskell
-- Компилятор автоматически выводит типы
double x = x * 2           -- double :: Num a => a -> a
isEven x = x `mod` 2 == 0  -- isEven :: Integral a => a -> Bool
append xs ys = xs ++ ys     -- append :: [a] -> [a] -> [a]
```

---

# Алгебраические типы данных

## Что такое ADT?
**Алгебраические типы данных** — способ создания новых типов путем комбинирования существующих.

## Виды ADT:
- **Product types** — типы-произведения (кортежи, записи)
- **Sum types** — типы-суммы (перечисления, варианты)
- **Recursive types** — рекурсивные типы (списки, деревья)

---

# Product Types

## Кортежи:
```haskell
-- Кортежи фиксированной длины
(1, "hello")           :: (Int, String)
(1, 2, 3)             :: (Int, Int, Int)
(True, 42, "test")     :: (Bool, Int, String)

-- Функции для работы с кортежами
fst :: (a, b) -> a
snd :: (a, b) -> b

-- Пример использования
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getSecond :: (a, b, c) -> b
getSecond (_, y, _) = y
```

## Записи (Records):
```haskell
-- Определение записи
data Player = Player
    { playerName :: String
    , playerHealth :: Int
    , playerLevel :: Int
    , playerPosition :: Position
    } deriving (Show, Eq)

-- Создание записи
hero :: Player
hero = Player "Aragorn" 150 5 (Position 0 0)

-- Доступ к полям
getName :: Player -> String
getName = playerName

getHealth :: Player -> Int
getHealth = playerHealth

-- Обновление полей
healPlayer :: Int -> Player -> Player
healPlayer amount player = player { playerHealth = playerHealth player + amount }
```

---

# Sum Types

## Перечисления:
```haskell
-- Простое перечисление
data Direction = North | South | East | West
    deriving (Show, Eq)

-- Перечисление с данными
data UnitType = Warrior | Archer | Mage
    deriving (Show, Eq)

-- Перечисление с параметрами
data GameEvent = 
    UnitMoved Unit Position Position
  | UnitAttacked Unit Unit Int
  | UnitDied Unit
  | GameStarted
  | GameEnded String
    deriving (Show, Eq)

-- Функции для работы с перечислениями
isWarrior :: UnitType -> Bool
isWarrior Warrior = True
isWarrior _ = False

getEventDescription :: GameEvent -> String
getEventDescription (UnitMoved unit from to) = 
    "Unit " ++ unitName unit ++ " moved from " ++ show from ++ " to " ++ show to
getEventDescription (UnitAttacked attacker target damage) = 
    "Unit " ++ unitName attacker ++ " attacked " ++ unitName target ++ " for " ++ show damage ++ " damage"
getEventDescription (UnitDied unit) = 
    "Unit " ++ unitName unit ++ " died"
getEventDescription GameStarted = "Game started"
getEventDescription (GameEnded winner) = "Game ended. Winner: " ++ winner
```

---

# Рекурсивные типы

## Списки:
```haskell
-- Определение списка
data List a = Empty | Cons a (List a)
    deriving (Show, Eq)

-- Функции для работы со списком
listLength :: List a -> Int
listLength Empty = 0
listLength (Cons _ xs) = 1 + listLength xs

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)
```

## Деревья:
```haskell
-- Бинарное дерево
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Функции для работы с деревом
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Empty = Node x Empty Empty
treeInsert x (Node val left right)
    | x < val = Node val (treeInsert x left) right
    | x > val = Node val left (treeInsert x right)
    | otherwise = Node val left right
```

---

# Pattern Matching

## Сопоставление с образцом:
```haskell
-- Сопоставление с кортежами
getCoordinates :: (Int, Int) -> String
getCoordinates (x, y) = "Position: (" ++ show x ++ ", " ++ show y ++ ")"

-- Сопоставление со списками
listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- Сопоставление с записями
isAlive :: Player -> Bool
isAlive player = playerHealth player > 0

-- Сопоставление с перечислениями
canMove :: UnitType -> Bool
canMove Warrior = True
canMove Archer = True
canMove Mage = False

-- Сопоставление с вложенными структурами
getUnitInfo :: Unit -> String
getUnitInfo (Unit name health level pos) = 
    "Unit: " ++ name ++ ", Health: " ++ show health ++ 
    ", Level: " ++ show level ++ ", Position: " ++ show pos
```

---

# Guards и Case выражения

## Guards:
```haskell
-- Использование guards
describeHealth :: Int -> String
describeHealth health
    | health <= 0 = "Dead"
    | health < 25 = "Critical"
    | health < 50 = "Low"
    | health < 75 = "Medium"
    | otherwise = "High"

-- Guards с условиями
canAttack :: Unit -> Unit -> Bool
canAttack attacker target
    | not (isAlive attacker) = False
    | not (isAlive target) = False
    | distance (unitPosition attacker) (unitPosition target) > attackRange attacker = False
    | otherwise = True
```

## Case выражения:
```haskell
-- Case для перечислений
getUnitDescription :: UnitType -> String
getUnitDescription unitType = case unitType of
    Warrior -> "Strong melee fighter"
    Archer -> "Ranged attacker"
    Mage -> "Magic user"

-- Case для сложных типов
handleGameEvent :: GameEvent -> String
handleGameEvent event = case event of
    UnitMoved unit from to -> 
        "Unit " ++ unitName unit ++ " moved from " ++ show from ++ " to " ++ show to
    UnitAttacked attacker target damage -> 
        "Unit " ++ unitName attacker ++ " attacked " ++ unitName target ++ " for " ++ show damage ++ " damage"
    UnitDied unit -> 
        "Unit " ++ unitName unit ++ " died"
    GameStarted -> "Game started"
    GameEnded winner -> "Game ended. Winner: " ++ winner
```

---

# Type Classes

## Что такое Type Classes?
**Type Classes** — интерфейсы для типов, определяющие набор операций.

## Основные Type Classes:
```haskell
-- Eq - равенство
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Show - преобразование в строку
class Show a where
    show :: a -> String

-- Ord - упорядочивание
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool

-- Num - числовые операции
class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
```

---

# Реализация Type Classes

## Автоматическая реализация:
```haskell
-- deriving автоматически создает реализации
data Position = Position Int Int
    deriving (Show, Eq, Ord)

-- Show: Position 1 2
-- Eq: Position 1 2 == Position 1 2 -> True
-- Ord: Position 1 2 < Position 2 1 -> True
```

## Ручная реализация:
```haskell
-- Ручная реализация Show
data Unit = Unit String Int Int Position

instance Show Unit where
    show (Unit name health level pos) = 
        "Unit " ++ name ++ " (HP: " ++ show health ++ 
        ", Level: " ++ show level ++ ", Pos: " ++ show pos ++ ")"

-- Ручная реализация Eq
instance Eq Unit where
    (Unit name1 _ _ _) == (Unit name2 _ _ _) = name1 == name2

-- Ручная реализация Ord
instance Ord Unit where
    compare (Unit _ health1 _ _) (Unit _ health2 _ _) = compare health1 health2
```

---

# Практический пример: Игровые типы

```haskell
-- Основные типы для игры
data Position = Position 
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

data UnitType = Warrior | Archer | Mage
    deriving (Show, Eq)

data Unit = Unit
    { unitName :: String
    , unitType :: UnitType
    , unitHealth :: Int
    , unitMaxHealth :: Int
    , unitAttack :: Int
    , unitDefense :: Int
    , unitPosition :: Position
    , unitMovementRange :: Int
    , unitAttackRange :: Int
    } deriving (Show, Eq)

data BuildingType = Barracks | MageTower | Farm | Mine
    deriving (Show, Eq)

data Building = Building
    { buildingName :: String
    , buildingType :: BuildingType
    , buildingHealth :: Int
    , buildingPosition :: Position
    , buildingLevel :: Int
    } deriving (Show, Eq)

data ResourceType = Gold | Wood | Stone | Food
    deriving (Show, Eq)

data Resource = Resource
    { resourceType :: ResourceType
    , resourceAmount :: Int
    } deriving (Show, Eq)

data GameState = GameState
    { players :: [Player]
    , units :: [Unit]
    , buildings :: [Building]
    , resources :: [Resource]
    , currentTurn :: Int
    , gamePhase :: GamePhase
    } deriving (Show, Eq)

data GamePhase = PlayerTurn | AITurn | GameOver
    deriving (Show, Eq)

data Player = Player
    { playerName :: String
    , playerResources :: [Resource]
    , playerUnits :: [Unit]
    , playerBuildings :: [Building]
    } deriving (Show, Eq)

-- Функции для работы с типами
isAlive :: Unit -> Bool
isAlive unit = unitHealth unit > 0

canMoveTo :: Unit -> Position -> Bool
canMoveTo unit target = 
    let currentPos = unitPosition unit
        distance = manhattanDistance currentPos target
    in distance <= unitMovementRange unit

canAttack :: Unit -> Unit -> Bool
canAttack attacker target
    | not (isAlive attacker) = False
    | not (isAlive target) = False
    | otherwise = 
        let distance = manhattanDistance (unitPosition attacker) (unitPosition target)
        in distance <= unitAttackRange attacker

getUnitsInRange :: Position -> Int -> [Unit] -> [Unit]
getUnitsInRange center range units = 
    filter (\unit -> manhattanDistance center (unitPosition unit) <= range) units

findNearestEnemy :: Unit -> [Unit] -> Maybe Unit
findNearestEnemy unit enemies = 
    let aliveEnemies = filter isAlive enemies
        distances = map (\enemy -> (enemy, manhattanDistance (unitPosition unit) (unitPosition enemy))) aliveEnemies
        sorted = sortBy (comparing snd) distances
    in case sorted of
        [] -> Nothing
        ((enemy, _):_) -> Just enemy

-- Вспомогательные функции
manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x1 y1) (Position x2 y2) = 
    abs (x1 - x2) + abs (y1 - y2)

euclideanDistance :: Position -> Position -> Double
euclideanDistance (Position x1 y1) (Position x2 y2) = 
    sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

-- Функции для работы с ресурсами
hasEnoughResources :: Player -> [Resource] -> Bool
hasEnoughResources player required = 
    all (\req -> 
        let available = find (\r -> resourceType r == resourceType req) (playerResources player)
        in case available of
            Just avail -> resourceAmount avail >= resourceAmount req
            Nothing -> False
    ) required

spendResources :: Player -> [Resource] -> Maybe Player
spendResources player required
    | not (hasEnoughResources player required) = Nothing
    | otherwise = Just $ player { playerResources = updatedResources }
    where
        updatedResources = map updateResource (playerResources player)
        updateResource playerRes = 
            case find (\r -> resourceType r == resourceType playerRes) required of
                Just req -> playerRes { resourceAmount = resourceAmount playerRes - resourceAmount req }
                Nothing -> playerRes
```

---

# Лучшие практики работы с типами

## ✅ Что делать:
- **Использовать описательные имена** для типов и конструкторов
- **Применять deriving** для стандартных Type Classes
- **Создавать специфичные типы** вместо использования базовых
- **Использовать Pattern Matching** для безопасной работы с данными
- **Группировать связанные данные** в записи

## ❌ Чего избегать:
- **Использовать базовые типы** для доменных понятий
- **Создавать слишком сложные** типы
- **Игнорировать Type Classes** при проектировании
- **Забывать про Pattern Matching** в функциях
- **Создавать небезопасные** типы

---

# Домашнее задание

## Задача 1:
Создать типы для игровых карт и местности

## Задача 2:
Реализовать функции для работы с игровыми объектами

## Задача 3:
Создать Type Class для игровых сущностей

---

# Что дальше?

## На следующей лекции:
- **Функции высшего порядка**
- **Map, filter, fold**
- **Композиция функций**
- **Частичное применение**

## Подготовка:
- Изучить главу 25-26 из учебника
- Выполнить домашнее задание
- Подготовить вопросы по текущей теме

---

# Вопросы?

## Контакты:
- **Email:** [ваш.email@university.edu]
- **Telegram:** [@username]
- **Офис:** [номер кабинета]

## Следующая лекция: **Функции высшего порядка**
