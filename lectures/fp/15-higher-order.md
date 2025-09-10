---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Функции высшего порядка
## Лекция 15: Функции как данные

**Преподаватель:** [Ваше имя]  
**Группа:** 203  
**Семестр:** Осенний 2024

---

# План лекции

1. **Что такое функции высшего порядка?**
2. **Map, filter, fold**
3. **Композиция функций**
4. **Частичное применение**
5. **Лямбда-функции**
6. **Практический пример: Игровая логика**

---

# Что такое функции высшего порядка?

## Определение:
**Функция высшего порядка** — функция, которая принимает другие функции в качестве аргументов или возвращает функции.

## Преимущества:
- **Переиспользование** кода
- **Абстракция** общих паттернов
- **Читаемость** и выразительность
- **Композиция** функций

## Примеры:
- `map` — применяет функцию к каждому элементу списка
- `filter` — фильтрует элементы по условию
- `fold` — сворачивает список в одно значение

---

# Map функция

## Определение:
```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

## Использование:
```haskell
-- Применение функции к каждому элементу
doubleList :: [Int] -> [Int]
doubleList xs = map (*2) xs

-- Преобразование типов
getNames :: [Unit] -> [String]
getNames units = map unitName units

-- Работа с позициями
getXCoordinates :: [Position] -> [Int]
getXCoordinates positions = map x positions

-- Сложные преобразования
formatUnits :: [Unit] -> [String]
formatUnits units = map formatUnit units
    where
        formatUnit unit = unitName unit ++ " (HP: " ++ show (unitHealth unit) ++ ")"

-- Примеры вызовов
-- map (*2) [1, 2, 3] -> [2, 4, 6]
-- map show [1, 2, 3] -> ["1", "2", "3"]
-- map length ["hello", "world"] -> [5, 5]
```

---

# Filter функция

## Определение:
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
```

## Использование:
```haskell
-- Фильтрация по условию
aliveUnits :: [Unit] -> [Unit]
aliveUnits units = filter isAlive units

-- Фильтрация по типу
warriors :: [Unit] -> [Unit]
warriors units = filter (\unit -> unitType unit == Warrior) units

-- Фильтрация по позиции
unitsInRange :: Position -> Int -> [Unit] -> [Unit]
unitsInRange center range units = 
    filter (\unit -> manhattanDistance center (unitPosition unit) <= range) units

-- Фильтрация по здоровью
healthyUnits :: [Unit] -> [Unit]
healthyUnits units = filter (\unit -> unitHealth unit > 50) units

-- Комбинированная фильтрация
strongWarriors :: [Unit] -> [Unit]
strongWarriors units = filter (\unit -> 
    unitType unit == Warrior && unitHealth unit > 100) units

-- Примеры вызовов
-- filter (>5) [1, 2, 3, 4, 5, 6, 7, 8, 9] -> [6, 7, 8, 9]
-- filter even [1, 2, 3, 4, 5, 6] -> [2, 4, 6]
-- filter (not . null) ["", "hello", "", "world"] -> ["hello", "world"]
```

---

# Fold функции

## Foldr (справа налево):
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

-- Примеры использования
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

productList :: [Int] -> Int
productList xs = foldr (*) 1 xs

concatList :: [[a]] -> [a]
concatList xs = foldr (++) [] xs

-- Сложные примеры
countAliveUnits :: [Unit] -> Int
countAliveUnits units = foldr (\unit count -> 
    if isAlive unit then count + 1 else count) 0 units

getTotalHealth :: [Unit] -> Int
getTotalHealth units = foldr (\unit total -> 
    total + unitHealth unit) 0 units

findStrongestUnit :: [Unit] -> Maybe Unit
findStrongestUnit units = foldr (\unit strongest -> 
    case strongest of
        Nothing -> Just unit
        Just current -> if unitHealth unit > unitHealth current 
                       then Just unit else Just current) Nothing units
```

## Foldl (слева направо):
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Примеры использования
reverseList :: [a] -> [a]
reverseList xs = foldl (\acc x -> x : acc) [] xs

digitsToNumber :: [Int] -> Int
digitsToNumber digits = foldl (\acc digit -> acc * 10 + digit) 0 digits

-- Игровые примеры
calculateTotalDamage :: [Unit] -> Int
calculateTotalDamage units = foldl (\total unit -> 
    total + unitAttack unit) 0 units

buildUnitList :: [Unit] -> String
buildUnitList units = foldl (\acc unit -> 
    acc ++ unitName unit ++ ", ") "" units
```

---

# Композиция функций

## Оператор композиции:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

-- Примеры использования
-- Композиция простых функций
doubleAndIncrement :: Int -> Int
doubleAndIncrement = (+1) . (*2)

-- Композиция с функциями для работы со списками
getAliveUnitNames :: [Unit] -> [String]
getAliveUnitNames = map unitName . filter isAlive

getStrongUnitPositions :: [Unit] -> [Position]
getStrongUnitPositions = map unitPosition . filter (\unit -> unitHealth unit > 100)

-- Сложная композиция
formatAliveUnits :: [Unit] -> String
formatAliveUnits = 
    concat . map (\unit -> unitName unit ++ " (HP: " ++ show (unitHealth unit) ++ ")\n") 
    . filter isAlive

-- Композиция с преобразованием типов
getUnitHealths :: [Unit] -> [Int]
getUnitHealths = map unitHealth . filter isAlive

-- Композиция с сортировкой
getSortedUnitNames :: [Unit] -> [String]
getSortedUnitNames = map unitName . sortBy (comparing unitHealth) . filter isAlive
```

## Множественная композиция:
```haskell
-- Цепочка преобразований
processGameUnits :: [Unit] -> String
processGameUnits = 
    formatUnits 
    . sortBy (comparing unitHealth) 
    . filter isAlive 
    . filter (\unit -> unitType unit == Warrior)

-- Разбиение на шаги для читаемости
processGameUnits' :: [Unit] -> String
processGameUnits' units = 
    let aliveUnits = filter isAlive units
        warriors = filter (\unit -> unitType unit == Warrior) aliveUnits
        sortedWarriors = sortBy (comparing unitHealth) warriors
    in formatUnits sortedWarriors

-- Композиция с условиями
getGameSummary :: GameState -> String
getGameSummary = 
    formatGameState 
    . calculateStatistics 
    . filterActivePlayers 
    . getCurrentState
```

---

# Частичное применение

## Что такое частичное применение?
**Частичное применение** — создание новой функции путем фиксации части аргументов существующей функции.

## Примеры:
```haskell
-- Частичное применение с двумя аргументами
add :: Int -> Int -> Int
add x y = x + y

addFive :: Int -> Int
addFive = add 5

-- Частичное применение с функциями высшего порядка
filterAlive :: [Unit] -> [Unit]
filterAlive = filter isAlive

mapNames :: [Unit] -> [String]
mapNames = map unitName

-- Комбинирование частичного применения
getAliveUnitNames :: [Unit] -> [String]
getAliveUnitNames = map unitName . filter isAlive

-- Частичное применение с операторами
isGreaterThan :: Int -> Int -> Bool
isGreaterThan = (>)

isLessThan :: Int -> Int -> Bool
isLessThan = (<)

-- Создание специализированных функций
isHealthy :: Unit -> Bool
isHealthy = (>100) . unitHealth

isWeak :: Unit -> Bool
isWeak = (<50) . unitHealth

-- Частичное применение с кортежами
getFirst :: (a, b, c) -> a
getFirst = fst . (\(x, _, _) -> (x, x))

-- Частичное применение с Maybe
maybeDefault :: a -> Maybe a -> a
maybeDefault defaultVal = maybe defaultVal id
```

---

# Лямбда-функции

## Синтаксис:
```haskell
-- Лямбда-функции (анонимные функции)
\x -> x + 1                    -- добавляет 1 к аргументу
\x y -> x + y                  -- складывает два аргумента
\unit -> unitHealth unit > 50  -- проверяет здоровье юнита

-- Использование с map
incrementList :: [Int] -> [Int]
incrementList xs = map (\x -> x + 1) xs

-- Использование с filter
strongUnits :: [Unit] -> [Unit]
strongUnits units = filter (\unit -> unitHealth unit > 100) units

-- Использование с fold
totalAttack :: [Unit] -> Int
totalAttack units = foldr (\unit total -> total + unitAttack unit) 0 units

-- Сложные лямбда-функции
formatUnitDetailed :: Unit -> String
formatUnitDetailed unit = 
    let healthStatus = if unitHealth unit > 50 then "Healthy" else "Weak"
    in unitName unit ++ " (" ++ healthStatus ++ ", HP: " ++ show (unitHealth unit) ++ ")"

-- Лямбда-функции с pattern matching
getUnitPositions :: [Unit] -> [Position]
getUnitPositions = map (\(Unit _ _ _ _ _ _ pos _ _) -> pos)

-- Лямбда-функции с guards
classifyUnit :: Unit -> String
classifyUnit unit = 
    let health = unitHealth unit
    in if health > 100 then "Strong"
       else if health > 50 then "Medium"
       else "Weak"
```

---

# Практический пример: Игровая логика

```haskell
-- Основные типы для игры
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

data Position = Position Int Int deriving (Show, Eq)
data UnitType = Warrior | Archer | Mage deriving (Show, Eq)

-- Функции для работы с юнитами
isAlive :: Unit -> Bool
isAlive unit = unitHealth unit > 0

isHealthy :: Unit -> Bool
isHealthy unit = unitHealth unit > unitMaxHealth unit `div` 2

canMoveTo :: Unit -> Position -> Bool
canMoveTo unit target = 
    let distance = manhattanDistance (unitPosition unit) target
    in distance <= unitMovementRange unit

canAttack :: Unit -> Unit -> Bool
canAttack attacker target
    | not (isAlive attacker) = False
    | not (isAlive target) = False
    | otherwise = 
        let distance = manhattanDistance (unitPosition attacker) (unitPosition target)
        in distance <= unitAttackRange attacker

-- Функции высшего порядка для игровой логики
getAliveUnits :: [Unit] -> [Unit]
getAliveUnits = filter isAlive

getUnitsByType :: UnitType -> [Unit] -> [Unit]
getUnitsByType targetType = filter (\unit -> unitType unit == targetType)

getUnitsInRange :: Position -> Int -> [Unit] -> [Unit]
getUnitsInRange center range = filter (\unit -> 
    manhattanDistance center (unitPosition unit) <= range)

getUnitNames :: [Unit] -> [String]
getUnitNames = map unitName

getUnitPositions :: [Unit] -> [Position]
getUnitPositions = map unitPosition

getUnitHealths :: [Unit] -> [Int]
getUnitHealths = map unitHealth

-- Сложные функции с композицией
getAliveUnitNames :: [Unit] -> [String]
getAliveUnitNames = map unitName . filter isAlive

getHealthyUnitPositions :: [Unit] -> [Position]
getHealthyUnitPositions = map unitPosition . filter isHealthy

getStrongUnitsInRange :: Position -> Int -> [Unit] -> [Unit]
getStrongUnitsInRange center range = 
    filter isHealthy . getUnitsInRange center range

-- Функции для анализа игрового состояния
calculateTotalHealth :: [Unit] -> Int
calculateTotalHealth = foldr (\unit total -> total + unitHealth unit) 0

calculateAverageHealth :: [Unit] -> Double
calculateAverageHealth units = 
    let total = calculateTotalHealth units
        count = length units
    in if count == 0 then 0.0 else fromIntegral total / fromIntegral count

findStrongestUnit :: [Unit] -> Maybe Unit
findStrongestUnit units = 
    let aliveUnits = filter isAlive units
    in if null aliveUnits 
       then Nothing 
       else Just (foldr1 (\u1 u2 -> 
           if unitHealth u1 > unitHealth u2 then u1 else u2) aliveUnits)

findWeakestUnit :: [Unit] -> Maybe Unit
findWeakestUnit units = 
    let aliveUnits = filter isAlive units
    in if null aliveUnits 
       then Nothing 
       else Just (foldr1 (\u1 u2 -> 
           if unitHealth u1 < unitHealth u2 then u1 else u2) aliveUnits)

-- Функции для игровых действий
getAttackableTargets :: Unit -> [Unit] -> [Unit]
getAttackableTargets attacker = 
    filter (canAttack attacker) . filter isAlive

getBestTarget :: Unit -> [Unit] -> Maybe Unit
getBestTarget attacker = 
    let targets = getAttackableTargets attacker enemies
    in if null targets 
       then Nothing 
       else Just (foldr1 (\t1 t2 -> 
           if unitHealth t1 < unitHealth t2 then t1 else t2) targets)

-- Функции для форматирования
formatUnit :: Unit -> String
formatUnit unit = 
    unitName unit ++ " (" ++ show (unitType unit) ++ 
    ", HP: " ++ show (unitHealth unit) ++ "/" ++ show (unitMaxHealth unit) ++ ")"

formatUnitList :: [Unit] -> String
formatUnitList units = 
    concat . map (\unit -> formatUnit unit ++ "\n") $ units

formatGameSummary :: [Unit] -> String
formatGameSummary units = 
    let aliveCount = length (filter isAlive units)
        totalCount = length units
        totalHealth = calculateTotalHealth units
        avgHealth = calculateAverageHealth units
    in "Units: " ++ show aliveCount ++ "/" ++ show totalCount ++ 
       "\nTotal Health: " ++ show totalHealth ++ 
       "\nAverage Health: " ++ show avgHealth

-- Вспомогательные функции
manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x1 y1) (Position x2 y2) = 
    abs (x1 - x2) + abs (y1 - y2)

-- Функции для работы с ресурсами
data Resource = Resource String Int deriving (Show, Eq)

getResourceAmount :: Resource -> Int
getResourceAmount (Resource _ amount) = amount

getResourceName :: Resource -> String
getResourceName (Resource name _) = name

calculateTotalResources :: [Resource] -> Int
calculateTotalResources = foldr (\res total -> total + getResourceAmount res) 0

getResourcesByName :: String -> [Resource] -> [Resource]
getResourcesByName targetName = filter (\res -> getResourceName res == targetName)

-- Функции для работы с игровыми событиями
data GameEvent = 
    UnitMoved Unit Position Position
  | UnitAttacked Unit Unit Int
  | UnitDied Unit
  | ResourceCollected Resource
  | GameStarted
  | GameEnded String
    deriving (Show, Eq)

formatGameEvent :: GameEvent -> String
formatGameEvent event = case event of
    UnitMoved unit from to -> 
        unitName unit ++ " moved from " ++ show from ++ " to " ++ show to
    UnitAttacked attacker target damage -> 
        unitName attacker ++ " attacked " ++ unitName target ++ " for " ++ show damage ++ " damage"
    UnitDied unit -> 
        unitName unit ++ " died"
    ResourceCollected resource -> 
        "Collected " ++ show (getResourceAmount resource) ++ " " ++ getResourceName resource
    GameStarted -> "Game started"
    GameEnded winner -> "Game ended. Winner: " ++ winner

-- Функции для анализа игровых событий
getEventsByType :: GameEvent -> [GameEvent] -> [GameEvent]
getEventsByType targetEvent = filter (\event -> 
    case (targetEvent, event) of
        (UnitMoved _ _ _, UnitMoved _ _ _) -> True
        (UnitAttacked _ _ _, UnitAttacked _ _ _) -> True
        (UnitDied _, UnitDied _) -> True
        (ResourceCollected _, ResourceCollected _) -> True
        (GameStarted, GameStarted) -> True
        (GameEnded _, GameEnded _) -> True
        _ -> False)

countEventType :: GameEvent -> [GameEvent] -> Int
countEventType eventType = length . getEventsByType eventType
```

---

# Лучшие практики функций высшего порядка

## ✅ Что делать:
- **Использовать map, filter, fold** для работы со списками
- **Применять композицию функций** для создания цепочек преобразований
- **Использовать частичное применение** для создания специализированных функций
- **Создавать читаемые лямбда-функции** с понятными именами
- **Комбинировать функции** для сложных операций

## ❌ Чего избегать:
- **Создавать слишком сложные** композиции
- **Использовать лямбда-функции** там, где можно использовать именованные функции
- **Игнорировать читаемость** кода ради краткости
- **Смешивать** императивный и функциональный стили
- **Забывать про производительность** при работе с большими списками

---

# Домашнее задание

## Задача 1:
Реализовать функции для работы с игровыми картами

## Задача 2:
Создать функции для анализа игрового состояния

## Задача 3:
Реализовать функции для игровых действий

---

# Что дальше?

## На следующей лекции:
- **Монады и IO**
- **Работа с файлами**
- **Обработка ошибок**
- **Тестирование**

## Подготовка:
- Изучить главу 27-28 из учебника
- Выполнить домашнее задание
- Подготовить вопросы по текущей теме

---

# Вопросы?

## Контакты:
- **Email:** [ваш.email@university.edu]
- **Telegram:** [@username]
- **Офис:** [номер кабинета]

## Следующая лекция: **Монады и IO**
