# Лекция 2: Основные концепции ФП

## 📚 Цель лекции

Изучить детали реализации основных концепций ФП в Haskell: типы данных, функции, монады, функторы и их практическое применение.

## 🕐 План лекции

1. **Система типов Haskell**
2. **Функции и их типы**
3. **Функторы и аппликативные функторы**
4. **Монады и их применение**
5. **Типы-классы и их использование**
6. **Практические примеры**

---

## 🏗️ Система типов Haskell

### Базовые типы

```haskell
-- Базовые типы
basicTypes :: IO ()
basicTypes = do
    -- Целые числа
    let intValue = 42 :: Int
    let integerValue = 12345678901234567890 :: Integer
    
    -- Числа с плавающей точкой
    let floatValue = 3.14 :: Float
    let doubleValue = 3.141592653589793 :: Double
    
    -- Символы и строки
    let charValue = 'A' :: Char
    let stringValue = "Hello, Haskell!" :: String
    
    -- Логические значения
    let boolValue = True :: Bool
    
    putStrLn $ "Int: " ++ show intValue
    putStrLn $ "Integer: " ++ show integerValue
    putStrLn $ "Float: " ++ show floatValue
    putStrLn $ "Double: " ++ show doubleValue
    putStrLn $ "Char: " ++ show charValue
    putStrLn $ "String: " ++ show stringValue
    putStrLn $ "Bool: " ++ show boolValue

-- Типы-синонимы
type Name = String
type Age = Int
type PersonTuple = (Name, Age)

-- Использование типов-синонимов
createPerson :: Name -> Age -> PersonTuple
createPerson name age = (name, age)

-- Новые типы данных
newtype UserId = UserId Int deriving (Show, Eq, Ord)
newtype Email = Email String deriving (Show, Eq)

-- Использование новых типов
createUser :: UserId -> Email -> (UserId, Email)
createUser uid email = (uid, email)
```

### Алгебраические типы данных

```haskell
-- Простые алгебраические типы
data Color = Red | Green | Blue | Yellow deriving (Show, Eq)

data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
           deriving (Show)

-- Рекурсивные типы данных
data List a = Empty | Cons a (List a) deriving (Show)

-- Типы с параметрами
data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)

data Either a b = Left a | Right b deriving (Show, Eq)

-- Типы с полями (записи)
data Person = Person
    { name :: String
    , age  :: Int
    , email :: String
    } deriving (Show, Eq)

-- Типы с ограничениями
data OrdList a = OrdList [a] deriving Show

-- Функции для работы с типами
getColorName :: Color -> String
getColorName Red = "Красный"
getColorName Green = "Зеленый"
getColorName Blue = "Синий"
getColorName Yellow = "Желтый"

getShapeArea :: Shape -> Double
getShapeArea (Circle r) = pi * r * r
getShapeArea (Rectangle w h) = w * h
getShapeArea (Triangle a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- Работа с рекурсивными типами
listLength :: List a -> Int
listLength Empty = 0
listLength (Cons _ xs) = 1 + listLength xs

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)
```

---

## 🔧 Функции и их типы

### Сигнатуры функций

```haskell
-- Явное указание типов
add :: Int -> Int -> Int
add x y = x + y

-- Функции с полиморфными типами
id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

-- Функции высшего порядка
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- Функции с ограничениями типов
sum :: (Num a, Foldable t) => t a -> a
sum = foldr (+) 0

product :: (Num a, Foldable t) => t a -> a
product = foldr (*) 1

-- Каррирование и частичное применение
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addFive :: Int -> Int
addFive = addThree 2 3

-- Композиция функций
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- Использование композиции
doubleThenSquare :: Int -> Int
doubleThenSquare = (^2) . (*2)

-- Функции с несколькими аргументами
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
```

### Лямбда-функции

```haskell
-- Лямбда-функции
lambdaExamples :: IO ()
lambdaExamples = do
    -- Простая лямбда
    let add = \x y -> x + y
    putStrLn $ "Лямбда сложение: " ++ show (add 5 3)
    
    -- Лямбда в map
    let numbers = [1..5]
    let doubled = map (\x -> x * 2) numbers
    putStrLn $ "Удвоенные числа: " ++ show doubled
    
    -- Лямбда с несколькими паттернами
    let process = \case
            [] -> "Пустой список"
            [x] -> "Один элемент: " ++ show x
            xs -> "Несколько элементов: " ++ show (length xs)
    
    putStrLn $ process []
    putStrLn $ process [42]
    putStrLn $ process [1, 2, 3]
    
    -- Лямбда с where
    let complexLambda = \x y -> result
            where
                result = x * y + x + y
    
    putStrLn $ "Сложная лямбда: " ++ show (complexLambda 3 4)

-- Лямбды в функциях высшего порядка
applyFunction :: (a -> b) -> a -> b
applyFunction f = \x -> f x

-- Лямбды для создания функций
createMultiplier :: Int -> (Int -> Int)
createMultiplier n = \x -> x * n

-- Использование
main :: IO ()
main = do
    lambdaExamples
    
    let multiplyBy3 = createMultiplier 3
    putStrLn $ "Умножаем на 3: " ++ show (multiplyBy3 7)
    
    let applyTwice = applyFunction (\x -> x * x)
    putStrLn $ "Применяем дважды: " ++ show (applyTwice 5)
```

---

## 🎯 Функторы и аппликативные функторы

### Функторы

```haskell
-- Класс Functor
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Реализация для Maybe
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- Реализация для списков
instance Functor [] where
    fmap = map

-- Реализация для Either
instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

-- Реализация для кортежей
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

-- Использование функторов
functorExamples :: IO ()
functorExamples = do
    -- Maybe
    let maybeValue = Just 5
    let doubled = fmap (*2) maybeValue
    putStrLn $ "Maybe: " ++ show doubled
    
    -- Списки
    let numbers = [1, 2, 3, 4, 5]
    let squared = fmap (^2) numbers
    putStrLn $ "Квадраты: " ++ show squared
    
    -- Either
    let eitherValue = Right "Hello"
    let uppercased = fmap (map toUpper) eitherValue
    putStrLn $ "Either: " ++ show uppercased
    
    -- Кортежи
    let tuple = ("Label", 42)
    let modified = fmap (*2) tuple
    putStrLn $ "Кортеж: " ++ show modified

-- Оператор <$> (синоним для fmap)
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- Использование оператора
operatorExamples :: IO ()
operatorExamples = do
    let numbers = [1..5]
    let doubled = (*2) <$> numbers
    putStrLn $ "Оператор <$>: " ++ show doubled
    
    let maybeValue = Just 10
    let result = show <$> maybeValue
    putStrLn $ "Maybe с оператором: " ++ show result
```

### Аппликативные функторы

```haskell
-- Класс Applicative
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- Реализация для Maybe
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x

-- Реализация для списков
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- Реализация для Either
instance Applicative (Either a) where
    pure = Right
    Left e <*> _ = Left e
    Right f <*> x = fmap f x

-- Использование аппликативных функторов
applicativeExamples :: IO ()
applicativeExamples = do
    -- Maybe
    let maybeFunc = Just (*2)
    let maybeValue = Just 5
    let result = maybeFunc <*> maybeValue
    putStrLn $ "Maybe Applicative: " ++ show result
    
    -- Списки
    let funcs = [(+1), (*2), (^2)]
    let values = [1, 2, 3]
    let results = funcs <*> values
    putStrLn $ "Списки Applicative: " ++ show results
    
    -- Either
    let eitherFunc = Right (+10)
    let eitherValue = Right 5
    let eitherResult = eitherFunc <*> eitherValue
    putStrLn $ "Either Applicative: " ++ show eitherResult

-- Полезные функции для аппликативных функторов
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- Использование liftA2
liftExamples :: IO ()
liftExamples = do
    let maybeX = Just 5
    let maybeY = Just 3
    let sum = liftA2 (+) maybeX maybeY
    putStrLn $ "LiftA2 сумма: " ++ show sum
    
    let listX = [1, 2]
    let listY = [10, 20]
    let products = liftA2 (*) listX listY
    putStrLn $ "LiftA2 произведения: " ++ show products
```

---

## 🔮 Монады и их применение

### Основы монад

```haskell
-- Класс Monad
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
    return = pure

-- Реализация для Maybe
instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x

-- Реализация для списков
instance Monad [] where
    xs >>= f = concat (map f xs)

-- Реализация для Either
instance Monad (Either a) where
    Left e >>= _ = Left e
    Right x >>= f = f x

-- Использование монад
monadExamples :: IO ()
monadExamples = do
    -- Maybe
    let maybeValue = Just 5
    let result = maybeValue >>= \x -> 
                 if x > 0 then Just (x * 2) else Nothing
    putStrLn $ "Maybe Monad: " ++ show result
    
    -- Списки
    let numbers = [1, 2, 3]
    let result = numbers >>= \x -> [x, x * 2, x * 3]
    putStrLn $ "Списки Monad: " ++ show result
    
    -- Either
    let eitherValue = Right 10
    let result = eitherValue >>= \x -> 
                 if x > 5 then Right (x * 2) else Left "Слишком мало"
    putStrLn $ "Either Monad: " ++ show result

-- Оператор >> (последовательное выполнение)
(>>) :: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k

-- do-нотация
doNotationExamples :: IO ()
doNotationExamples = do
    -- Простая последовательность
    putStrLn "Первое действие"
    putStrLn "Второе действие"
    
    -- Использование значений
    x <- return 5
    y <- return 3
    putStrLn $ "Сумма: " ++ show (x + y)
    
    -- Условные действия
    if x > y
        then putStrLn "x больше y"
        else putStrLn "y больше или равно x"

-- Монада IO
ioMonadExamples :: IO ()
ioMonadExamples = do
    putStrLn "Введите число:"
    input <- getLine
    let number = read input :: Int
    
    if number > 0
        then do
            putStrLn $ "Положительное число: " ++ show number
            putStrLn $ "Квадрат: " ++ show (number ^ 2)
        else do
            putStrLn $ "Неположительное число: " ++ show number
            putStrLn "Попробуйте снова"
```

### Полезные монадические функции

```haskell
-- sequence - выполняет список монадических действий
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = do
    x <- m
    xs <- sequence ms
    return (x:xs)

-- mapM - применяет функцию к списку и собирает результаты
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

-- forM - mapM с переставленными аргументами
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

-- when - выполняет действие при условии
when :: Monad m => Bool -> m () -> m ()
when True action = action
when False _ = return ()

-- unless - выполняет действие когда условие ложно
unless :: Monad m => Bool -> m () -> m ()
unless True _ = return ()
unless False action = action

-- Использование полезных функций
utilityExamples :: IO ()
utilityExamples = do
    -- sequence
    let actions = [putStrLn "Действие 1", putStrLn "Действие 2"]
    sequence actions
    
    -- mapM
    let numbers = [1..5]
    mapM (\x -> putStrLn $ "Число: " ++ show x) numbers
    
    -- forM
    forM [10, 20, 30] (\x -> putStrLn $ "Значение: " ++ show x)
    
    -- when
    when True (putStrLn "Условие истинно")
    when False (putStrLn "Это не выведется")
    
    -- unless
    unless False (putStrLn "Условие ложно")
    unless True (putStrLn "Это не выведется")
```

---

## 🎨 Типы-классы и их использование

### Основные типы-классы

```haskell
-- Eq - равенство
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)

-- Ord - упорядочивание
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a

-- Show - преобразование в строку
class Show a where
    show :: a -> String
    showsPrec :: Int -> a -> ShowS
    showList :: [a] -> ShowS

-- Read - чтение из строки
class Read a where
    readsPrec :: Int -> ReadS a
    readList :: ReadS [a]

-- Enum - перечисления
class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]

-- Bounded - ограниченные типы
class Bounded a where
    minBound :: a
    maxBound :: a

-- Num - числовые типы
class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

-- Использование типов-классов
typeClassExamples :: IO ()
typeClassExamples = do
    -- Eq
    let a = 5 :: Int
    let b = 5 :: Int
    putStrLn $ "a == b: " ++ show (a == b)
    
    -- Ord
    let x = 3 :: Int
    let y = 7 :: Int
    putStrLn $ "x < y: " ++ show (x < y)
    putStrLn $ "max x y: " ++ show (max x y)
    
    -- Show
    let value = 42 :: Int
    putStrLn $ "Показать: " ++ show value
    
    -- Enum
    let chars = ['a'..'e']
    putStrLn $ "Символы: " ++ show chars
    
    -- Bounded
    putStrLn $ "Min Int: " ++ show (minBound :: Int)
    putStrLn $ "Max Int: " ++ show (maxBound :: Int)
    
    -- Num
    let result = 5 + 3 * 2
    putStrLn $ "Результат: " ++ show result
```

### Создание собственных типов-классов

```haskell
-- Пользовательский тип-класс для геометрических фигур
class Geometric a where
    area :: a -> Double
    perimeter :: a -> Double
    isRegular :: a -> Bool

-- Реализация для круга
data Circle = Circle Double deriving (Show, Eq)

instance Geometric Circle where
    area (Circle r) = pi * r * r
    perimeter (Circle r) = 2 * pi * r
    isRegular _ = True

-- Реализация для прямоугольника
data Rectangle = Rectangle Double Double deriving (Show, Eq)

instance Geometric Rectangle where
    area (Rectangle w h) = w * h
    perimeter (Rectangle w h) = 2 * (w + h)
    isRegular (Rectangle w h) = w == h

-- Реализация для треугольника
data Triangle = Triangle Double Double Double deriving (Show, Eq)

instance Geometric Triangle where
    area (Triangle a b c) = 
        let s = (a + b + c) / 2
        in sqrt (s * (s - a) * (s - b) * (s - c))
    perimeter (Triangle a b c) = a + b + c
    isRegular (Triangle a b c) = a == b && b == c

-- Функции для работы с геометрическими фигурами
describeShape :: Geometric a => a -> String
describeShape shape = 
    "Площадь: " ++ show (area shape) ++ 
    ", Периметр: " ++ show (perimeter shape) ++
    ", Правильная: " ++ show (isRegular shape)

-- Использование
geometricExamples :: IO ()
geometricExamples = do
    let circle = Circle 5.0
    let rectangle = Rectangle 4.0 6.0
    let triangle = Triangle 3.0 4.0 5.0
    
    putStrLn $ "Круг: " ++ describeShape circle
    putStrLn $ "Прямоугольник: " ++ describeShape rectangle
    putStrLn $ "Треугольник: " ++ describeShape triangle
```

---

## 💻 Практические примеры

### Пример 1: Система управления библиотекой

```haskell
-- Типы данных для библиотеки
data Book = Book
    { title :: String
    , author :: String
    , isbn :: String
    , available :: Bool
    } deriving (Show, Eq)

data Reader = Reader
    { readerName :: String
    , readerId :: Int
    , borrowedBooks :: [String]  -- ISBN книг
    } deriving (Show, Eq)

data Library = Library
    { books :: [Book]
    , readers :: [Reader]
    } deriving (Show)

-- Функции для работы с книгами
findBook :: String -> [Book] -> Maybe Book
findBook isbn books = 
    case filter (\b -> isbn == isbn b) books of
        [] -> Nothing
        (b:_) -> Just b

borrowBook :: String -> Int -> Library -> Maybe Library
borrowBook isbn readerId library = do
    book <- findBook isbn (books library)
    guard (available book)
    
    let updatedBook = book { available = False }
    let updatedBooks = map (\b -> if isbn b == isbn then updatedBook else b) (books library)
    
    let reader = findReader readerId (readers library)
    let updatedReader = reader { borrowedBooks = isbn : borrowedBooks reader }
    let updatedReaders = map (\r -> if readerId r == readerId then updatedReader else r) (readers library)
    
    return library { books = updatedBooks, readers = updatedReaders }

findReader :: Int -> [Reader] -> Reader
findReader id readers = 
    case filter (\r -> readerId r == id) readers of
        [] -> Reader "Неизвестный" id []
        (r:_) -> r

-- Использование
libraryExample :: IO ()
libraryExample = do
    let book1 = Book "Война и мир" "Толстой" "123-456" True
    let book2 = Book "Преступление и наказание" "Достоевский" "789-012" True
    
    let reader1 = Reader "Иван" 1 []
    let reader2 = Reader "Мария" 2 []
    
    let library = Library [book1, book2] [reader1, reader2]
    
    putStrLn "Исходная библиотека:"
    print library
    
    case borrowBook "123-456" 1 library of
        Just newLibrary -> do
            putStrLn "\nПосле выдачи книги:"
            print newLibrary
        Nothing -> putStrLn "Ошибка выдачи книги"
```

### Пример 2: Система управления университетом

```haskell
-- Типы данных для университета
data Student = Student
    { studentName :: String
    , studentId :: Int
    , gpa :: Double
    , courses :: [String]
    } deriving (Show, Eq)

data Course = Course
    { courseName :: String
    , courseId :: String
    , credits :: Int
    , enrolledStudents :: [Int]
    } deriving (Show, Eq)

data University = University
    { students :: [Student]
    , courses :: [Course]
    } deriving (Show)

-- Функции для работы со студентами
enrollStudent :: Int -> String -> University -> Maybe University
enrollStudent studentId courseId university = do
    student <- findStudent studentId (students university)
    course <- findCourse courseId (courses university)
    
    let updatedStudent = student { courses = courseId : courses student }
    let updatedCourse = course { enrolledStudents = studentId : enrolledStudents course }
    
    let updatedStudents = map (\s -> if studentId s == studentId then updatedStudent else s) (students university)
    let updatedCourses = map (\c -> if courseId c == courseId then updatedCourse else c) (courses university)
    
    return university { students = updatedStudents, courses = updatedCourses }

findStudent :: Int -> [Student] -> Maybe Student
findStudent id students = 
    case filter (\s -> studentId s == id) students of
        [] -> Nothing
        (s:_) -> Just s

findCourse :: String -> [Course] -> Maybe Course
findCourse id courses = 
    case filter (\c -> courseId c == id) courses of
        [] -> Nothing
        (c:_) -> Just c

-- Статистика университета
getAverageGPA :: University -> Double
getAverageGPA university = 
    let gpas = map gpa (students university)
    in if null gpas then 0.0 else sum gpas / fromIntegral (length gpas)

getTotalCredits :: University -> Int
getTotalCredits university = 
    sum (map credits (courses university))

-- Использование
universityExample :: IO ()
universityExample = do
    let student1 = Student "Иван" 1 3.8 []
    let student2 = Student "Мария" 2 3.9 []
    
    let course1 = Course "Математика" "MATH101" 3 []
    let course2 = Course "Физика" "PHYS101" 4 []
    
    let university = University [student1, student2] [course1, course2]
    
    putStrLn "Исходный университет:"
    print university
    
    case enrollStudent 1 "MATH101" university of
        Just newUniversity -> do
            putStrLn "\nПосле записи студента на курс:"
            print newUniversity
            putStrLn $ "Средний GPA: " ++ show (getAverageGPA newUniversity)
            putStrLn $ "Общее количество кредитов: " ++ show (getTotalCredits newUniversity)
        Nothing -> putStrLn "Ошибка записи на курс"
```

---

## 🧠 Вопросы для самопроверки

### Базовые вопросы
1. **Что такое функтор в Haskell?**
   - Функтор - это тип-класс, который может содержать значения и поддерживает операцию fmap для применения функции к содержимому.

2. **В чем разница между функтором и монадой?**
   - Функтор позволяет применять чистые функции к значениям, монада позволяет связывать вычисления и обрабатывать побочные эффекты.

3. **Что означает оператор >>= в монадах?**
   - Оператор >>= (bind) связывает монадическое значение с функцией, которая возвращает монадическое значение.

### Продвинутые вопросы
4. **Как работают аппликативные функторы?**
   - Аппликативные функторы позволяют применять функции, находящиеся внутри функтора, к значениям, находящимся внутри функтора.

5. **Что такое do-нотация и когда ее использовать?**
   - Do-нотация - это синтаксический сахар для работы с монадами, упрощающий последовательные вычисления.

6. **Как создать собственный тип-класс?**
   - Определить класс с сигнатурами методов, затем создать экземпляры для конкретных типов данных.

---

## 📖 Дополнительные материалы

### Книги
- **"Haskell Programming from First Principles"** - Кристофер Аллен
- **"Real World Haskell"** - Брайан О'Салливан
- **"Category Theory in Context"** - Эмили Рихл

### Онлайн ресурсы
- [Haskell Type Classes](https://wiki.haskell.org/Typeclassopedia)
- [Monad Tutorials](https://wiki.haskell.org/Monad_tutorials)
- [Haskell Functors](https://wiki.haskell.org/Functor)

### Видео курсы
- **"Advanced Haskell"** - YouTube
- **"Category Theory for Programmers"** - Bartosz Milewski
- **"Haskell Monads"** - YouTube

---

## 🎯 Заключение

В данной лекции мы изучили:

✅ **Систему типов Haskell** и алгебраические типы данных  
✅ **Функции и их типы** с полиморфизмом  
✅ **Функторы и аппликативные функторы** для работы с контекстом  
✅ **Монады** для обработки побочных эффектов  
✅ **Типы-классы** для создания абстракций  
✅ **Практические примеры** реальных систем  

### Ключевые моменты для запоминания:
- **Функтор** - базовый способ работы с контекстом
- **Монада** - мощный инструмент для связывания вычислений
- **Типы-классы** - основа для создания абстракций в Haskell
- **Система типов** - обеспечивает безопасность и корректность программ

### Следующая лекция:
**"Монады и ввод-вывод"** - углубленное изучение монад, их применения и работы с вводом-выводом.

---

## 📝 Домашнее задание

1. **Создайте тип данных `Tree`** для бинарного дерева
2. **Реализуйте экземпляр `Functor`** для дерева
3. **Создайте функции** для обхода дерева
4. **Добавьте экземпляр `Show`** для красивого вывода

**Критерии оценки**:
- Правильная реализация типа данных (3 балла)
- Корректный экземпляр Functor (3 балла)
- Функции обхода дерева (2 балла)
- Качество и читаемость кода (2 балла)

**Максимальный балл: 10**
