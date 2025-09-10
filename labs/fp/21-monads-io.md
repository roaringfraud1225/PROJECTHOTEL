# Лабораторная работа 21: Монады и IO

## Цель работы
Изучить концепцию монад в Haskell, включая Maybe, Either, List монады, и освоить работу с вводом-выводом через IO монаду.

## Теоретические основы

### Монады
Монада - это тип-конструктор с двумя операциями:
- **return** - оборачивает значение в монаду
- **>>=** (bind) - связывает вычисления в монаде

### Основные монады
1. **Maybe** - обработка отсутствующих значений
2. **Either** - обработка ошибок
3. **List** - работа со списками
4. **IO** - ввод-вывод
5. **State** - управление состоянием

## Задание

### Часть 1: Maybe монада

Изучите и реализуйте работу с Maybe монадой:

```haskell
-- Базовые операции с Maybe
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0 = Nothing
  | otherwise = Just (sqrt x)

-- Использование Maybe монады
calculateResult :: Int -> Int -> Maybe Double
calculateResult x y = do
  divResult <- safeDiv x y
  sqrtResult <- safeSqrt (fromIntegral divResult)
  return sqrtResult

-- Альтернативная запись с >>=
calculateResult' :: Int -> Int -> Maybe Double
calculateResult' x y = 
  safeDiv x y >>= \divResult ->
  safeSqrt (fromIntegral divResult)

-- Обработка цепочки Maybe операций
processData :: [Maybe Int] -> Maybe [Int]
processData = sequence

-- Фильтрация Just значений
extractJust :: [Maybe a] -> [a]
extractJust = concatMap maybeToList

-- Примеры использования
example1 :: Maybe Int
example1 = do
  x <- Just 10
  y <- Just 5
  z <- safeDiv x y
  return (z * 2)

example2 :: Maybe String
example2 = do
  x <- Just "Hello"
  y <- Just "World"
  return (x ++ " " ++ y)
```

### Часть 2: Either монада

Работа с Either монадой для обработки ошибок:

```haskell
-- Типы ошибок
data CalculationError = 
  DivisionByZero 
  | NegativeNumber 
  | InvalidInput String
  deriving (Show, Eq)

-- Безопасные операции с Either
safeDivEither :: Int -> Int -> Either CalculationError Int
safeDivEither _ 0 = Left DivisionByZero
safeDivEither x y = Right (x `div` y)

safeSqrtEither :: Double -> Either CalculationError Double
safeSqrtEither x
  | x < 0 = Left NegativeNumber
  | otherwise = Right (sqrt x)

-- Цепочка Either операций
calculateResultEither :: Int -> Int -> Either CalculationError Double
calculateResultEither x y = do
  divResult <- safeDivEither x y
  sqrtResult <- safeSqrtEither (fromIntegral divResult)
  return sqrtResult

-- Обработка ошибок
handleError :: Either CalculationError Double -> String
handleError (Left DivisionByZero) = "Ошибка: деление на ноль"
handleError (Left NegativeNumber) = "Ошибка: отрицательное число"
handleError (Left (InvalidInput msg)) = "Ошибка: " ++ msg
handleError (Right result) = "Результат: " ++ show result

-- Комбинирование Either с Maybe
maybeToEither :: String -> Maybe a -> Either String a
maybeToEither errorMsg Nothing = Left errorMsg
maybeToEither _ (Just value) = Right value

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right value) = Just value
```

### Часть 3: List монада

Использование списков как монады:

```haskell
-- Генерация списков с помощью монады
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = do
  a <- [1..10]
  b <- [1..10]
  c <- [1..10]
  guard (a^2 + b^2 == c^2)
  return (a, b, c)

-- Фильтрация с помощью guard
evenSquares :: [Int]
evenSquares = do
  x <- [1..20]
  guard (even x)
  return (x^2)

-- Комбинирование списков
combinations :: [a] -> [b] -> [(a, b)]
combinations xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- Поиск в списке
findElement :: Eq a => a -> [a] -> Maybe Int
findElement _ [] = Nothing
findElement x (y:ys)
  | x == y = Just 0
  | otherwise = case findElement x ys of
      Nothing -> Nothing
      Just n -> Just (n + 1)

-- Альтернативная реализация с монадами
findElementM :: Eq a => a -> [a] -> Maybe Int
findElementM x xs = listToMaybe $ do
  (index, element) <- zip [0..] xs
  guard (x == element)
  return index
```

### Часть 4: IO монада

Работа с вводом-выводом:

```haskell
-- Базовые IO операции
greetUser :: IO ()
greetUser = do
  putStrLn "Как вас зовут?"
  name <- getLine
  putStrLn ("Привет, " ++ name ++ "!")

-- Чтение и запись файлов
readWriteFile :: IO ()
readWriteFile = do
  putStrLn "Введите имя файла для чтения:"
  inputFile <- getLine
  putStrLn "Введите имя файла для записи:"
  outputFile <- getLine
  
  content <- readFile inputFile
  writeFile outputFile (map toUpper content)
  putStrLn "Файл обработан и сохранен!"

-- Интерактивный калькулятор
calculator :: IO ()
calculator = do
  putStrLn "Простой калькулятор"
  putStrLn "Введите первое число:"
  num1Str <- getLine
  putStrLn "Введите операцию (+, -, *, /):"
  op <- getLine
  putStrLn "Введите второе число:"
  num2Str <- getLine
  
  let num1 = read num1Str :: Double
      num2 = read num2Str :: Double
      result = case op of
        "+" -> num1 + num2
        "-" -> num1 - num2
        "*" -> num1 * num2
        "/" -> if num2 /= 0 then num1 / num2 else error "Деление на ноль"
        _ -> error "Неизвестная операция"
  
  putStrLn ("Результат: " ++ show result)

-- Работа с исключениями в IO
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  catch (do
    content <- readFile path
    return (Right content))
    (\e -> return (Left ("Ошибка чтения файла: " ++ show (e :: IOError)))

-- Логирование
logMessage :: String -> IO ()
logMessage msg = do
  timestamp <- getCurrentTime
  let logEntry = show timestamp ++ ": " ++ msg ++ "\n"
  appendFile "app.log" logEntry
  putStrLn msg
```

### Часть 5: Комбинирование монад

Создание сложных вычислений с несколькими монадами:

```haskell
-- Комбинирование Maybe и IO
safeReadAndProcess :: FilePath -> IO (Maybe String)
safeReadAndProcess path = do
  content <- readFile path
  return $ processContent content
  where
    processContent content
      | null content = Nothing
      | otherwise = Just (map toUpper content)

-- Комбинирование Either и IO
safeCalculation :: IO (Either String Double)
safeCalculation = do
  putStrLn "Введите первое число:"
  num1Str <- getLine
  putStrLn "Введите второе число:"
  num2Str <- getLine
  
  let num1 = readEither num1Str :: Either String Double
      num2 = readEither num2Str :: Either String Double
  
  return $ do
    x <- num1
    y <- num2
    if y == 0 
      then Left "Деление на ноль"
      else Right (x / y)

-- Монада State с IO
type GameState = (Int, String)

gameLoop :: StateT GameState IO ()
gameLoop = do
  (score, playerName) <- get
  lift $ putStrLn $ "Игрок: " ++ playerName ++ ", Счет: " ++ show score
  
  lift $ putStrLn "Введите действие (quit/score/name):"
  action <- lift getLine
  
  case action of
    "quit" -> return ()
    "score" -> do
      lift $ putStrLn $ "Текущий счет: " ++ show score
      gameLoop
    "name" -> do
      lift $ putStrLn "Введите новое имя:"
      newName <- lift getLine
      put (score, newName)
      gameLoop
    _ -> do
      lift $ putStrLn "Неизвестное действие"
      gameLoop

-- Запуск игры
runGame :: IO ()
runGame = do
  putStrLn "Введите имя игрока:"
  name <- getLine
  evalStateT gameLoop (0, name)
```

### Часть 6: Пользовательские монады

Создание собственных монад:

```haskell
-- Монада для вычислений с логированием
newtype Logger a = Logger { runLogger :: (a, [String]) }

instance Monad Logger where
  return x = Logger (x, [])
  (Logger (x, logs)) >>= f = 
    let Logger (y, newLogs) = f x
    in Logger (y, logs ++ newLogs)

instance Functor Logger where
  fmap f (Logger (x, logs)) = Logger (f x, logs)

instance Applicative Logger where
  pure = return
  (<*>) = ap

-- Операции для Logger
logMessage :: String -> Logger ()
logMessage msg = Logger ((), [msg])

getLogs :: Logger [String]
getLogs = Logger ([], [])

-- Пример использования Logger
calculateWithLog :: Double -> Double -> Logger Double
calculateWithLog x y = do
  logMessage $ "Начинаем вычисления с x = " ++ show x ++ ", y = " ++ show y
  
  let sum = x + y
  logMessage $ "Сумма: " ++ show sum
  
  let product = x * y
  logMessage $ "Произведение: " ++ show product
  
  let result = sum + product
  logMessage $ "Финальный результат: " ++ show result
  
  return result

-- Монада для вычислений с состоянием
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State f) >>= g = State $ \s ->
    let (x, s') = f s
        State g' = g x
    in g' s'

instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (x, s') = g s
    in (f x, s')

instance Applicative (State s) where
  pure = return
  (<*>) = ap

-- Операции для State
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Пример использования State
counter :: State Int Int
counter = do
  current <- get
  put (current + 1)
  return current

runCounter :: Int -> [Int]
runCounter n = take n $ evalState (sequence (repeat counter)) 0
```

## Требования к реализации

### Обязательные компоненты:
1. **Maybe монада** с методами:
   - Безопасные математические операции
   - Обработка отсутствующих значений
   - Цепочки вычислений

2. **Either монада** с методами:
   - Обработка ошибок
   - Типизированные ошибки
   - Преобразования между монадами

3. **List монада** с методами:
   - Генерация комбинаций
   - Фильтрация с guard
   - Поиск элементов

4. **IO монада** с методами:
   - Интерактивные программы
   - Работа с файлами
   - Обработка исключений

### Дополнительные задания (для получения 5-6 баллов):

1. **Пользовательские монады:**
   - Logger монада для логирования
   - State монада для управления состоянием
   - Reader монада для доступа к окружению

2. **Монадные трансформеры:**
   - StateT для комбинирования State и IO
   - ExceptT для обработки ошибок
   - ReaderT для конфигурации

3. **Продвинутые техники:**
   - do-нотация
   - Монадные законы
   - Оптимизация монадных вычислений

## Пример использования

```haskell
main :: IO ()
main = do
  putStrLn "Демонстрация монад в Haskell"
  
  -- Maybe монада
  putStrLn "\n=== Maybe монада ==="
  print $ calculateResult 10 5
  print $ calculateResult 10 0
  
  -- Either монада
  putStrLn "\n=== Either монада ==="
  print $ calculateResultEither 10 5
  print $ calculateResultEither 10 0
  print $ calculateResultEither (-5) 2
  
  -- List монада
  putStrLn "\n=== List монада ==="
  print $ take 5 pythagoreanTriples
  print $ take 10 evenSquares
  
  -- IO монада
  putStrLn "\n=== IO монада ==="
  greetUser
  
  -- Пользовательские монады
  putStrLn "\n=== Пользовательские монады ==="
  let (result, logs) = runLogger $ calculateWithLog 3.0 4.0
  putStrLn $ "Результат: " ++ show result
  putStrLn "Логи:"
  mapM_ putStrLn logs
  
  putStrLn "\n=== State монада ==="
  print $ runCounter 10
```

## Тестирование

### Базовые тесты:
1. Тестирование Maybe монады
2. Тестирование Either монады
3. Тестирование List монады
4. Тестирование IO операций

### Расширенные тесты:
1. Тестирование пользовательских монад
2. Тестирование монадных трансформеров
3. Тестирование производительности
4. Тестирование монадных законов

## Критерии оценки

- **3 балла:** Базовые монады (Maybe, Either, List, IO)
- **4 балла:** Пользовательские монады
- **5 баллов:** Монадные трансформеры
- **6 баллов:** Продвинутые техники и оптимизация

## Вопросы для самопроверки

1. Что такое монада и зачем она нужна?
2. Как работают Maybe и Either монады?
3. Как использовать do-нотацию?
4. Как создавать пользовательские монады?
5. Что такое монадные трансформеры?

## Дополнительные материалы

- [Haskell Monads Tutorial](https://wiki.haskell.org/Monad)
- [Real World Haskell - Chapter 14](http://book.realworldhaskell.org/read/monads.html)
- [All About Monads](https://wiki.haskell.org/All_About_Monads)

## Следующая лабораторная работа

В следующей работе мы создадим **финальный проект на Haskell**, объединив все изученные концепции функционального программирования.
