# Лабораторная работа 22: Финальный проект на Haskell

## Цель работы
Создать полноценное функциональное приложение на Haskell, объединив все изученные концепции ФП, включая чистые функции, монады, типы данных и функторы.

## Описание проекта

### Функциональный калькулятор выражений
Полноценное приложение для вычисления математических выражений с поддержкой:
- Парсинга и вычисления выражений
- Обработки ошибок через монады
- Интерактивного интерфейса
- Сохранения истории вычислений
- Расширенных математических функций

## Архитектура проекта

### Структура модулей
```
src/
├── Main.hs              # Главный модуль
├── Parser.hs            # Парсер выражений
├── Evaluator.hs         # Вычислитель
├── Types.hs             # Типы данных
├── MathFunctions.hs     # Математические функции
├── History.hs           # История вычислений
├── IO.hs                # Ввод-вывод
└── Utils.hs             # Вспомогательные функции
```

### Основные компоненты
1. **Parser** - парсинг математических выражений
2. **Evaluator** - вычисление выражений
3. **MathFunctions** - библиотека математических функций
4. **History** - управление историей вычислений
5. **IO** - интерактивный интерфейс

## Реализация

### 1. Главный модуль

```haskell
module Main where

import Parser
import Evaluator
import MathFunctions
import History
import IO
import Types

main :: IO ()
main = do
  putStrLn "Функциональный калькулятор выражений"
  putStrLn "Введите 'help' для справки, 'quit' для выхода"
  runCalculator emptyHistory

runCalculator :: History -> IO ()
runCalculator history = do
  putStr "calc> "
  input <- getLine
  
  case input of
    "quit" -> putStrLn "До свидания!"
    "help" -> showHelp >> runCalculator history
    "history" -> showHistory history >> runCalculator history
    "clear" -> runCalculator emptyHistory
    _ -> do
      let result = processInput input
      case result of
        Left error -> do
          putStrLn $ "Ошибка: " ++ error
          runCalculator history
        Right (value, newHistory) -> do
          putStrLn $ "Результат: " ++ show value
          runCalculator newHistory

showHelp :: IO ()
showHelp = do
  putStrLn "Доступные команды:"
  putStrLn "  help     - показать эту справку"
  putStrLn "  history  - показать историю вычислений"
  putStrLn "  clear    - очистить историю"
  putStrLn "  quit     - выйти из программы"
  putStrLn "  <выражение> - вычислить выражение"
  putStrLn ""
  putStrLn "Поддерживаемые операции:"
  putStrLn "  +, -, *, /, ^, sqrt, sin, cos, log"
```

### 2. Типы данных

```haskell
module Types where

-- Типы выражений
data Expr = 
  Number Double
  | Var String
  | BinOp Op Expr Expr
  | UnaryOp UnaryOp Expr
  | Function String Expr
  deriving (Show, Eq)

-- Бинарные операции
data Op = Add | Sub | Mul | Div | Pow deriving (Show, Eq)

-- Унарные операции
data UnaryOp = Neg | Sqrt deriving (Show, Eq)

-- Результат вычисления
data CalculationResult = 
  Success Double
  | Error String
  deriving (Show, Eq)

-- История вычислений
data HistoryEntry = HistoryEntry {
  expression :: String,
  result :: Double,
  timestamp :: String
} deriving (Show, Eq)

newtype History = History [HistoryEntry] deriving (Show, Eq)

-- Контекст вычислений
data Context = Context {
  variables :: [(String, Double)],
  functions :: [(String, Double -> Double)]
} deriving (Show)

-- Операции с историей
emptyHistory :: History
emptyHistory = History []

addToHistory :: String -> Double -> History -> History
addToHistory expr result (History entries) = 
  History (HistoryEntry expr result (show ()) : entries)

getHistoryEntries :: History -> [HistoryEntry]
getHistoryEntries (History entries) = entries

-- Операции с контекстом
emptyContext :: Context
emptyContext = Context [] []

addVariable :: String -> Double -> Context -> Context
addVariable name value (Context vars funcs) = 
  Context ((name, value) : vars) funcs

addFunction :: String -> (Double -> Double) -> Context -> Context
addFunction name func (Context vars funcs) = 
  Context vars ((name, func) : funcs)

lookupVariable :: String -> Context -> Maybe Double
lookupVariable name (Context vars _) = lookup name vars

lookupFunction :: String -> Context -> Maybe (Double -> Double)
lookupFunction name (Context _ funcs) = lookup name funcs
```

### 3. Парсер выражений

```haskell
module Parser where

import Types
import Text.ParserCombinators.Parsec
import Control.Monad

-- Парсер математических выражений
parseExpression :: String -> Either String Expr
parseExpression input = case parse exprParser "" input of
  Left error -> Left $ "Ошибка парсинга: " ++ show error
  Right expr -> Right expr

-- Основной парсер выражений
exprParser :: Parser Expr
exprParser = term `chainl1` addOp

-- Парсер термов (умножение, деление)
term :: Parser Expr
term = factor `chainl1` mulOp

-- Парсер факторов (степень, унарные операции)
factor :: Parser Expr
factor = power `chainl1` powOp

-- Парсер степеней
power :: Parser Expr
power = do
  base <- unary
  rest <- optionMaybe (char '^' >> power)
  case rest of
    Nothing -> return base
    Just exp -> return $ BinOp Pow base exp

-- Парсер унарных операций
unary :: Parser Expr
unary = do
  op <- optionMaybe (char '-' <|> char '+')
  case op of
    Just '-' -> UnaryOp Neg <$> unary
    Just '+' -> unary
    Nothing -> primary

-- Парсер первичных выражений
primary :: Parser Expr
primary = 
  try functionCall
  <|> try variable
  <|> try number
  <|> parens

-- Парсер вызовов функций
functionCall :: Parser Expr
functionCall = do
  name <- functionName
  char '('
  arg <- exprParser
  char ')'
  return $ Function name arg

-- Парсер имен функций
functionName :: Parser String
functionName = many1 letter

-- Парсер переменных
variable :: Parser Expr
variable = Var <$> many1 letter

-- Парсер чисел
number :: Parser Expr
number = Number <$> double

-- Парсер скобок
parens :: Parser Expr
parens = char '(' *> exprParser <* char ')'

-- Парсер операторов
addOp :: Parser (Expr -> Expr -> Expr)
addOp = 
  char '+' >> return (BinOp Add)
  <|> char '-' >> return (BinOp Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = 
  char '*' >> return (BinOp Mul)
  <|> char '/' >> return (BinOp Div)

powOp :: Parser (Expr -> Expr -> Expr)
powOp = char '^' >> return (BinOp Pow)

-- Вспомогательные функции
double :: Parser Double
double = do
  whole <- many1 digit
  decimal <- option "" (char '.' >> many1 digit)
  return $ read (whole ++ "." ++ decimal)

-- Цепочка левоассоциативных операторов
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> return x
```

### 4. Вычислитель выражений

```haskell
module Evaluator where

import Types
import MathFunctions
import Control.Monad.Except

-- Вычисление выражения в контексте
evaluate :: Expr -> Context -> Either String Double
evaluate expr context = runExcept $ eval expr context

-- Монада для вычислений с обработкой ошибок
type EvalM = Except String

-- Основная функция вычисления
eval :: Expr -> Context -> EvalM Double
eval (Number n) _ = return n

eval (Var name) context = case lookupVariable name context of
  Just value -> return value
  Nothing -> throwError $ "Неизвестная переменная: " ++ name

eval (BinOp op left right) context = do
  leftVal <- eval left context
  rightVal <- eval right context
  case op of
    Add -> return $ leftVal + rightVal
    Sub -> return $ leftVal - rightVal
    Mul -> return $ leftVal * rightVal
    Div -> if rightVal == 0 
           then throwError "Деление на ноль"
           else return $ leftVal / rightVal
    Pow -> return $ leftVal ** rightVal

eval (UnaryOp op expr) context = do
  value <- eval expr context
  case op of
    Neg -> return $ -value
    Sqrt -> if value < 0 
            then throwError "Квадратный корень из отрицательного числа"
            else return $ sqrt value

eval (Function name arg) context = do
  argVal <- eval arg context
  case lookupFunction name context of
    Just func -> return $ func argVal
    Nothing -> case lookup name builtinFunctions of
      Just func -> return $ func argVal
      Nothing -> throwError $ "Неизвестная функция: " ++ name

-- Встроенные функции
builtinFunctions :: [(String, Double -> Double)]
builtinFunctions = [
  ("sin", sin),
  ("cos", cos),
  ("tan", tan),
  ("log", log),
  ("exp", exp),
  ("abs", abs),
  ("floor", fromIntegral . floor),
  ("ceil", fromIntegral . ceiling)
]

-- Проверка корректности выражения
validateExpression :: Expr -> Either String ()
validateExpression (Number _) = return ()
validateExpression (Var _) = return ()
validateExpression (BinOp _ left right) = do
  validateExpression left
  validateExpression right
validateExpression (UnaryOp _ expr) = validateExpression expr
validateExpression (Function _ arg) = validateExpression arg

-- Оптимизация выражений
optimize :: Expr -> Expr
optimize (Number n) = Number n
optimize (Var name) = Var name
optimize (BinOp op left right) = 
  let optLeft = optimize left
      optRight = optimize right
  in case (optLeft, optRight) of
    (Number 0, _) -> case op of
      Add -> optRight
      Mul -> Number 0
      _ -> BinOp op optLeft optRight
    (_, Number 0) -> case op of
      Add -> optLeft
      Mul -> Number 0
      _ -> BinOp op optLeft optRight
    (Number 1, _) -> case op of
      Mul -> optRight
      _ -> BinOp op optLeft optRight
    (_, Number 1) -> case op of
      Mul -> optLeft
      Pow -> optLeft
      _ -> BinOp op optLeft optRight
    _ -> BinOp op optLeft optRight

optimize (UnaryOp op expr) = 
  let optExpr = optimize expr
  in case optExpr of
    Number n -> case op of
      Neg -> Number (-n)
      Sqrt -> Number (sqrt n)
    _ -> UnaryOp op optExpr

optimize (Function name arg) = 
  Function name (optimize arg)
```

### 5. Математические функции

```haskell
module MathFunctions where

import Types

-- Расширенные математические функции
factorial :: Double -> Double
factorial n
  | n < 0 = error "Факториал отрицательного числа не определен"
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * factorial (n - 1)

-- Биномиальный коэффициент
binomial :: Double -> Double -> Double
binomial n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = factorial n / (factorial k * factorial (n - k))

-- Наибольший общий делитель
gcd' :: Double -> Double -> Double
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod'` b)
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

-- Наименьшее общее кратное
lcm' :: Double -> Double -> Double
lcm' a b = abs (a * b) / gcd' a b

-- Проверка простоты числа
isPrime :: Double -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = not $ any (\d -> n `mod'` d == 0) [3, 5..sqrt' n]
  where
    sqrt' = ceiling . sqrt
    mod' x y = x - y * fromIntegral (floor (x / y))

-- Следующее простое число
nextPrime :: Double -> Double
nextPrime n
  | n < 2 = 2
  | isPrime (n + 1) = n + 1
  | otherwise = nextPrime (n + 1)

-- Предыдущее простое число
prevPrime :: Double -> Double
prevPrime n
  | n <= 2 = error "Нет предыдущего простого числа"
  | isPrime (n - 1) = n - 1
  | otherwise = prevPrime (n - 1)

-- Сумма делителей
sumDivisors :: Double -> Double
sumDivisors n = sum [d | d <- [1..n], n `mod'` d == 0]
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

-- Количество делителей
countDivisors :: Double -> Double
countDivisors n = fromIntegral $ length [d | d <- [1..n], n `mod'` d == 0]
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

-- Проверка совершенного числа
isPerfect :: Double -> Bool
isPerfect n = sumDivisors n == 2 * n

-- Проверка дружественных чисел
areAmicable :: Double -> Double -> Bool
areAmicable a b = 
  a /= b && sumDivisors a == b && sumDivisors b == a

-- Числа Фибоначчи
fibonacci :: Double -> Double
fibonacci n
  | n < 0 = error "Числа Фибоначчи не определены для отрицательных чисел"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- Быстрое вычисление чисел Фибоначчи
fibonacciFast :: Double -> Double
fibonacciFast n
  | n < 0 = error "Числа Фибоначчи не определены для отрицательных чисел"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = snd $ fibHelper n
  where
    fibHelper :: Double -> (Double, Double)
    fibHelper 0 = (0, 1)
    fibHelper n
      | even n = (a, b)
      | otherwise = (b, a + b)
      where
        (a, b) = fibHelper (n `div` 2)
        b' = a + b
        a' = b' - a
```

### 6. Управление историей

```haskell
module History where

import Types
import Data.Time
import Data.List

-- Добавление записи в историю
addCalculation :: String -> Double -> History -> IO History
addCalculation expr result history = do
  now <- getCurrentTime
  let entry = HistoryEntry expr result (show now)
  return $ addToHistory expr result history

-- Показать историю
showHistory :: History -> IO ()
showHistory history = do
  let entries = getHistoryEntries history
  if null entries
    then putStrLn "История пуста"
    else do
      putStrLn "История вычислений:"
      mapM_ showEntry (reverse entries)
  where
    showEntry (HistoryEntry expr result timestamp) = do
      putStrLn $ "  " ++ expr ++ " = " ++ show result ++ " (" ++ timestamp ++ ")"

-- Поиск в истории
searchHistory :: String -> History -> [HistoryEntry]
searchHistory query (History entries) = 
  filter (\entry -> query `isInfixOf` expression entry) entries

-- Фильтрация по результату
filterByResult :: (Double -> Bool) -> History -> [HistoryEntry]
filterByResult predicate (History entries) = 
  filter (\entry -> predicate (result entry)) entries

-- Статистика истории
historyStats :: History -> (Int, Double, Double, Double)
historyStats (History entries) = 
  let results = map result entries
      count = length results
      sum' = sum results
      avg = if count > 0 then sum' / fromIntegral count else 0
      max' = maximum results
  in (count, sum', avg, max')

-- Экспорт истории в файл
exportHistory :: FilePath -> History -> IO ()
exportHistory filepath (History entries) = do
  let content = unlines $ map formatEntry entries
  writeFile filepath content
  where
    formatEntry (HistoryEntry expr result timestamp) = 
      expr ++ "," ++ show result ++ "," ++ timestamp

-- Импорт истории из файла
importHistory :: FilePath -> IO History
importHistory filepath = do
  content <- readFile filepath
  let lines' = lines content
  let entries = map parseEntry lines'
  return $ History entries
  where
    parseEntry line = 
      let parts = splitOn ',' line
      in case parts of
        [expr, resultStr, timestamp] -> 
          HistoryEntry expr (read resultStr) timestamp
        _ -> error "Неверный формат записи"

-- Разделение строки по разделителю
splitOn :: Char -> String -> [String]
splitOn delimiter = words . map (\c -> if c == delimiter then ' ' else c)

-- Очистка истории
clearHistory :: History
clearHistory = History []

-- Удаление дубликатов
removeDuplicates :: History -> History
removeDuplicates (History entries) = 
  History $ nubBy (\a b -> expression a == expression b && result a == result b) entries
```

### 7. Модуль ввода-вывода

```haskell
module IO where

import Types
import Parser
import Evaluator
import History
import MathFunctions
import Control.Monad.Except

-- Обработка пользовательского ввода
processInput :: String -> Either String (Double, History)
processInput input = do
  let trimmed = trim input
  if null trimmed
    then Left "Пустой ввод"
    else do
      expr <- parseExpression trimmed
      validateExpression expr
      let optimizedExpr = optimize expr
      result <- evaluate optimizedExpr emptyContext
      return (result, addToHistory trimmed result emptyHistory)

-- Удаление пробелов
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Интерактивный режим
interactiveMode :: IO ()
interactiveMode = do
  putStrLn "Введите математическое выражение:"
  input <- getLine
  
  case processInput input of
    Left error -> do
      putStrLn $ "Ошибка: " ++ error
      interactiveMode
    Right (result, _) -> do
      putStrLn $ "Результат: " ++ show result
      putStrLn "Продолжить? (y/n)"
      continue <- getLine
      case continue of
        "y" -> interactiveMode
        "n" -> putStrLn "До свидания!"
        _ -> do
          putStrLn "Неверный ввод. Продолжаем..."
          interactiveMode

-- Файловый режим
fileMode :: FilePath -> IO ()
fileMode filepath = do
  content <- readFile filepath
  let lines' = lines content
  results <- mapM processLine lines'
  mapM_ printResult (zip lines' results)
  where
    processLine line = 
      case processInput line of
        Left error -> Left error
        Right (result, _) -> Right result
    
    printResult (line, result) = case result of
      Left error -> putStrLn $ line ++ " -> Ошибка: " ++ error
      Right value -> putStrLn $ line ++ " = " ++ show value

-- Режим тестирования
testMode :: IO ()
testMode = do
  putStrLn "Запуск тестов..."
  
  let testCases = [
        ("2 + 2", 4),
        ("10 * 5", 50),
        ("100 / 4", 25),
        ("2 ^ 10", 1024),
        ("sqrt(16)", 4),
        ("sin(0)", 0),
        ("cos(0)", 1),
        ("log(1)", 0)
      ]
  
  results <- mapM runTest testCases
  let passed = length $ filter id results
  let total = length testCases
  
  putStrLn $ "Тесты пройдены: " ++ show passed ++ "/" ++ show total
  
  where
    runTest (expr, expected) = do
      case processInput expr of
        Left error -> do
          putStrLn $ "ОШИБКА: " ++ expr ++ " -> " ++ error
          return False
        Right (result, _) -> do
          let success = abs (result - expected) < 0.0001
          if success
            then putStrLn $ "OK: " ++ expr ++ " = " ++ show result
            else putStrLn $ "FAIL: " ++ expr ++ " ожидалось " ++ show expected ++ ", получено " ++ show result
          return success

-- Режим демонстрации
demoMode :: IO ()
demoMode = do
  putStrLn "Демонстрация возможностей калькулятора:"
  putStrLn ""
  
  let demos = [
        ("2 + 3 * 4", "Демонстрация приоритета операций"),
        ("(2 + 3) * 4", "Демонстрация скобок"),
        ("sqrt(16) + 2^3", "Демонстрация функций"),
        ("sin(0) + cos(0)", "Демонстрация тригонометрии"),
        ("factorial(5)", "Демонстрация факториала"),
        ("fibonacci(10)", "Демонстрация чисел Фибоначчи")
      ]
  
  mapM_ runDemo demos
  
  where
    runDemo (expr, description) = do
      putStrLn $ description ++ ":"
      putStrLn $ "  " ++ expr
      case processInput expr of
        Left error -> putStrLn $ "  Ошибка: " ++ error
        Right (result, _) -> putStrLn $ "  = " ++ show result
      putStrLn ""
```

## Тестирование

### Основные тесты
```haskell
module Tests where

import Test.HUnit
import Types
import Parser
import Evaluator
import MathFunctions

-- Тесты парсера
parserTests :: Test
parserTests = TestList [
  TestLabel "parse number" $ TestCase $ 
    assertEqual "should parse number" (Right (Number 42)) (parseExpression "42"),
  
  TestLabel "parse addition" $ TestCase $ 
    assertEqual "should parse addition" 
      (Right (BinOp Add (Number 2) (Number 3))) 
      (parseExpression "2+3"),
  
  TestLabel "parse function" $ TestCase $ 
    assertEqual "should parse function" 
      (Right (Function "sin" (Number 0))) 
      (parseExpression "sin(0)")
  ]

-- Тесты вычислителя
evaluatorTests :: Test
evaluatorTests = TestList [
  TestLabel "evaluate number" $ TestCase $ 
    assertEqual "should evaluate number" (Right 42) (evaluate (Number 42) emptyContext),
  
  TestLabel "evaluate addition" $ TestCase $ 
    assertEqual "should evaluate addition" (Right 5) 
      (evaluate (BinOp Add (Number 2) (Number 3)) emptyContext),
  
  TestLabel "evaluate division by zero" $ TestCase $ 
    assertEqual "should handle division by zero" (Left "Деление на ноль") 
      (evaluate (BinOp Div (Number 1) (Number 0)) emptyContext)
  ]

-- Тесты математических функций
mathTests :: Test
mathTests = TestList [
  TestLabel "factorial" $ TestCase $ 
    assertEqual "should calculate factorial" 120 (factorial 5),
  
  TestLabel "fibonacci" $ TestCase $ 
    assertEqual "should calculate fibonacci" 55 (fibonacci 10),
  
  TestLabel "isPrime" $ TestCase $ 
    assertBool "should identify prime numbers" (isPrime 17)
  ]

-- Запуск всех тестов
runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [
  parserTests,
  evaluatorTests,
  mathTests
  ]
```

## Запуск и демонстрация

### Основные функции для демонстрации:
1. **Парсинг выражений** - различные математические выражения
2. **Вычисление** - корректные и ошибочные выражения
3. **Математические функции** - встроенные и пользовательские
4. **История вычислений** - сохранение и просмотр
5. **Обработка ошибок** - различные типы ошибок
6. **Интерактивный режим** - пользовательский ввод

### Требования к демонстрации:
- Рабочий парсер математических выражений
- Корректное вычисление выражений
- Обработка ошибок через монады
- Интерактивный интерфейс
- Объяснение функционального подхода

## Критерии оценки

- **3 балла:** Базовый парсер и вычислитель
- **4 балла:** Обработка ошибок и математические функции
- **5 баллов:** История вычислений и интерактивный режим
- **6 баллов:** Расширенные возможности и оптимизация

## Вопросы для самопроверки

1. Как функциональный подход упрощает парсинг?
2. Как монады помогают в обработке ошибок?
3. Как организовать чистые вычисления?
4. Как управлять состоянием в функциональном стиле?
5. Какие преимущества дает функциональное программирование?

## Заключение

Финальный проект демонстрирует:
- Владение функциональным программированием
- Понимание монад и типов
- Умение создавать чистые функции
- Качество кода и тестирования
- Способность к комплексной разработке

Проект готов к дальнейшему развитию и может стать основой для математического программного обеспечения.
