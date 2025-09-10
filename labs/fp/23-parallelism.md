# Лабораторная работа 23: Параллелизм в Haskell

## Цель работы
Изучить возможности параллельного и конкурентного программирования в Haskell, включая параллельные вычисления, конкурентность с помощью IO монады и STM.

## Теоретические основы

### Параллелизм vs Конкурентность
- **Параллелизм** - одновременное выполнение вычислений
- **Конкурентность** - управление несколькими задачами

### Основные механизмы
1. **par/pseq** - параллельные вычисления
2. **Control.Parallel.Strategies** - стратегии параллелизма
3. **Control.Concurrent** - конкурентность
4. **STM** - программная транзакционная память

## Задание

### Часть 1: Базовые параллельные вычисления

Изучите и реализуйте параллельные вычисления:

```haskell
import Control.Parallel
import Control.Parallel.Strategies

-- Простые параллельные вычисления
parallelSum :: [Int] -> Int
parallelSum xs = a `par` b `pseq` (a + b)
  where
    a = sum (take half xs)
    b = sum (drop half xs)
    half = length xs `div` 2

-- Параллельное вычисление с par
parallelFib :: Int -> Int
parallelFib n
  | n <= 1 = n
  | otherwise = n1 `par` n2 `pseq` (n1 + n2)
  where
    n1 = parallelFib (n - 1)
    n2 = parallelFib (n - 2)

-- Параллельное вычисление факториала
parallelFactorial :: Int -> Integer
parallelFactorial n
  | n <= 1 = 1
  | otherwise = 
      let half = n `div` 2
          left = parallelFactorial half
          right = parallelFactorial (n - half)
      in left `par` right `pseq` (left * right)

-- Параллельное вычисление с использованием стратегий
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f xs = map f xs `using` parList rseq

-- Параллельная фильтрация
parallelFilter :: (a -> Bool) -> [a] -> [a]
parallelFilter p xs = filter p xs `using` parList rseq

-- Параллельное вычисление скалярного произведения
parallelDotProduct :: [Double] -> [Double] -> Double
parallelDotProduct xs ys = 
  sum (zipWith (*) xs ys) `using` parList rseq

-- Параллельное вычисление матричного умножения
parallelMatrixMult :: [[Double]] -> [[Double]] -> [[Double]]
parallelMatrixMult a b = 
  [[sum [a !! i !! k * b !! k !! j | k <- [0..n-1]] | j <- [0..m-1]] | i <- [0..p-1]]
  `using` parList (parList rseq)
  where
    p = length a
    n = length (head a)
    m = length (head b)

-- Параллельное вычисление с разделением данных
parallelChunked :: Int -> (a -> b) -> [a] -> [b]
parallelChunked chunkSize f xs = 
  concat (map (map f) chunks) `using` parList rseq
  where
    chunks = splitIntoChunks chunkSize xs

splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks n xs = take n xs : splitIntoChunks n (drop n xs)
```

### Часть 2: Стратегии параллелизма

Использование Control.Parallel.Strategies:

```haskell
import Control.Parallel.Strategies

-- Различные стратегии параллелизма
basicStrategy :: [a] -> [a]
basicStrategy xs = xs `using` rseq

parallelStrategy :: [a] -> [a]
parallelStrategy xs = xs `using` parList rseq

deepParallelStrategy :: [a] -> [a]
deepParallelStrategy xs = xs `using` parList (parList rseq)

-- Стратегии для различных типов данных
parallelTuple :: (a, b) -> (a, b)
parallelTuple (x, y) = (x, y) `using` rpar `par` rseq

parallelList :: [a] -> [a]
parallelList xs = xs `using` parList rseq

parallelTree :: Tree a -> Tree a
parallelTree (Node x left right) = 
  (Node x left right) `using` rpar `par` rseq
parallelTree (Leaf x) = Leaf x

-- Пользовательские стратегии
myStrategy :: Strategy a
myStrategy = rpar

parallelWithStrategy :: Strategy a -> [a] -> [a]
parallelWithStrategy strat xs = xs `using` parList strat

-- Стратегии для вложенных структур
nestedParallel :: [[a]] -> [[a]]
nestedParallel xss = xss `using` parList (parList rseq)

-- Стратегии с оценкой
evaluatedStrategy :: [a] -> [a]
evaluatedStrategy xs = xs `using` parList rdeepseq

-- Стратегии для различных типов вычислений
parallelComputation :: [Int] -> [Int]
parallelComputation xs = 
  map expensiveFunction xs `using` parList rseq
  where
    expensiveFunction x = sum [1..x]

-- Стратегии с разделением нагрузки
loadBalancedStrategy :: [a] -> [a]
loadBalancedStrategy xs = 
  let chunks = splitIntoChunks (length xs `div` numCapabilities) xs
  in concat chunks `using` parList rseq

-- Стратегии для рекурсивных структур
parallelRecursive :: [a] -> [a]
parallelRecursive [] = []
parallelRecursive [x] = [x]
parallelRecursive xs = 
  let (left, right) = splitAt (length xs `div` 2) xs
      leftResult = parallelRecursive left
      rightResult = parallelRecursive right
  in leftResult `par` rightResult `pseq` (leftResult ++ rightResult)
```

### Часть 3: Конкурентность с Control.Concurrent

Работа с потоками и конкурентностью:

```haskell
import Control.Concurrent
import Control.Monad
import Data.IORef

-- Простой поток
simpleThread :: IO ()
simpleThread = do
  threadId <- myThreadId
  putStrLn $ "Поток " ++ show threadId ++ " запущен"
  threadDelay 1000000  -- 1 секунда
  putStrLn $ "Поток " ++ show threadId ++ " завершен"

-- Запуск нескольких потоков
runMultipleThreads :: Int -> IO ()
runMultipleThreads n = do
  putStrLn $ "Запуск " ++ show n ++ " потоков"
  replicateM_ n (forkIO simpleThread)
  threadDelay (n * 1000000)  -- Ждем завершения всех потоков
  putStrLn "Все потоки завершены"

-- Поток с параметрами
parameterizedThread :: String -> Int -> IO ()
parameterizedThread name delay = do
  threadId <- myThreadId
  putStrLn $ "Поток " ++ name ++ " (" ++ show threadId ++ ") запущен"
  threadDelay (delay * 100000)
  putStrLn $ "Поток " ++ name ++ " завершен"

-- Управление потоками
threadManagement :: IO ()
threadManagement = do
  putStrLn "Запуск потоков с управлением"
  
  -- Запуск потока
  threadId1 <- forkIO $ parameterizedThread "Worker1" 2
  threadId2 <- forkIO $ parameterizedThread "Worker2" 3
  
  putStrLn $ "Запущены потоки: " ++ show threadId1 ++ ", " ++ show threadId2
  
  -- Ожидание завершения
  threadDelay 5000000  -- 5 секунд
  
  putStrLn "Основной поток завершен"

-- Потоки с возвращаемыми значениями
threadWithResult :: IO String
threadWithResult = do
  threadId <- myThreadId
  putStrLn $ "Поток " ++ show threadId ++ " выполняет вычисления"
  threadDelay 2000000  -- 2 секунды
  return $ "Результат от потока " ++ show threadId

-- Запуск потока с получением результата
runThreadWithResult :: IO ()
runThreadWithResult = do
  putStrLn "Запуск потока с результатом"
  
  -- Создаем IORef для результата
  resultRef <- newIORef ""
  
  -- Запускаем поток
  forkIO $ do
    result <- threadWithResult
    writeIORef resultRef result
  
  -- Ждем результат
  threadDelay 3000000  -- 3 секунды
  
  -- Читаем результат
  result <- readIORef resultRef
  putStrLn $ "Получен результат: " ++ result

-- Потоки с общими данными
sharedDataThreads :: IO ()
sharedDataThreads = do
  putStrLn "Демонстрация потоков с общими данными"
  
  -- Создаем общий счетчик
  counterRef <- newIORef 0
  
  -- Функция для увеличения счетчика
  let incrementCounter = do
        current <- readIORef counterRef
        threadDelay 100000  -- Небольшая задержка
        writeIORef counterRef (current + 1)
        putStrLn $ "Счетчик увеличен до: " ++ show (current + 1)
  
  -- Запускаем несколько потоков
  replicateM_ 5 (forkIO incrementCounter)
  
  -- Ждем завершения
  threadDelay 2000000  -- 2 секунды
  
  -- Проверяем финальное значение
  finalValue <- readIORef counterRef
  putStrLn $ "Финальное значение счетчика: " ++ show finalValue
```

### Часть 4: STM (Software Transactional Memory)

Работа с транзакционной памятью:

```haskell
import Control.Concurrent.STM
import Control.Monad

-- Простой счетчик с STM
type Counter = TVar Int

newCounter :: Int -> STM Counter
newCounter initial = newTVar initial

incrementCounter :: Counter -> STM ()
incrementCounter counter = do
  current <- readTVar counter
  writeTVar counter (current + 1)

decrementCounter :: Counter -> STM ()
decrementCounter counter = do
  current <- readTVar counter
  writeTVar counter (current - 1)

getCounter :: Counter -> STM Int
getCounter counter = readTVar counter

-- Банковский счет с STM
type Account = TVar Double

newAccount :: Double -> STM Account
newAccount balance = newTVar balance

deposit :: Account -> Double -> STM ()
deposit account amount = do
  current <- readTVar account
  writeTVar account (current + amount)

withdraw :: Account -> Double -> STM Bool
withdraw account amount = do
  current <- readTVar account
  if current >= amount
    then do
      writeTVar account (current - amount)
      return True
    else return False

getBalance :: Account -> STM Double
getBalance account = readTVar account

-- Безопасный перевод между счетами
transfer :: Account -> Account -> Double -> STM Bool
transfer from to amount = do
  success <- withdraw from amount
  if success
    then do
      deposit to amount
      return True
    else return False

-- Очередь с STM
type Queue a = TVar [a]

newQueue :: STM (Queue a)
newQueue = newTVar []

enqueue :: Queue a -> a -> STM ()
enqueue queue item = do
  items <- readTVar queue
  writeTVar queue (items ++ [item])

dequeue :: Queue a -> STM (Maybe a)
dequeue queue = do
  items <- readTVar queue
  case items of
    [] -> return Nothing
    (x:xs) -> do
      writeTVar queue xs
      return (Just x)

isEmpty :: Queue a -> STM Bool
isEmpty queue = do
  items <- readTVar queue
  return (null items)

-- Семафор с STM
type Semaphore = TVar Int

newSemaphore :: Int -> STM Semaphore
newSemaphore permits = newTVar permits

acquire :: Semaphore -> STM ()
acquire semaphore = do
  permits <- readTVar semaphore
  if permits > 0
    then writeTVar semaphore (permits - 1)
    else retry

release :: Semaphore -> STM ()
release semaphore = do
  permits <- readTVar semaphore
  writeTVar semaphore (permits + 1)

-- Пример использования семафора
semaphoreExample :: IO ()
semaphoreExample = do
  putStrLn "Демонстрация семафора"
  
  -- Создаем семафор с 2 разрешениями
  semaphore <- atomically $ newSemaphore 2
  
  -- Функция для работы с семафором
  let worker name = do
        putStrLn $ name ++ " пытается получить разрешение"
        atomically $ acquire semaphore
        putStrLn $ name ++ " получил разрешение"
        threadDelay 2000000  -- 2 секунды работы
        atomically $ release semaphore
        putStrLn $ name ++ " освободил разрешение"
  
  -- Запускаем несколько потоков
  forkIO $ worker "Worker1"
  forkIO $ worker "Worker2"
  forkIO $ worker "Worker3"
  forkIO $ worker "Worker4"
  
  -- Ждем завершения
  threadDelay 10000000  -- 10 секунд
  putStrLn "Все потоки завершены"
```

### Часть 5: Параллельные алгоритмы

Реализация параллельных алгоритмов:

```haskell
import Control.Parallel
import Control.Parallel.Strategies
import Data.List

-- Параллельная сортировка слиянием
parallelMergeSort :: Ord a => [a] -> [a]
parallelMergeSort [] = []
parallelMergeSort [x] = [x]
parallelMergeSort xs = 
  let (left, right) = splitAt (length xs `div` 2) xs
      sortedLeft = parallelMergeSort left
      sortedRight = parallelMergeSort right
  in sortedLeft `par` sortedRight `pseq` merge sortedLeft sortedRight

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Параллельная быстрая сортировка
parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort [] = []
parallelQuickSort [x] = [x]
parallelQuickSort (pivot:xs) = 
  let (smaller, larger) = partition (<= pivot) xs
      sortedSmaller = parallelQuickSort smaller
      sortedLarger = parallelQuickSort larger
  in sortedSmaller `par` sortedLarger `pseq` 
     (sortedSmaller ++ [pivot] ++ sortedLarger)

-- Параллельный поиск
parallelSearch :: Eq a => a -> [a] -> Maybe Int
parallelSearch target xs = 
  let chunks = splitIntoChunks (length xs `div` numCapabilities) xs
      searchChunk chunk = findIndex (== target) chunk
      results = map searchChunk chunks
  in case find (/= Nothing) results of
       Just (Just index) -> Just index
       _ -> Nothing

-- Параллельное вычисление числа π
parallelPi :: Int -> Double
parallelPi n = 
  let terms = [4.0 / (1.0 + ((fromIntegral i + 0.5) / fromIntegral n)^2) | i <- [0..n-1]]
      result = sum terms
  in result / fromIntegral n `using` parList rseq

-- Параллельное вычисление интеграла
parallelIntegral :: (Double -> Double) -> Double -> Double -> Int -> Double
parallelIntegral f a b n = 
  let h = (b - a) / fromIntegral n
      points = [a + fromIntegral i * h | i <- [0..n]]
      values = map f points
      result = h * sum values
  in result `using` parList rseq

-- Параллельное вычисление с использованием монады
parallelMonadic :: [Int] -> [Int]
parallelMonadic xs = 
  let results = map expensiveComputation xs
  in results `using` parList rseq
  where
    expensiveComputation x = 
      sum [1..x]  -- Имитация дорогого вычисления

-- Параллельная обработка изображений
type Pixel = (Int, Int, Int)  -- RGB
type Image = [[Pixel]]

parallelImageProcessing :: (Pixel -> Pixel) -> Image -> Image
parallelImageProcessing filterFunc image = 
  let processedRows = map (map filterFunc) image
  in processedRows `using` parList (parList rseq)

-- Фильтр для изображения
brightnessFilter :: Double -> Pixel -> Pixel
brightnessFilter factor (r, g, b) = 
  (min 255 $ round $ fromIntegral r * factor,
   min 255 $ round $ fromIntegral g * factor,
   min 255 $ round $ fromIntegral b * factor)

-- Параллельная обработка с разделением данных
parallelDataProcessing :: Int -> (a -> b) -> [a] -> [b]
parallelDataProcessing numChunks f xs = 
  let chunkSize = (length xs + numChunks - 1) `div` numChunks
      chunks = splitIntoChunks chunkSize xs
      processedChunks = map (map f) chunks
  in concat processedChunks `using` parList rseq
```

### Часть 6: Мониторинг и профилирование

Инструменты для анализа производительности:

```haskell
import Control.Parallel
import Control.Parallel.Strategies
import System.CPUTime
import Text.Printf

-- Измерение времени выполнения
timeIt :: IO a -> IO (a, Double)
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let cpuTime = fromIntegral (end - start) / (10^12)
  return (result, cpuTime)

-- Сравнение последовательного и параллельного выполнения
comparePerformance :: [Int] -> IO ()
comparePerformance xs = do
  putStrLn $ "Тестирование на списке из " ++ show (length xs) ++ " элементов"
  
  -- Последовательное выполнение
  (seqResult, seqTime) <- timeIt $ return $ sum xs
  printf "Последовательное: %.6f сек\n" seqTime
  
  -- Параллельное выполнение
  (parResult, parTime) <- timeIt $ return $ sum xs `using` parList rseq
  printf "Параллельное: %.6f сек\n" parTime
  
  -- Проверка корректности
  if seqResult == parResult
    then printf "Результаты совпадают\n"
    else printf "ОШИБКА: результаты не совпадают\n"
  
  -- Вычисление ускорения
  let speedup = seqTime / parTime
  printf "Ускорение: %.2fx\n" speedup

-- Профилирование параллельных вычислений
profileParallel :: [Int] -> IO ()
profileParallel xs = do
  putStrLn "Профилирование параллельных вычислений"
  
  let computations = [
        ("Последовательное суммирование", sum xs),
        ("Параллельное суммирование", sum xs `using` parList rseq),
        ("Параллельная сортировка", parallelMergeSort xs),
        ("Параллельная фильтрация", filter (> 0) xs `using` parList rseq)
      ]
  
  mapM_ profileComputation computations
  where
    profileComputation (name, computation) = do
      (_, time) <- timeIt $ return computation
      printf "%-30s: %.6f сек\n" name time

-- Мониторинг использования ресурсов
resourceMonitor :: IO ()
resourceMonitor = do
  putStrLn "Мониторинг ресурсов"
  
  -- Получаем количество доступных ядер
  cores <- getNumCapabilities
  putStrLn $ "Доступно ядер: " ++ show cores
  
  -- Получаем информацию о текущем потоке
  threadId <- myThreadId
  putStrLn $ "Текущий поток: " ++ show threadId
  
  -- Создаем несколько потоков для демонстрации
  replicateM_ cores (forkIO $ do
    threadId <- myThreadId
    putStrLn $ "Рабочий поток: " ++ show threadId
    threadDelay 1000000
    putStrLn $ "Поток " ++ show threadId ++ " завершен"
  )
  
  putStrLn "Все потоки запущены"
  threadDelay 2000000
  putStrLn "Мониторинг завершен"

-- Тест производительности
performanceTest :: IO ()
performanceTest = do
  putStrLn "Тест производительности параллельных вычислений"
  
  let testSizes = [1000, 10000, 100000]
  
  mapM_ testSize testSizes
  where
    testSize size = do
      let testData = [1..size]
      putStrLn $ "\nТест с " ++ show size ++ " элементами:"
      
      -- Тест суммирования
      (_, seqTime) <- timeIt $ return $ sum testData
      (_, parTime) <- timeIt $ return $ sum testData `using` parList rseq
      
      printf "Суммирование: последовательное=%.6f, параллельное=%.6f, ускорение=%.2fx\n" 
             seqTime parTime (seqTime / parTime)
      
      -- Тест сортировки
      (_, sortTime) <- timeIt $ return $ parallelMergeSort testData
      printf "Сортировка: %.6f сек\n" sortTime
```

## Требования к реализации

### Обязательные компоненты:
1. **Параллельные вычисления** с методами:
   - Использование par/pseq
   - Стратегии параллелизма
   - Параллельные алгоритмы

2. **Конкурентность** с методами:
   - Управление потоками
   - Общие данные
   - Синхронизация

3. **STM** с методами:
   - Транзакционные переменные
   - Атомарные операции
   - Безопасные структуры данных

### Дополнительные задания (для получения 5-6 баллов):

1. **Продвинутые алгоритмы:**
   - Параллельные алгоритмы сортировки
   - Параллельная обработка изображений
   - Параллельные численные методы

2. **Оптимизация производительности:**
   - Профилирование
   - Настройка стратегий
   - Балансировка нагрузки

3. **Распределенные вычисления:**
   - Работа с несколькими машинами
   - Распределенные алгоритмы
   - Сетевое взаимодействие

## Пример использования

```haskell
main :: IO ()
main = do
  putStrLn "Демонстрация параллелизма в Haskell"
  
  -- Тест базовых параллельных вычислений
  putStrLn "\n=== Базовые параллельные вычисления ==="
  let numbers = [1..1000000]
  (result, time) <- timeIt $ return $ parallelSum numbers
  printf "Параллельная сумма: %d (%.6f сек)\n" result time
  
  -- Тест конкурентности
  putStrLn "\n=== Конкурентность ==="
  runMultipleThreads 4
  
  -- Тест STM
  putStrLn "\n=== STM ==="
  semaphoreExample
  
  -- Тест производительности
  putStrLn "\n=== Тест производительности ==="
  comparePerformance [1..1000000]
  
  -- Мониторинг ресурсов
  putStrLn "\n=== Мониторинг ресурсов ==="
  resourceMonitor
  
  putStrLn "\nДемонстрация завершена!"
```

## Тестирование

### Базовые тесты:
1. Тестирование параллельных вычислений
2. Тестирование конкурентности
3. Тестирование STM
4. Проверка корректности результатов

### Расширенные тесты:
1. Тестирование производительности
2. Тестирование масштабируемости
3. Тестирование устойчивости
4. Профилирование памяти

## Критерии оценки

- **3 балла:** Базовые параллельные вычисления и конкурентность
- **4 балла:** STM и продвинутые алгоритмы
- **5 баллов:** Оптимизация и профилирование
- **6 баллов:** Распределенные вычисления и инновации

## Вопросы для самопроверки

1. В чем разница между параллелизмом и конкурентностью?
2. Как работают стратегии параллелизма?
3. Как STM обеспечивает безопасность?
4. Как измерить эффективность параллельных вычислений?
5. Когда стоит использовать параллелизм?

## Дополнительные материалы

- [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html)
- [Haskell Parallel Programming](https://wiki.haskell.org/Parallel_programming)
- [STM Tutorial](https://wiki.haskell.org/Software_transactional_memory)

## Следующая лабораторная работа

В следующей работе мы проведем **сравнительный анализ** ООП и ФП подходов.
