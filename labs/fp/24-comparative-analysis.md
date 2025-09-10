# Лабораторная работа 24: Сравнительный анализ ООП и ФП

## Цель работы
Провести сравнительный анализ объектно-ориентированного и функционального подходов к программированию, изучив их сильные и слабые стороны на практических примерах.

## Теоретические основы

### Парадигмы программирования
- **ООП:** инкапсуляция, наследование, полиморфизм
- **ФП:** чистые функции, неизменяемость, композиция
- **Гибридные подходы:** сочетание преимуществ обеих парадигм

### Критерии сравнения
1. **Читаемость кода** - понятность и структурированность
2. **Производительность** - скорость выполнения и использование памяти
3. **Безопасность** - предотвращение ошибок и исключений
4. **Масштабируемость** - способность к расширению
5. **Тестируемость** - простота написания тестов

## Задание

### Часть 1: Сравнение базовых концепций

Создайте эквивалентные реализации в Java и Haskell:

#### Задача: Управление библиотекой

**Java (ООП):**
```java
// Книга
public class Book {
    private String title;
    private String author;
    private String isbn;
    private boolean available;
    
    public Book(String title, String author, String isbn) {
        this.title = title;
        this.author = author;
        this.isbn = isbn;
        this.available = true;
    }
    
    // Геттеры и сеттеры
    public String getTitle() { return title; }
    public String getAuthor() { return author; }
    public String getIsbn() { return isbn; }
    public boolean isAvailable() { return available; }
    
    public void setAvailable(boolean available) {
        this.available = available;
    }
    
    @Override
    public String toString() {
        return String.format("Book{title='%s', author='%s', isbn='%s', available=%s}", 
                           title, author, isbn, available);
    }
}

// Библиотека
public class Library {
    private List<Book> books;
    private Map<String, Book> booksByIsbn;
    
    public Library() {
        this.books = new ArrayList<>();
        this.booksByIsbn = new HashMap<>();
    }
    
    public void addBook(Book book) {
        books.add(book);
        booksByIsbn.put(book.getIsbn(), book);
    }
    
    public Book findBookByIsbn(String isbn) {
        return booksByIsbn.get(isbn);
    }
    
    public List<Book> findBooksByAuthor(String author) {
        return books.stream()
                   .filter(book -> book.getAuthor().equals(author))
                   .collect(Collectors.toList());
    }
    
    public boolean borrowBook(String isbn) {
        Book book = findBookByIsbn(isbn);
        if (book != null && book.isAvailable()) {
            book.setAvailable(false);
            return true;
        }
        return false;
    }
    
    public boolean returnBook(String isbn) {
        Book book = findBookByIsbn(isbn);
        if (book != null && !book.isAvailable()) {
            book.setAvailable(true);
            return true;
        }
        return false;
    }
    
    public List<Book> getAvailableBooks() {
        return books.stream()
                   .filter(Book::isAvailable)
                   .collect(Collectors.toList());
    }
    
    public List<Book> getBorrowedBooks() {
        return books.stream()
                   .filter(book -> !book.isAvailable())
                   .collect(Collectors.toList());
    }
}

// Пользователь
public class User {
    private String name;
    private String email;
    private List<Book> borrowedBooks;
    
    public User(String name, String email) {
        this.name = name;
        this.email = email;
        this.borrowedBooks = new ArrayList<>();
    }
    
    public void borrowBook(Book book) {
        if (book.isAvailable()) {
            book.setAvailable(false);
            borrowedBooks.add(book);
        }
    }
    
    public void returnBook(Book book) {
        if (borrowedBooks.contains(book)) {
            book.setAvailable(true);
            borrowedBooks.remove(book);
        }
    }
    
    public List<Book> getBorrowedBooks() {
        return new ArrayList<>(borrowedBooks);
    }
}
```

**Haskell (ФП):**
```haskell
-- Типы данных
data Book = Book {
  title :: String,
  author :: String,
  isbn :: String,
  available :: Bool
} deriving (Show, Eq)

data User = User {
  name :: String,
  email :: String,
  borrowedBooks :: [String]  -- ISBN книг
} deriving (Show, Eq)

type Library = [Book]

-- Функции для работы с книгами
createBook :: String -> String -> String -> Book
createBook title author isbn = Book title author isbn True

isBookAvailable :: Book -> Bool
isBookAvailable = available

markBookAsBorrowed :: Book -> Book
markBookAsBorrowed book = book { available = False }

markBookAsReturned :: Book -> Book
markBookAsReturned book = book { available = True }

-- Функции для работы с библиотекой
addBook :: Book -> Library -> Library
addBook book library = book : library

findBookByIsbn :: String -> Library -> Maybe Book
findBookByIsbn isbn = find (\book -> isbn == isbn book) library

findBooksByAuthor :: String -> Library -> [Book]
findBooksByAuthor author = filter (\book -> author == author book) library

borrowBook :: String -> Library -> Maybe Library
borrowBook isbn library = 
  case findBookByIsbn isbn library of
    Just book -> Just (map (\b -> if isbn b == isbn then markBookAsBorrowed b else b) library)
    Nothing -> Nothing

returnBook :: String -> Library -> Maybe Library
returnBook isbn library = 
  case findBookByIsbn isbn library of
    Just book -> Just (map (\b -> if isbn b == isbn then markBookAsReturned b else b) library)
    Nothing -> Nothing

getAvailableBooks :: Library -> [Book]
getAvailableBooks = filter isBookAvailable

getBorrowedBooks :: Library -> [Book]
getBorrowedBooks = filter (not . isBookAvailable)

-- Функции для работы с пользователями
createUser :: String -> String -> User
createUser name email = User name email []

borrowBookForUser :: String -> User -> Library -> Maybe (User, Library)
borrowBookForUser isbn user library = 
  case borrowBook isbn library of
    Just newLibrary -> Just (user { borrowedBooks = isbn : borrowedBooks user }, newLibrary)
    Nothing -> Nothing

returnBookForUser :: String -> User -> Library -> Maybe (User, Library)
returnBookForUser isbn user library = 
  case returnBook isbn library of
    Just newLibrary -> Just (user { borrowedBooks = filter (/= isbn) (borrowedBooks user) }, newLibrary)
    Nothing -> Nothing

getUserBorrowedBooks :: User -> Library -> [Book]
getUserBorrowedBooks user library = 
  filter (\book -> isbn book `elem` borrowedBooks user) library
```

### Часть 2: Сравнение алгоритмов

Реализуйте алгоритм сортировки в обеих парадигмах:

#### Быстрая сортировка

**Java (ООП):**
```java
public class QuickSort {
    
    public static <T extends Comparable<T>> void sort(List<T> list) {
        if (list.size() <= 1) return;
        
        T pivot = list.get(list.size() / 2);
        List<T> less = new ArrayList<>();
        List<T> equal = new ArrayList<>();
        List<T> greater = new ArrayList<>();
        
        for (T item : list) {
            int comparison = item.compareTo(pivot);
            if (comparison < 0) {
                less.add(item);
            } else if (comparison == 0) {
                equal.add(item);
            } else {
                greater.add(item);
            }
        }
        
        sort(less);
        sort(greater);
        
        list.clear();
        list.addAll(less);
        list.addAll(equal);
        list.addAll(greater);
    }
    
    // Функциональный стиль с потоками
    public static <T extends Comparable<T>> List<T> sortFunctional(List<T> list) {
        if (list.size() <= 1) return new ArrayList<>(list);
        
        T pivot = list.get(list.size() / 2);
        
        List<T> less = list.stream()
                           .filter(item -> item.compareTo(pivot) < 0)
                           .collect(Collectors.toList());
        
        List<T> equal = list.stream()
                            .filter(item -> item.compareTo(pivot) == 0)
                            .collect(Collectors.toList());
        
        List<T> greater = list.stream()
                              .filter(item -> item.compareTo(pivot) > 0)
                              .collect(Collectors.toList());
        
        List<T> result = new ArrayList<>();
        result.addAll(sortFunctional(less));
        result.addAll(equal);
        result.addAll(sortFunctional(greater));
        
        return result;
    }
}
```

**Haskell (ФП):**
```haskell
-- Быстрая сортировка
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (pivot:xs) = 
  quickSort less ++ [pivot] ++ quickSort greater
  where
    less = [x | x <- xs, x <= pivot]
    greater = [x | x <- xs, x > pivot]

-- Быстрая сортировка с использованием filter
quickSortFilter :: Ord a => [a] -> [a]
quickSortFilter [] = []
quickSortFilter [x] = [x]
quickSortFilter (pivot:xs) = 
  quickSortFilter (filter (<= pivot) xs) ++ 
  [pivot] ++ 
  quickSortFilter (filter (> pivot) xs)

-- Быстрая сортировка с использованием монад
quickSortMonadic :: Ord a => [a] -> [a]
quickSortMonadic [] = []
quickSortMonadic [x] = [x]
quickSortMonadic (pivot:xs) = do
  let (less, greater) = partition (<= pivot) xs
  quickSortMonadic less ++ [pivot] ++ quickSortMonadic greater

-- Параллельная быстрая сортировка
parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort [] = []
parallelQuickSort [x] = [x]
parallelQuickSort (pivot:xs) = 
  let (less, greater) = partition (<= pivot) xs
      sortedLess = parallelQuickSort less
      sortedGreater = parallelQuickSort greater
  in sortedLess `par` sortedGreater `pseq` 
     (sortedLess ++ [pivot] ++ sortedGreater)
```

### Часть 3: Сравнение обработки ошибок

#### Обработка деления на ноль

**Java (ООП):**
```java
public class SafeCalculator {
    
    public static class DivisionResult {
        private final boolean success;
        private final double result;
        private final String error;
        
        private DivisionResult(boolean success, double result, String error) {
            this.success = success;
            this.result = result;
            this.error = error;
        }
        
        public static DivisionResult success(double result) {
            return new DivisionResult(true, result, null);
        }
        
        public static DivisionResult error(String error) {
            return new DivisionResult(false, 0.0, error);
        }
        
        public boolean isSuccess() { return success; }
        public double getResult() { return result; }
        public String getError() { return error; }
    }
    
    public static DivisionResult safeDivide(double numerator, double denominator) {
        try {
            if (denominator == 0) {
                return DivisionResult.error("Деление на ноль");
            }
            double result = numerator / denominator;
            return DivisionResult.success(result);
        } catch (Exception e) {
            return DivisionResult.error("Ошибка вычисления: " + e.getMessage());
        }
    }
    
    public static void demonstrateErrorHandling() {
        DivisionResult result1 = safeDivide(10.0, 2.0);
        if (result1.isSuccess()) {
            System.out.println("Результат: " + result1.getResult());
        } else {
            System.out.println("Ошибка: " + result1.getError());
        }
        
        DivisionResult result2 = safeDivide(10.0, 0.0);
        if (result2.isSuccess()) {
            System.out.println("Результат: " + result2.getResult());
        } else {
            System.out.println("Ошибка: " + result2.getError());
        }
    }
}
```

**Haskell (ФП):**
```haskell
-- Обработка ошибок с Maybe
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide numerator denominator = Just (numerator / denominator)

-- Обработка ошибок с Either
safeDivideEither :: Double -> Double -> Either String Double
safeDivideEither _ 0 = Left "Деление на ноль"
safeDivideEither numerator denominator = Right (numerator / denominator)

-- Обработка ошибок с пользовательским типом
data CalculationResult = 
  Success Double
  | Error String
  deriving (Show, Eq)

safeDivideResult :: Double -> Double -> CalculationResult
safeDivideResult _ 0 = Error "Деление на ноль"
safeDivideResult numerator denominator = Success (numerator / denominator)

-- Демонстрация обработки ошибок
demonstrateErrorHandling :: IO ()
demonstrateErrorHandling = do
  putStrLn "Демонстрация обработки ошибок в Haskell"
  
  -- С Maybe
  case safeDivide 10.0 2.0 of
    Just result -> putStrLn $ "Результат: " ++ show result
    Nothing -> putStrLn "Ошибка: деление на ноль"
  
  case safeDivide 10.0 0.0 of
    Just result -> putStrLn $ "Результат: " ++ show result
    Nothing -> putStrLn "Ошибка: деление на ноль"
  
  -- С Either
  case safeDivideEither 10.0 2.0 of
    Right result -> putStrLn $ "Результат: " ++ show result
    Left error -> putStrLn $ "Ошибка: " ++ error
  
  case safeDivideEither 10.0 0.0 of
    Right result -> putStrLn $ "Результат: " ++ show result
    Left error -> putStrLn $ "Ошибка: " ++ error
  
  -- С пользовательским типом
  case safeDivideResult 10.0 2.0 of
    Success result -> putStrLn $ "Результат: " ++ show result
    Error error -> putStrLn $ "Ошибка: " ++ error
  
  case safeDivideResult 10.0 0.0 of
    Success result -> putStrLn $ "Результат: " ++ show result
    Error error -> putStrLn $ "Ошибка: " ++ error
```

### Часть 4: Сравнение производительности

Создайте бенчмарки для сравнения:

**Java (ООП):**
```java
import java.util.*;
import java.util.stream.Collectors;

public class PerformanceBenchmark {
    
    public static void benchmarkSorting() {
        System.out.println("Бенчмарк сортировки");
        
        // Генерация тестовых данных
        List<Integer> data = generateRandomData(100000);
        
        // Тест быстрой сортировки
        long startTime = System.currentTimeMillis();
        List<Integer> sorted = new ArrayList<>(data);
        QuickSort.sort(sorted);
        long endTime = System.currentTimeMillis();
        
        System.out.println("Время сортировки (ООП): " + (endTime - startTime) + " мс");
        
        // Тест функционального стиля
        startTime = System.currentTimeMillis();
        List<Integer> sortedFunctional = QuickSort.sortFunctional(data);
        endTime = System.currentTimeMillis();
        
        System.out.println("Время сортировки (функциональный): " + (endTime - startTime) + " мс");
        
        // Проверка корректности
        boolean isCorrect = sorted.equals(sortedFunctional);
        System.out.println("Результаты совпадают: " + isCorrect);
    }
    
    public static void benchmarkLibraryOperations() {
        System.out.println("\nБенчмарк операций с библиотекой");
        
        // Создание библиотеки
        Library library = new Library();
        
        // Добавление книг
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < 10000; i++) {
            Book book = new Book("Книга " + i, "Автор " + i, "ISBN" + i);
            library.addBook(book);
        }
        long endTime = System.currentTimeMillis();
        
        System.out.println("Время добавления 10000 книг: " + (endTime - startTime) + " мс");
        
        // Поиск книг
        startTime = System.currentTimeMillis();
        for (int i = 0; i < 1000; i++) {
            library.findBookByIsbn("ISBN" + i);
        }
        endTime = System.currentTimeMillis();
        
        System.out.println("Время поиска 1000 книг: " + (endTime - startTime) + " мс");
        
        // Фильтрация книг
        startTime = System.currentTimeMillis();
        List<Book> availableBooks = library.getAvailableBooks();
        endTime = System.currentTimeMillis();
        
        System.out.println("Время фильтрации доступных книг: " + (endTime - startTime) + " мс");
        System.out.println("Количество доступных книг: " + availableBooks.size());
    }
    
    private static List<Integer> generateRandomData(int size) {
        Random random = new Random();
        return random.ints(size, 1, 1000000)
                    .boxed()
                    .collect(Collectors.toList());
    }
    
    public static void main(String[] args) {
        benchmarkSorting();
        benchmarkLibraryOperations();
    }
}
```

**Haskell (ФП):**
```haskell
import System.CPUTime
import Text.Printf
import Data.List
import System.Random

-- Бенчмарк сортировки
benchmarkSorting :: IO ()
benchmarkSorting = do
  putStrLn "Бенчмарк сортировки"
  
  -- Генерация тестовых данных
  let data = take 100000 (randomRs (1, 1000000) (mkStdGen 42))
  
  -- Тест быстрой сортировки
  (_, sortTime) <- timeIt $ return $ quickSort data
  printf "Время сортировки (базовая): %.6f сек\n" sortTime
  
  -- Тест функциональной сортировки
  (_, sortFilterTime) <- timeIt $ return $ quickSortFilter data
  printf "Время сортировки (filter): %.6f сек\n" sortFilterTime
  
  -- Тест параллельной сортировки
  (_, parallelSortTime) <- timeIt $ return $ parallelQuickSort data
  printf "Время сортировки (параллельная): %.6f сек\n" parallelSortTime
  
  -- Проверка корректности
  let sorted1 = quickSort data
      sorted2 = quickSortFilter data
      sorted3 = parallelQuickSort data
  
  putStrLn $ "Результаты совпадают: " ++ show (sorted1 == sorted2 && sorted2 == sorted3)

-- Бенчмарк операций с библиотекой
benchmarkLibraryOperations :: IO ()
benchmarkLibraryOperations = do
  putStrLn "\nБенчмарк операций с библиотекой"
  
  -- Создание библиотеки
  let books = [createBook ("Книга " ++ show i) ("Автор " ++ show i) ("ISBN" ++ show i) | i <- [1..10000]]
  
  -- Добавление книг
  (_, addTime) <- timeIt $ return $ length books
  printf "Время создания 10000 книг: %.6f сек\n" addTime
  
  -- Поиск книг
  let searchOperations = [findBookByIsbn ("ISBN" ++ show i) books | i <- [1..1000]]
  (_, searchTime) <- timeIt $ return $ length searchOperations
  printf "Время поиска 1000 книг: %.6f сек\n" searchTime
  
  -- Фильтрация книг
  (_, filterTime) <- timeIt $ return $ getAvailableBooks books
  printf "Время фильтрации доступных книг: %.6f сек\n" filterTime
  
  let availableCount = length (getAvailableBooks books)
  putStrLn $ "Количество доступных книг: " ++ show availableCount

-- Измерение времени выполнения
timeIt :: IO a -> IO (a, Double)
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let cpuTime = fromIntegral (end - start) / (10^12)
  return (result, cpuTime)

-- Основная функция
main :: IO ()
main = do
  benchmarkSorting
  benchmarkLibraryOperations
```

### Часть 5: Анализ и сравнение

Создайте отчет о сравнении:

```haskell
-- Анализ производительности
performanceAnalysis :: IO ()
performanceAnalysis = do
  putStrLn "=== АНАЛИЗ ПРОИЗВОДИТЕЛЬНОСТИ ==="
  
  -- Тест на различных размерах данных
  let sizes = [1000, 10000, 100000]
  
  mapM_ analyzeSize sizes
  
  where
    analyzeSize size = do
      putStrLn $ "\nРазмер данных: " ++ show size
      
      let data = take size (randomRs (1, 1000000) (mkStdGen 42))
      
      -- Последовательная сортировка
      (_, seqTime) <- timeIt $ return $ quickSort data
      
      -- Параллельная сортировка
      (_, parTime) <- timeIt $ return $ parallelQuickSort data
      
      let speedup = seqTime / parTime
      printf "Последовательная: %.6f сек\n" seqTime
      printf "Параллельная: %.6f сек\n" parTime
      printf "Ускорение: %.2fx\n" speedup

-- Анализ использования памяти
memoryAnalysis :: IO ()
memoryAnalysis = do
  putStrLn "\n=== АНАЛИЗ ПАМЯТИ ==="
  
  -- Тест на больших данных
  let largeData = take 1000000 (randomRs (1, 1000000) (mkStdGen 42))
  
  putStrLn "Тест на 1 миллионе элементов"
  
  -- Измеряем время и косвенно память
  (_, time) <- timeIt $ return $ quickSort largeData
  printf "Время сортировки: %.6f сек\n" time
  
  putStrLn "Примечание: В Haskell управление памятью автоматическое"
  putStrLn "и оптимизировано компилятором GHC"

-- Сравнение подходов
approachComparison :: IO ()
approachComparison = do
  putStrLn "\n=== СРАВНЕНИЕ ПОДХОДОВ ==="
  
  putStrLn "\nООП (Java):"
  putStrLn "  + Инкапсуляция и сокрытие данных"
  putStrLn "  + Наследование и полиморфизм"
  putStrLn "  + Модификация состояния объектов"
  putStrLn "  + Хорошая производительность"
  putStrLn "  - Изменяемое состояние"
  putStrLn "  - Сложность тестирования"
  putStrLn "  - Потенциальные race conditions"
  
  putStrLn "\nФП (Haskell):"
  putStrLn "  + Неизменяемость данных"
  putStrLn "  + Чистые функции"
  putStrLn "  + Легкость тестирования"
  putStrLn "  + Автоматическая оптимизация"
  putStrLn "  - Сложность для новичков"
  putStrLn "  - Менее интуитивное управление состоянием"
  putStrLn "  - Потенциальные проблемы с производительностью"

-- Рекомендации по выбору
recommendations :: IO ()
recommendations = do
  putStrLn "\n=== РЕКОМЕНДАЦИИ ==="
  
  putStrLn "\nИспользуйте ООП когда:"
  putStrLn "  - Работаете с изменяемым состоянием"
  putStrLn "  - Нужна высокая производительность"
  putStrLn "  - Команда знакома с ООП"
  putStrLn "  - Разрабатываете GUI приложения"
  
  putStrLn "\nИспользуйте ФП когда:"
  putStrLn "  - Работаете с неизменяемыми данными"
  putStrLn "  - Нужна высокая надежность"
  putStrLn "  - Разрабатываете математические алгоритмы"
  putStrLn "  - Важна простота тестирования"
  
  putStrLn "\nРассмотрите гибридный подход:"
  putStrLn "  - ООП для структуры приложения"
  putStrLn "  - ФП для алгоритмов и вычислений"
  putStrLn "  - Использование функциональных возможностей в ООП языках"
```

## Требования к реализации

### Обязательные компоненты:
1. **Эквивалентные реализации** с методами:
   - Библиотечная система
   - Алгоритм сортировки
   - Обработка ошибок

2. **Бенчмарки производительности** с методами:
   - Измерение времени выполнения
   - Сравнение подходов
   - Анализ результатов

3. **Сравнительный анализ** с методами:
   - Критерии сравнения
   - Выводы и рекомендации
   - Практические примеры

### Дополнительные задания (для получения 5-6 баллов):

1. **Расширенный анализ:**
   - Анализ использования памяти
   - Тестирование масштабируемости
   - Сравнение различных алгоритмов

2. **Гибридные подходы:**
   - Функциональное программирование в Java
   - ООП концепции в Haskell
   - Лучшие практики комбинирования

3. **Практические рекомендации:**
   - Выбор парадигмы для проекта
   - Миграция между подходами
   - Обучение команды

## Пример использования

```haskell
main :: IO ()
main = do
  putStrLn "Сравнительный анализ ООП и ФП подходов"
  putStrLn "======================================="
  
  -- Запуск бенчмарков
  benchmarkSorting
  benchmarkLibraryOperations
  
  -- Анализ производительности
  performanceAnalysis
  
  -- Анализ памяти
  memoryAnalysis
  
  -- Сравнение подходов
  approachComparison
  
  -- Рекомендации
  recommendations
  
  putStrLn "\nАнализ завершен!"
```

## Тестирование

### Базовые тесты:
1. Корректность эквивалентных реализаций
2. Производительность бенчмарков
3. Сравнение результатов
4. Валидация выводов

### Расширенные тесты:
1. Тестирование на различных данных
2. Анализ масштабируемости
3. Сравнение с эталонными реализациями
4. Валидация рекомендаций

## Критерии оценки

- **3 балла:** Базовые эквивалентные реализации и бенчмарки
- **4 балла:** Расширенный анализ производительности
- **5 баллов:** Гибридные подходы и практические рекомендации
- **6 баллов:** Инновационные решения и глубокий анализ

## Вопросы для самопроверки

1. Какие преимущества дает каждая парадигма?
2. Как измерить производительность разных подходов?
3. Когда стоит использовать гибридные решения?
4. Как выбрать парадигму для проекта?
5. Какие тренды в развитии парадигм?

## Заключение

Сравнительный анализ демонстрирует:
- Понимание сильных и слабых сторон парадигм
- Умение выбирать подходящий подход
- Способность к объективной оценке
- Знание практических аспектов разработки
- Готовность к принятию архитектурных решений

Обе парадигмы имеют свои преимущества, и выбор должен основываться на конкретных требованиях проекта.
