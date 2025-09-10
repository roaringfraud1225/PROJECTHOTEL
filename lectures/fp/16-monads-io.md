---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Монады и IO
## Лекция 16: Работа с побочными эффектами

**Преподаватель:** [Ваше имя]  
**Группа:** 203  
**Семестр:** Осенний 2024

---

# План лекции

1. **Что такое монады?**
2. **Maybe монада**
3. **IO монада**
4. **Работа с файлами**
5. **Обработка ошибок**
6. **Практический пример: Игровой движок**

---

# Что такое монады?

## Определение:
**Монада** — это способ структурирования вычислений с побочными эффектами в чистом функциональном языке.

## Основные операции:
- **return** — помещает значение в монадический контекст
- **>>=** (bind) — связывает монадические вычисления
- **>>** — связывает монадические вычисления без передачи значения

## Преимущества:
- **Чистота** функций сохраняется
- **Композиция** вычислений с эффектами
- **Обработка ошибок** и исключений
- **Последовательность** операций

---

# Maybe монада

## Что такое Maybe?
**Maybe** — монада для представления вычислений, которые могут не дать результата.

## Определение:
```haskell
data Maybe a = Nothing | Just a
    deriving (Show, Eq)

-- Maybe является монадой
instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    Just x >>= f = f x
```

## Использование:
```haskell
-- Безопасное деление
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- Безопасное извлечение элемента
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Безопасное извлечение из списка
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
    | n < 0 = Nothing
    | otherwise = safeIndex xs (n - 1)

-- Композиция Maybe вычислений
calculateUnitDamage :: Unit -> Unit -> Maybe Int
calculateUnitDamage attacker target = do
    attackPower <- getAttackPower attacker
    defense <- getDefense target
    return (max 1 (attackPower - defense))

-- Альтернативная запись с >>=
calculateUnitDamage' :: Unit -> Unit -> Maybe Int
calculateUnitDamage' attacker target = 
    getAttackPower attacker >>= \attackPower ->
    getDefense target >>= \defense ->
    return (max 1 (attackPower - defense))
```

---

# Maybe в игровой логике

```haskell
-- Функции для работы с Maybe в игре
findUnitByName :: String -> [Unit] -> Maybe Unit
findUnitByName name units = 
    case filter (\unit -> unitName unit == name) units of
        [] -> Nothing
        (unit:_) -> Just unit

findUnitAtPosition :: Position -> [Unit] -> Maybe Unit
findUnitAtPosition pos units = 
    case filter (\unit -> unitPosition unit == pos) units of
        [] -> Nothing
        (unit:_) -> Just unit

-- Безопасное движение юнита
safeMoveUnit :: Unit -> Position -> [Unit] -> Maybe Unit
safeMoveUnit unit newPos allUnits = do
    -- Проверяем, что позиция свободна
    guard (isNothing (findUnitAtPosition newPos allUnits))
    -- Проверяем, что юнит может двигаться
    guard (canMoveTo unit newPos)
    -- Возвращаем обновленный юнит
    return unit { unitPosition = newPos }

-- Безопасная атака
safeAttack :: Unit -> Unit -> Maybe Int
safeAttack attacker target = do
    -- Проверяем, что атакующий жив
    guard (isAlive attacker)
    -- Проверяем, что цель жива
    guard (isAlive target)
    -- Проверяем, что атака возможна
    guard (canAttack attacker target)
    -- Вычисляем урон
    damage <- calculateDamage attacker target
    return damage

-- Безопасное получение информации
getUnitInfo :: String -> [Unit] -> Maybe String
getUnitInfo name units = do
    unit <- findUnitByName name units
    return $ formatUnit unit

-- Обработка цепочки Maybe операций
processUnitAction :: String -> Position -> [Unit] -> Maybe String
processUnitAction unitName targetPos units = do
    unit <- findUnitByName unitName units
    updatedUnit <- safeMoveUnit unit targetPos units
    return $ "Unit " ++ unitName unit ++ " moved to " ++ show targetPos
```

---

# IO монада

## Что такое IO?
**IO** — монада для работы с вводом-выводом и другими побочными эффектами.

## Особенности:
- **Чистые функции** не могут выполнять IO
- **IO действия** выполняются только в main или других IO функциях
- **Последовательность** операций гарантирована

## Базовые IO операции:
```haskell
-- Чтение строки
getLine :: IO String

-- Запись строки
putStrLn :: String -> IO ()

-- Чтение символа
getChar :: IO Char

-- Запись символа
putChar :: Char -> IO ()

-- Чтение числа
readLn :: Read a => IO a

-- Возврат значения
return :: a -> IO a
```

---

# Работа с IO

```haskell
-- Простая программа с IO
main :: IO ()
main = do
    putStrLn "Добро пожаловать в игру!"
    putStrLn "Введите имя игрока:"
    playerName <- getLine
    putStrLn $ "Привет, " ++ playerName ++ "!"
    
    putStrLn "Введите уровень сложности (1-5):"
    difficultyStr <- getLine
    let difficulty = read difficultyStr :: Int
    
    if difficulty >= 1 && difficulty <= 5
        then putStrLn $ "Уровень сложности установлен: " ++ show difficulty
        else putStrLn "Неверный уровень, установлен средний (3)"
    
    putStrLn "Игра начинается!"

-- Функция для получения пользовательского ввода
getPlayerInput :: String -> IO String
getPlayerInput prompt = do
    putStr prompt
    getLine

-- Функция для получения числового ввода
getNumberInput :: String -> IO Int
getNumberInput prompt = do
    putStr prompt
    input <- getLine
    case reads input of
        [(n, "")] -> return n
        _ -> do
            putStrLn "Неверный ввод, попробуйте снова"
            getNumberInput prompt

-- Функция для выбора из меню
showMenu :: [String] -> IO Int
showMenu options = do
    putStrLn "Выберите опцию:"
    mapM_ (\i -> putStrLn $ show i ++ ". " ++ options !! (i-1)) [1..length options]
    choice <- getNumberInput "Ваш выбор: "
    if choice >= 1 && choice <= length options
        then return choice
        else do
            putStrLn "Неверный выбор, попробуйте снова"
            showMenu options
```

---

# Работа с файлами

## Чтение файлов:
```haskell
-- Чтение содержимого файла
readGameFile :: FilePath -> IO String
readGameFile filename = do
    content <- readFile filename
    return content

-- Чтение файла с обработкой ошибок
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile filename = do
    catch (do
        content <- readFile filename
        return (Right content))
        (\e -> return (Left ("Ошибка чтения файла: " ++ show (e :: IOError)))

-- Чтение игрового состояния из файла
loadGameState :: FilePath -> IO (Maybe GameState)
loadGameState filename = do
    result <- safeReadFile filename
    case result of
        Left error -> do
            putStrLn $ "Ошибка загрузки: " ++ error
            return Nothing
        Right content -> do
            case parseGameState content of
                Just state -> return (Just state)
                Nothing -> do
                    putStrLn "Ошибка парсинга файла"
                    return Nothing
```

## Запись в файлы:
```haskell
-- Запись в файл
writeGameFile :: FilePath -> String -> IO ()
writeGameFile filename content = do
    writeFile filename content
    putStrLn $ "Файл " ++ filename ++ " сохранен"

-- Безопасная запись
safeWriteFile :: FilePath -> String -> IO (Either String ())
safeWriteFile filename content = do
    catch (do
        writeFile filename content
        return (Right ()))
        (\e -> return (Left ("Ошибка записи файла: " ++ show (e :: IOError))))

-- Сохранение игрового состояния
saveGameState :: FilePath -> GameState -> IO Bool
saveGameState filename state = do
    let content = show state
    result <- safeWriteFile filename content
    case result of
        Left error -> do
            putStrLn $ "Ошибка сохранения: " ++ error
            return False
        Right _ -> do
            putStrLn "Игра сохранена успешно"
            return True
```

---

# Обработка ошибок

## Either монада:
```haskell
-- Either для обработки ошибок
data GameError = 
    FileNotFound String
  | ParseError String
  | ValidationError String
  | NetworkError String
    deriving (Show, Eq)

-- Функции с обработкой ошибок
loadGameConfig :: FilePath -> IO (Either GameError GameConfig)
loadGameConfig filename = do
    result <- safeReadFile filename
    case result of
        Left ioError -> return (Left (FileNotFound (show ioError)))
        Right content -> case parseConfig content of
            Just config -> return (Right config)
            Nothing -> return (Left (ParseError "Неверный формат конфигурации"))

-- Обработка цепочки Either операций
initializeGame :: FilePath -> IO (Either GameError GameState)
initializeGame configFile = do
    config <- loadGameConfig configFile
    case config of
        Left error -> return (Left error)
        Right cfg -> do
            let gameState = createInitialGameState cfg
            if validateGameState gameState
                then return (Right gameState)
                else return (Left (ValidationError "Неверное начальное состояние игры"))

-- Функции для работы с Either
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right a) = Right a

mapRight :: (a -> b) -> Either e a -> Either e b
mapRight _ (Left e) = Left e
mapRight f (Right a) = Right (f a)

-- Композиция Either функций
composeEither :: (a -> Either e b) -> (b -> Either e c) -> a -> Either e c
composeEither f g x = f x >>= g
```

---

# Практический пример: Игровой движок

```haskell
-- Основные типы для игрового движка
data GameConfig = GameConfig
    { boardSize :: Int
    , maxUnits :: Int
    , startingResources :: [Resource]
    , aiDifficulty :: Int
    } deriving (Show, Read)

data GameState = GameState
    { players :: [Player]
    , units :: [Unit]
    , buildings :: [Building]
    , resources :: [Resource]
    , currentTurn :: Int
    , gamePhase :: GamePhase
    } deriving (Show, Read)

data GameAction = 
    MoveUnit String Position
  | AttackUnit String String
  | BuildBuilding String BuildingType Position
  | CollectResource String
  | EndTurn
    deriving (Show, Read)

-- Главная функция игры
main :: IO ()
main = do
    putStrLn "=== Пошаговая стратегия ==="
    
    -- Загрузка конфигурации
    config <- loadGameConfig "game.cfg"
    case config of
        Left error -> do
            putStrLn $ "Ошибка загрузки конфигурации: " ++ show error
            putStrLn "Используются настройки по умолчанию"
            let defaultConfig = GameConfig 10 50 [] 2
            startGame defaultConfig
        Right cfg -> startGame cfg

-- Запуск игры
startGame :: GameConfig -> IO ()
startGame config = do
    putStrLn "Игра инициализируется..."
    
    -- Создание начального состояния
    let initialState = createInitialGameState config
    
    -- Основной игровой цикл
    gameLoop initialState config

-- Основной игровой цикл
gameLoop :: GameState -> GameConfig -> IO ()
gameLoop state config = do
    -- Отображение текущего состояния
    displayGameState state
    
    -- Проверка окончания игры
    if isGameOver state
        then do
            putStrLn "Игра окончена!"
            displayGameResults state
        else do
            -- Ход игрока
            action <- getPlayerAction state
            let newState = executeAction action state
            
            -- Ход ИИ
            let finalState = executeAITurn newState config
            
            -- Сохранение игры
            saveGameState "autosave.sav" finalState
            
            -- Следующий ход
            gameLoop finalState config

-- Получение действия игрока
getPlayerAction :: GameState -> IO GameAction
getPlayerAction state = do
    putStrLn "\nДоступные действия:"
    putStrLn "1. Движение юнита"
    putStrLn "2. Атака"
    putStrLn "3. Строительство"
    putStrLn "4. Сбор ресурсов"
    putStrLn "5. Завершить ход"
    
    choice <- getNumberInput "Выберите действие: "
    case choice of
        1 -> getMoveAction state
        2 -> getAttackAction state
        3 -> getBuildAction state
        4 -> getCollectAction state
        5 -> return EndTurn
        _ -> do
            putStrLn "Неверный выбор, попробуйте снова"
            getPlayerAction state

-- Получение действия движения
getMoveAction :: GameState -> IO GameAction
getMoveAction state = do
    putStrLn "Доступные юниты:"
    let aliveUnits = filter isAlive (units state)
    mapM_ (\unit -> putStrLn $ "- " ++ unitName unit ++ " в позиции " ++ show (unitPosition unit)) aliveUnits
    
    unitName <- getPlayerInput "Введите имя юнита: "
    case findUnitByName unitName (units state) of
        Nothing -> do
            putStrLn "Юнит не найден"
            getMoveAction state
        Just unit -> do
            putStrLn $ "Текущая позиция: " ++ show (unitPosition unit)
            putStrLn "Введите новую позицию (x y):"
            posInput <- getLine
            case parsePosition posInput of
                Nothing -> do
                    putStrLn "Неверный формат позиции"
                    getMoveAction state
                Just pos -> return (MoveUnit unitName pos)

-- Выполнение действия
executeAction :: GameAction -> GameState -> GameState
executeAction action state = case action of
    MoveUnit unitName newPos -> moveUnit unitName newPos state
    AttackUnit attackerName targetName -> attackUnit attackerName targetName state
    BuildBuilding builderName buildingType pos -> buildBuilding builderName buildingType pos state
    CollectResource resourceName -> collectResource resourceName state
    EndTurn -> endTurn state

-- Движение юнита
moveUnit :: String -> Position -> GameState -> GameState
moveUnit unitName newPos state = 
    case findUnitByName unitName (units state) of
        Nothing -> state
        Just unit -> 
            if canMoveTo unit newPos
                then let updatedUnit = unit { unitPosition = newPos }
                         updatedUnits = map (\u -> if unitName u == unitName then updatedUnit else u) (units state)
                     in state { units = updatedUnits }
                else state

-- Атака юнита
attackUnit :: String -> String -> GameState -> GameState
attackUnit attackerName targetName state = 
    case (findUnitByName attackerName (units state), findUnitByName targetName (units state)) of
        (Just attacker, Just target) -> 
            if canAttack attacker target
                then let damage = calculateDamage attacker target
                         updatedTarget = target { unitHealth = max 0 (unitHealth target - damage) }
                         updatedUnits = map (\u -> if unitName u == targetName then updatedTarget else u) (units state)
                     in state { units = updatedUnits }
                else state
        _ -> state

-- Отображение игрового состояния
displayGameState :: GameState -> IO ()
displayGameState state = do
    putStrLn "\n=== Текущее состояние игры ==="
    putStrLn $ "Ход: " ++ show (currentTurn state)
    putStrLn $ "Фаза: " ++ show (gamePhase state)
    
    putStrLn "\nИгроки:"
    mapM_ displayPlayer (players state)
    
    putStrLn "\nЮниты:"
    let aliveUnits = filter isAlive (units state)
    mapM_ displayUnit aliveUnits
    
    putStrLn "\nРесурсы:"
    mapM_ displayResource (resources state)

-- Отображение игрока
displayPlayer :: Player -> IO ()
displayPlayer player = do
    putStrLn $ "Игрок: " ++ playerName player
    putStrLn $ "  Здоровье юнитов: " ++ show (sum (map unitHealth (playerUnits player)))
    putStrLn $ "  Количество зданий: " ++ show (length (playerBuildings player))

-- Отображение юнита
displayUnit :: Unit -> IO ()
displayUnit unit = do
    putStrLn $ "  " ++ unitName unit ++ " (" ++ show (unitType unit) ++ ")"
    putStrLn $ "    Позиция: " ++ show (unitPosition unit)
    putStrLn $ "    Здоровье: " ++ show (unitHealth unit) ++ "/" ++ show (unitMaxHealth unit)

-- Отображение ресурса
displayResource :: Resource -> IO ()
displayResource resource = do
    putStrLn $ "  " ++ getResourceName resource ++ ": " ++ show (getResourceAmount resource)

-- Вспомогательные функции
parsePosition :: String -> Maybe Position
parsePosition input = 
    case words input of
        [xStr, yStr] -> case (reads xStr, reads yStr) of
            ([(x, "")], [(y, "")]) -> Just (Position x y)
            _ -> Nothing
        _ -> Nothing

isGameOver :: GameState -> Bool
isGameOver state = 
    let alivePlayers = filter (\player -> any isAlive (playerUnits player)) (players state)
    in length alivePlayers <= 1

displayGameResults :: GameState -> IO ()
displayGameResults state = do
    putStrLn "\n=== Результаты игры ==="
    let winner = findWinner state
    case winner of
        Just player -> putStrLn $ "Победитель: " ++ playerName player
        Nothing -> putStrLn "Ничья!"

findWinner :: GameState -> Maybe Player
findWinner state = 
    let alivePlayers = filter (\player -> any isAlive (playerUnits player)) (players state)
    in case alivePlayers of
        [player] -> Just player
        _ -> Nothing
```

---

# Лучшие практики работы с монадами

## ✅ Что делать:
- **Использовать Maybe** для безопасных вычислений
- **Применять Either** для обработки ошибок
- **Разделять IO и чистую логику** в программах
- **Использовать do-нотацию** для читаемости
- **Обрабатывать ошибки** на соответствующем уровне

## ❌ Чего избегать:
- **Смешивать IO и чистые функции** без необходимости
- **Игнорировать обработку ошибок** в Maybe и Either
- **Создавать слишком сложные** монадические цепочки
- **Забывать про последовательность** IO операций
- **Использовать unsafePerformIO** без крайней необходимости

---

# Домашнее задание

## Задача 1:
Реализовать систему сохранения/загрузки игры

## Задача 2:
Создать интерактивное меню для игры

## Задача 3:
Реализовать обработку ошибок в игровом движке

---

# Что дальше?

## На следующей лекции:
- **Тестирование**
- **JUnit 5**
- **Mock объекты**
- **Тестирование архитектуры**

## Подготовка:
- Изучить главу 29-30 из учебника
- Выполнить домашнее задание
- Подготовить вопросы по текущей теме

---

# Вопросы?

## Контакты:
- **Email:** [ваш.email@university.edu]
- **Telegram:** [@username]
- **Офис:** [номер кабинета]

## Следующая лекция: **Тестирование**
