---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Лабораторная работа 20: Типы данных и сопоставление с образцом

## Цель работы
Изучить продвинутые возможности типов данных и сопоставления с образцом в Haskell на примере разработки игровых систем. Научиться создавать сложные типы данных, использовать паттерн-матчинг и работать с рекурсивными структурами.

## Теоретические основы

### Типы данных в Haskell
- **Алгебраические типы данных** - объединения и произведения типов
- **Рекурсивные типы** - типы, содержащие самих себя
- **Параметризованные типы** - типы с параметрами
- **Новые типы** - создание новых типов на основе существующих
- **Синонимы типов** - альтернативные имена для типов

### Сопоставление с образцом
- **Паттерн-матчинг** - сопоставление значений с образцами
- **Охранные выражения** - условная логика в функциях
- **Локальные определения** - where и let выражения
- **Рекурсивные паттерны** - работа с рекурсивными структурами

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Типы данных и паттерн-матчинг

#### 1. Сложные типы данных
```haskell
-- Синонимы типов для улучшения читаемости
type EntityId = String
type PlayerId = String
type GameTime = Int
type GameTurn = Int
type MapSize = (Int, Int)

-- Перечисления для состояний
data GameState = MainMenu | Playing | Paused | GameOver
    deriving (Show, Eq, Ord)

data PlayerState = Human | AI | Neutral
    deriving (Show, Eq, Ord)

data CombatState = Idle | Attacking | Defending | Moving
    deriving (Show, Eq, Ord)

-- Сложные типы для игровых механик
data CombatResult = CombatResult
    { damageDealt :: Int
    , damageReceived :: Int
    , attackerSurvived :: Bool
    , defenderSurvived :: Bool
    , experienceGained :: Int
    , criticalHit :: Bool
    } deriving (Show, Eq)

data ResourceCost = ResourceCost
    { goldCost :: Int
    , woodCost :: Int
    , stoneCost :: Int
    , foodCost :: Int
    , ironCost :: Int
    } deriving (Show, Eq)

data BuildingEffect = BuildingEffect
    { effectType :: EffectType
    , effectValue :: Int
    , effectDuration :: Int
    , effectTarget :: EffectTarget
    } deriving (Show, Eq)

data EffectType = HealthBonus | AttackBonus | DefenseBonus | MovementBonus | ProductionBonus
    deriving (Show, Eq, Ord)

data EffectTarget = Self | Allies | Enemies | All
    deriving (Show, Eq, Ord)
```

#### 2. Рекурсивные типы данных
```haskell
-- Дерево технологий
data TechTree = TechNode
    { techName :: String
    , techCost :: ResourceCost
    , techPrerequisites :: [String]
    , techEffects :: [BuildingEffect]
    , techChildren :: [TechTree]
    } deriving (Show, Eq)

-- Дерево решений ИИ
data DecisionTree = DecisionNode
    { condition :: AICondition
    , trueBranch :: DecisionTree
    , falseBranch :: DecisionTree
    } | ActionNode
    { action :: AIAction
    } deriving (Show, Eq)

data AICondition = HealthBelow Int
                 | EnemyNearby Double
                 | ResourceLow ResourceType
                 | BuildingUnderAttack
                 | AlliesInDanger
                 deriving (Show, Eq)

data AIAction = AttackNearest
              | RetreatToBase
              | GatherResources
              | BuildDefense
              | CallForHelp
              deriving (Show, Eq)

-- Связанный список для истории действий
data ActionHistory = ActionHistory
    { action :: GameAction
    , timestamp :: GameTime
    , player :: PlayerId
    , next :: Maybe ActionHistory
    } deriving (Show, Eq)

data GameAction = MoveAction EntityId Position
                | AttackAction EntityId EntityId
                | BuildAction BuildingType Position
                | UpgradeAction EntityId
                | ResearchAction String
                deriving (Show, Eq)
```

#### 3. Параметризованные типы
```haskell
-- Контейнер для игровых объектов
data GameContainer a = GameContainer
    { containerId :: String
    , containerName :: String
    , containerCapacity :: Int
    , containerItems :: [a]
    , containerType :: ContainerType
    } deriving (Show, Eq)

data ContainerType = Inventory | Warehouse | Armory | Library
    deriving (Show, Eq, Ord)

-- Очередь событий
data EventQueue a = EventQueue
    { queueId :: String
    , queueEvents :: [a]
    , queueMaxSize :: Int
    , queuePriority :: Priority
    } deriving (Show, Eq)

data Priority = Low | Normal | High | Critical
    deriving (Show, Eq, Ord)

-- Результат операции с возможностью ошибки
data GameResult a = Success a
                  | Failure String
                  | PartialSuccess a String
                  deriving (Show, Eq)

-- Монада для игровых вычислений
newtype GameM a = GameM { runGameM :: GameState -> (GameResult a, GameState) }

-- Функтор для GameM
instance Functor GameM where
    fmap f (GameM g) = GameM $ \state ->
        let (result, newState) = g state
        in (fmap f result, newState)

-- Аппликативный функтор для GameM
instance Applicative GameM where
    pure a = GameM $ \state -> (Success a, state)
    (<*>) (GameM f) (GameM g) = GameM $ \state ->
        let (fResult, state1) = f state
            (gResult, state2) = g state1
        in (fResult <*> gResult, state2)

-- Монада для GameM
instance Monad GameM where
    return = pure
    (>>=) (GameM f) g = GameM $ \state ->
        let (result, newState) = f state
        in case result of
            Success a -> runGameM (g a) newState
            Failure msg -> (Failure msg, newState)
            PartialSuccess a msg -> 
                let (finalResult, finalState) = runGameM (g a) newState
                in case finalResult of
                    Success b -> (PartialSuccess b msg, finalState)
                    Failure fmsg -> (Failure fmsg, finalState)
                    PartialSuccess b fmsg -> (PartialSuccess b (msg ++ "; " ++ fmsg), finalState)
```

#### 4. Паттерн-матчинг для сложных типов
```haskell
-- Функции для работы с деревом технологий
getTechCost :: TechTree -> ResourceCost
getTechCost (TechNode _ cost _ _ _) = cost

getTechPrerequisites :: TechTree -> [String]
getTechPrerequisites (TechNode _ _ prereqs _ _) = prereqs

hasPrerequisites :: TechTree -> [String] -> Bool
hasPrerequisites tech researched = 
    all (`elem` researched) (getTechPrerequisites tech)

canResearch :: TechTree -> [String] -> Bool
canResearch tech researched = 
    hasPrerequisites tech researched && 
    not (techName tech `elem` researched)

-- Функции для работы с деревом решений ИИ
evaluateCondition :: AICondition -> GameState -> Bool
evaluateCondition (HealthBelow threshold) state = 
    getPlayerHealth state < threshold
evaluateCondition (EnemyNearby radius) state = 
    any (\e -> distance (getPlayerPosition state) (getEntityPosition e) <= radius) 
         (getEnemyEntities state)
evaluateCondition (ResourceLow resourceType) state = 
    getResourceAmount state resourceType < 100
evaluateCondition BuildingUnderAttack state = 
    any isUnderAttack (getPlayerBuildings state)
evaluateCondition AlliesInDanger state = 
    any isInDanger (getPlayerAllies state)

makeDecision :: DecisionTree -> GameState -> AIAction
makeDecision (DecisionNode condition trueBranch falseBranch) state =
    if evaluateCondition condition state
    then makeDecision trueBranch state
    else makeDecision falseBranch state
makeDecision (ActionNode action) _ = action

-- Функции для работы с очередью событий
addEvent :: EventQueue a -> a -> EventQueue a
addEvent queue event = 
    if length (queueEvents queue) < queueMaxSize queue
    then queue { queueEvents = event : queueEvents queue }
    else queue

removeEvent :: EventQueue a -> EventQueue a
removeEvent queue = 
    case queueEvents queue of
        [] -> queue
        (_:rest) -> queue { queueEvents = rest }

getNextEvent :: EventQueue a -> Maybe a
getNextEvent queue = 
    case queueEvents queue of
        [] -> Nothing
        (event:_) -> Just event

-- Функции для работы с контейнерами
addItem :: GameContainer a -> a -> GameContainer a
addItem container item = 
    if length (containerItems container) < containerCapacity container
    then container { containerItems = item : containerItems container }
    else container

removeItem :: GameContainer a -> (a -> Bool) -> GameContainer a
removeItem container predicate = 
    container { containerItems = filter (not . predicate) (containerItems container) }

findItem :: GameContainer a -> (a -> Bool) -> Maybe a
findItem container predicate = 
    case filter predicate (containerItems container) of
        [] -> Nothing
        (item:_) -> Just item
```

#### 5. Паттерн-матчинг с охранными выражениями
```haskell
-- Функции для работы с боевой системой
calculateDamage :: Unit -> Unit -> CombatResult
calculateDamage attacker defender
    | not (canAttack attacker defender) = 
        CombatResult 0 0 True True 0 False
    | isCriticalHit attacker = 
        let damage = (unitAttack attacker * 2) - unitDefense defender
            finalDamage = max 1 damage
        in CombatResult finalDamage 0 True False 50 True
    | otherwise = 
        let damage = unitAttack attacker - unitDefense defender
            finalDamage = max 1 damage
            defenderSurvives = entityHealth (unitEntity defender) > finalDamage
        in CombatResult finalDamage 0 True defenderSurvives 10 False

canAttack :: Unit -> Unit -> Bool
canAttack attacker defender
    | entityTeam (unitEntity attacker) == entityTeam (unitEntity defender) = False
    | not (entityAlive (unitEntity defender)) = False
    | not (entityAlive (unitEntity attacker)) = False
    | otherwise = 
        let distance = distance (entityPosition (unitEntity attacker)) 
                               (entityPosition (unitEntity defender))
            attackRange = getAttackRange (unitType attacker)
        in distance <= attackRange

getAttackRange :: UnitType -> Double
getAttackRange Warrior = 1.0
getAttackRange Archer = 3.0
getAttackRange Mage = 2.0
getAttackRange Knight = 1.0

isCriticalHit :: Unit -> Bool
isCriticalHit unit
    | unitLevel unit >= 10 = unitExperience unit `mod` 100 < 20
    | unitLevel unit >= 5 = unitExperience unit `mod` 100 < 10
    | otherwise = unitExperience unit `mod` 100 < 5

-- Функции для работы с ресурсами
canAfford :: ResourceCost -> [Resource] -> Bool
canAfford cost resources = 
    let gold = findResource Gold resources
        wood = findResource Wood resources
        stone = findResource Stone resources
        food = findResource Food resources
        iron = findResource Iron resources
    in maybe False (>= goldCost cost) gold &&
       maybe False (>= woodCost cost) wood &&
       maybe False (>= stoneCost cost) stone &&
       maybe False (>= foodCost cost) food &&
       maybe False (>= ironCost cost) iron

findResource :: ResourceType -> [Resource] -> Maybe Int
findResource resourceType resources = 
    case filter (\r -> resourceType r == resourceType) resources of
        [] -> Nothing
        (resource:_) -> Just (resourceAmount resource)

spendResources :: ResourceCost -> [Resource] -> Maybe [Resource]
spendResources cost resources
    | not (canAfford cost resources) = Nothing
    | otherwise = 
        let updatedResources = map (updateResourceAmount cost) resources
        in Just updatedResources

updateResourceAmount :: ResourceCost -> Resource -> Resource
updateResourceAmount cost resource
    | resourceType resource == Gold = 
        resource { resourceAmount = resourceAmount resource - goldCost cost }
    | resourceType resource == Wood = 
        resource { resourceAmount = resourceAmount resource - woodCost cost }
    | resourceType resource == Stone = 
        resource { resourceAmount = resourceAmount resource - stoneCost cost }
    | resourceType resource == Food = 
        resource { resourceAmount = resourceAmount resource - foodCost cost }
    | resourceType resource == Iron = 
        resource { resourceAmount = resourceAmount resource - ironCost cost }
    | otherwise = resource
```

#### 6. Рекурсивные функции с паттерн-матчингом
```haskell
-- Функции для работы с деревом технологий
getAllTechnologies :: TechTree -> [String]
getAllTechnologies (TechNode name _ _ _ children) = 
    name : concatMap getAllTechnologies children

getResearchableTechnologies :: TechTree -> [String] -> [String]
getResearchableTechnologies tech researched = 
    case tech of
        TechNode name _ prereqs _ children ->
            let canResearchThis = canResearch tech researched
                researchableChildren = concatMap (\child -> 
                    getResearchableTechnologies child researched) children
            in if canResearchThis 
               then name : researchableChildren
               else researchableChildren

getTechnologyPath :: TechTree -> String -> [String]
getTechnologyPath tech targetName = 
    case tech of
        TechNode name _ _ _ children ->
            if name == targetName
            then [name]
            else case concatMap (\child -> getTechnologyPath child targetName) children of
                [] -> []
                path -> name : path

-- Функции для работы с деревом решений ИИ
getAllActions :: DecisionTree -> [AIAction]
getAllActions tree = 
    case tree of
        DecisionNode _ trueBranch falseBranch ->
            getAllActions trueBranch ++ getAllActions falseBranch
        ActionNode action -> [action]

getDecisionDepth :: DecisionTree -> Int
getDecisionDepth tree = 
    case tree of
        DecisionNode _ trueBranch falseBranch ->
            1 + max (getDecisionDepth trueBranch) (getDecisionDepth falseBranch)
        ActionNode _ -> 0

-- Функции для работы с историей действий
addActionToHistory :: ActionHistory -> GameAction -> PlayerId -> GameTime -> ActionHistory
addActionToHistory history action playerId timestamp = 
    ActionHistory action timestamp playerId (Just history)

getActionHistory :: ActionHistory -> [GameAction]
getActionHistory history = 
    action history : maybe [] getActionHistory (next history)

getPlayerActions :: ActionHistory -> PlayerId -> [GameAction]
getPlayerActions history playerId = 
    case history of
        ActionHistory act _ pid nextHistory ->
            let restActions = maybe [] (getPlayerActions playerId) nextHistory
            in if pid == playerId 
               then act : restActions
               else restActions

-- Функции для работы с контейнерами
getContainerContents :: GameContainer a -> [a]
getContainerContents container = containerItems container

isContainerFull :: GameContainer a -> Bool
isContainerFull container = 
    length (containerItems container) >= containerCapacity container

getContainerSpace :: GameContainer a -> Int
getContainerSpace container = 
    containerCapacity container - length (containerItems container)

-- Рекурсивная функция для поиска в контейнере
findItemRecursive :: GameContainer a -> (a -> Bool) -> Maybe a
findItemRecursive container predicate = 
    findItemRecursiveHelper (containerItems container) predicate

findItemRecursiveHelper :: [a] -> (a -> Bool) -> Maybe a
findItemRecursiveHelper [] _ = Nothing
findItemRecursiveHelper (item:rest) predicate = 
    if predicate item
    then Just item
    else findItemRecursiveHelper rest predicate
```

#### 7. Функции высшего порядка
```haskell
-- Функции для работы с игровыми сущностями
filterEntities :: [GameEntity] -> (GameEntity -> Bool) -> [GameEntity]
filterEntities entities predicate = filter predicate entities

mapEntities :: [GameEntity] -> (GameEntity -> GameEntity) -> [GameEntity]
mapEntities entities transform = map transform entities

foldEntities :: [GameEntity] -> b -> (b -> GameEntity -> b) -> b
foldEntities entities initial foldFunc = foldl foldFunc initial entities

-- Функции для работы с юнитами
filterUnits :: [Unit] -> (Unit -> Bool) -> [Unit]
filterUnits units predicate = filter predicate units

mapUnits :: [Unit] -> (Unit -> Unit) -> [Unit]
mapUnits units transform = map transform units

sortUnitsBy :: [Unit] -> (Unit -> Unit -> Ordering) -> [Unit]
sortUnitsBy units compareFunc = sortBy compareFunc units

-- Функции для работы с зданиями
filterBuildings :: [Building] -> (Building -> Bool) -> [Building]
filterBuildings buildings predicate = filter predicate buildings

mapBuildings :: [Building] -> (Building -> Building) -> [Building]
mapBuildings buildings transform = map transform buildings

groupBuildingsByType :: [Building] -> [(BuildingType, [Building])]
groupBuildingsByType buildings = 
    groupBy (\b1 b2 -> buildingType b1 == buildingType b2) 
            (sortBy (comparing buildingType) buildings)

-- Функции для работы с ресурсами
filterResources :: [Resource] -> (Resource -> Bool) -> [Resource]
filterResources resources predicate = filter predicate resources

mapResources :: [Resource] -> (Resource -> Resource) -> [Resource]
mapResources resources transform = map transform resources

sumResourceAmounts :: [Resource] -> ResourceType -> Int
sumResourceAmounts resources resourceType = 
    sum [ resourceAmount r | r <- resources, resourceType r == resourceType ]

-- Функции для работы с эффектами зданий
filterEffects :: [BuildingEffect] -> (BuildingEffect -> Bool) -> [BuildingEffect]
filterEffects effects predicate = filter predicate effects

mapEffects :: [BuildingEffect] -> (BuildingEffect -> BuildingEffect) -> [BuildingEffect]
mapEffects effects transform = map transform effects

getEffectsByType :: [BuildingEffect] -> EffectType -> [BuildingEffect]
getEffectsByType effects effectType = 
    filter (\e -> effectType e == effectType) effects

-- Функции для работы с боевыми результатами
filterCombatResults :: [CombatResult] -> (CombatResult -> Bool) -> [CombatResult]
filterCombatResults results predicate = filter predicate results

mapCombatResults :: [CombatResult] -> (CombatResult -> CombatResult) -> [CombatResult]
mapCombatResults results transform = map transform results

getTotalDamage :: [CombatResult] -> Int
getTotalDamage results = sum [ damageDealt r | r <- results ]

getTotalExperience :: [CombatResult] -> Int
getTotalExperience results = sum [ experienceGained r | r <- results ]
```

#### 8. Основные игровые функции
```haskell
-- Создание игрового мира с использованием сложных типов
createAdvancedGameWorld :: MapSize -> [TechTree] -> GameContainer GameEntity
createAdvancedGameWorld (width, height) techTrees = 
    let terrainEntities = createTerrainEntities width height
        baseEntities = createBaseEntities
        allEntities = terrainEntities ++ baseEntities
        container = GameContainer 
            { containerId = "world_1"
            , containerName = "Игровой мир"
            , containerCapacity = width * height
            , containerItems = allEntities
            , containerType = Warehouse
            }
    in container

createTerrainEntities :: Int -> Int -> [GameEntity]
createTerrainEntities width height = 
    [ createTerrain ("terrain_" ++ show x ++ "_" ++ show y) 
                   (getTerrainName (getTerrainType x y))
                   (getTerrainType x y) (x, y)
    | x <- [0..width-1], y <- [0..height-1] ]

createBaseEntities :: [GameEntity]
createBaseEntities = 
    [ createUnit "player_1" "Игрок" Warrior (10, 10) 1
    , createBuilding "building_1" "Главный замок" Castle (10, 11) 1
    , createResource "resource_1" "Золотая жила" Gold (5, 5) 1000
    ]

-- Функции для работы с игровым состоянием
updateGameState :: GameState -> GameAction -> GameState
updateGameState state action = 
    case action of
        MoveAction entityId newPosition -> 
            updateEntityPosition state entityId newPosition
        AttackAction attackerId defenderId -> 
            processAttack state attackerId defenderId
        BuildAction buildingType position -> 
            addBuilding state buildingType position
        UpgradeAction entityId -> 
            upgradeEntity state entityId
        ResearchAction techName -> 
            researchTechnology state techName

-- Вспомогательные функции для обновления состояния
updateEntityPosition :: GameState -> EntityId -> Position -> GameState
updateEntityPosition state entityId newPosition = state  -- Упрощенная реализация

processAttack :: GameState -> EntityId -> EntityId -> GameState
processAttack state attackerId defenderId = state  -- Упрощенная реализация

addBuilding :: GameState -> BuildingType -> Position -> GameState
addBuilding state buildingType position = state  -- Упрощенная реализация

upgradeEntity :: GameState -> EntityId -> GameState
upgradeEntity state entityId = state  -- Упрощенная реализация

researchTechnology :: GameState -> String -> GameState
researchTechnology state techName = state  -- Упрощенная реализация

-- Главная функция игры
main :: IO ()
main = do
    putStrLn "Добро пожаловать в игру 'Королевство'!"
    
    -- Создаем игровой мир
    let techTrees = createSampleTechTrees
        world = createAdvancedGameWorld (20, 20) techTrees
    putStrLn $ "Создан игровой мир с " ++ show (length (containerItems world)) ++ " сущностями"
    
    -- Создаем очередь событий
    let eventQueue = EventQueue "main_queue" [] 100 Normal
    putStrLn "Создана очередь событий"
    
    -- Создаем дерево решений ИИ
    let aiTree = createSampleAITree
    putStrLn "Создано дерево решений ИИ"
    
    putStrLn "Игра инициализирована успешно!"

-- Вспомогательные функции для создания примеров
createSampleTechTrees :: [TechTree]
createSampleTechTrees = 
    [ TechNode "Базовые технологии" (ResourceCost 0 0 0 0 0) [] [] []
    , TechNode "Военные технологии" (ResourceCost 100 50 0 0 0) [] [] []
    , TechNode "Экономические технологии" (ResourceCost 50 100 0 0 0) [] [] []
    ]

createSampleAITree :: DecisionTree
createSampleAITree = 
    DecisionNode (HealthBelow 50) 
        (ActionNode RetreatToBase)
        (DecisionNode (EnemyNearby 5.0) 
            (ActionNode AttackNearest)
            (ActionNode GatherResources)
        )
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичные типы данных и паттерн-матчинг:

### 1. **Гонки на выживание**
- Типы: трассы, автомобили, препятствия, физика движения
- Паттерны: обработка коллизий, расчет траекторий, ИИ гонщиков
- Особенности: рекурсивные структуры для трасс, деревья решений для ИИ

### 2. **Космическая колонизация**
- Типы: планеты, звездные системы, корабли, технологии
- Паттерны: генерация галактики, торговые маршруты, исследования
- Особенности: графы для звездных систем, деревья технологий

### 3. **Подземелье и драконы**
- Типы: уровни, персонажи, монстры, заклинания
- Паттерны: генерация подземелий, боевая система, квесты
- Особенности: рекурсивные структуры для уровней, деревья персонажей

### 4. **Город-государство**
- Типы: горожане, здания, экономика, политика
- Паттерны: симуляция агентов, экономические модели, события
- Особенности: графы для городской планировки, деревья решений

### 5. **Пиратская стратегия**
- Типы: корабли, острова, торговые маршруты, сокровища
- Паттерны: морская навигация, торговля, пиратские рейды
- Особенности: графы для торговых маршрутов, деревья кораблей

### 6. **Фермерское хозяйство**
- Типы: фермы, животные, культуры, рынок
- Паттерны: сельскохозяйственные циклы, рыночные цены, погода
- Особенности: рекурсивные структуры для ферм, деревья культур

### 7. **Киберпанк-тактика**
- Типы: хакеры, сети, программы, корпорации
- Паттерны: сетевые атаки, цифровая экономика, взломы
- Особенности: графы для сетей, деревья программ

### 8. **Средневековая осада**
- Типы: замки, армии, орудия, укрепления
- Паттерны: военные расчеты, осадные операции, тактика
- Особенности: рекурсивные структуры для укреплений, деревья армий

### 9. **Зомби-выживание**
- Типы: зомби, убежища, ресурсы, оружие
- Паттерны: ИИ зомби, система выживания, строительство
- Особенности: графы для убежищ, деревья решений для зомби

### 10. **Фэнтези-война**
- Типы: маги, существа, заклинания, армии
- Паттерны: магические эффекты, боевые расчеты, тактика
- Особенности: рекурсивные структуры для заклинаний, деревья существ

## Требования к реализации

### Обязательные требования:
1. **Создать сложные типы данных** с использованием алгебраических типов
2. **Реализовать рекурсивные типы** для игровых структур
3. **Использовать параметризованные типы** для обобщенных контейнеров
4. **Применить паттерн-матчинг** для обработки сложных типов
5. **Создать функции высшего порядка** для работы с коллекциями
6. **Демонстрировать работу** всех типов и функций

### Дополнительные требования:
1. **Добавить монады** для игровых вычислений
2. **Реализовать классы типов** для общих операций
3. **Создать функции для работы с файлами** и сериализацией
4. **Добавить обработку ошибок** и валидацию данных

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Сложные типы данных** | 4 | Алгебраические и рекурсивные типы |
| **Параметризованные типы** | 3 | Обобщенные контейнеры и функции |
| **Паттерн-матчинг** | 4 | Обработка сложных типов и структур |
| **Функции высшего порядка** | 3 | Работа с коллекциями и трансформациями |
| **Рекурсивные структуры** | 2 | Деревья, графы, связанные списки |
| **Качество кода** | 2 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Как создавать алгебраические типы данных в Haskell?
2. В чем разница между типами и новыми типами?
3. Как использовать параметризованные типы?
4. Как работать с рекурсивными структурами?
5. Как применять паттерн-матчинг к сложным типам?
6. Что такое функции высшего порядка?
7. Как создавать монады для игровых вычислений?

## Заключение

В данной лабораторной работе вы изучили продвинутые возможности типов данных и сопоставления с образцом в Haskell на примере создания игровых систем. Вы научились:

- Создавать сложные алгебраические типы данных
- Работать с рекурсивными и параметризованными типами
- Применять паттерн-матчинг к сложным структурам
- Использовать функции высшего порядка
- Создавать рекурсивные игровые структуры
- Применять функциональные концепции к игровой разработке

Полученные знания позволят вам создавать сложные и элегантные игровые системы на Haskell.
