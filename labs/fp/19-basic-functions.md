# Лабораторная работа 19: Основы функционального программирования

## Цель работы
Изучить основы функционального программирования в Haskell на примере разработки игровых систем. Научиться работать с функциями, типами данных, списками и основными концепциями ФП.

## Теоретические основы

### Функциональное программирование в Haskell
- **Чистые функции** - функции без побочных эффектов
- **Неизменяемость** - данные не изменяются после создания
- **Ленивые вычисления** - вычисления откладываются до необходимости
- **Паттерн-матчинг** - сопоставление с образцом
- **Рекурсия** - основной способ итерации

### Основные концепции
- **Функции высшего порядка** - функции, принимающие функции как аргументы
- **Каррирование** - частичное применение функций
- **Композиция функций** - объединение функций в цепочки
- **Типы и классы типов** - система типов Haskell

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Основы функционального программирования

#### 1. Базовые типы данных
```haskell
-- Основные типы для игры
type Name = String
type Health = Int
type Position = (Int, Int)
type Team = Int

-- Перечисления для типов сущностей
data EntityType = Unit | Building | Resource | Terrain
    deriving (Show, Eq, Ord)

-- Типы юнитов
data UnitType = Warrior | Archer | Mage | Knight
    deriving (Show, Eq, Ord)

-- Типы зданий
data BuildingType = TownHall | Barracks | Farm | Mine | Castle
    deriving (Show, Eq, Ord)

-- Типы ресурсов
data ResourceType = Gold | Wood | Stone | Food | Iron
    deriving (Show, Eq, Ord)

-- Типы местности
data TerrainType = Plains | Forest | Mountain | Water | Desert
    deriving (Show, Eq, Ord)
```

#### 2. Основные игровые сущности
```haskell
-- Базовая сущность игры
data GameEntity = GameEntity
    { entityId :: String
    , entityName :: Name
    , entityType :: EntityType
    , entityPosition :: Position
    , entityHealth :: Health
    , entityMaxHealth :: Health
    , entityTeam :: Team
    , entityAlive :: Bool
    } deriving (Show, Eq)

-- Юнит (наследует от GameEntity)
data Unit = Unit
    { unitEntity :: GameEntity
    , unitType :: UnitType
    , unitAttack :: Int
    , unitDefense :: Int
    , unitMovement :: Int
    , unitExperience :: Int
    , unitLevel :: Int
    } deriving (Show, Eq)

-- Здание (наследует от GameEntity)
data Building = Building
    { buildingEntity :: GameEntity
    , buildingType :: BuildingType
    , buildingLevel :: Int
    , buildingProduction :: Int
    , buildingConstruction :: Bool
    , buildingProgress :: Int
    } deriving (Show, Eq)

-- Ресурс (наследует от GameEntity)
data Resource = Resource
    { resourceEntity :: GameEntity
    , resourceType :: ResourceType
    , resourceAmount :: Int
    , resourceMaxAmount :: Int
    , resourceRenewable :: Bool
    } deriving (Show, Eq)

-- Местность
data Terrain = Terrain
    { terrainEntity :: GameEntity
    , terrainType :: TerrainType
    , terrainMovementCost :: Int
    , terrainDefenseBonus :: Int
    } deriving (Show, Eq)
```

#### 3. Основные функции
```haskell
-- Создание базовой сущности
createEntity :: String -> Name -> EntityType -> Position -> Health -> Team -> GameEntity
createEntity id name entityType position health team = GameEntity
    { entityId = id
    , entityName = name
    , entityType = entityType
    , entityPosition = position
    , entityHealth = health
    , entityMaxHealth = health
    , entityTeam = team
    , entityAlive = True
    }

-- Создание юнита
createUnit :: String -> Name -> UnitType -> Position -> Team -> Unit
createUnit id name unitType position team = Unit
    { unitEntity = createEntity id name Unit position (getUnitHealth unitType) team
    , unitType = unitType
    , unitAttack = getUnitAttack unitType
    , unitDefense = getUnitDefense unitType
    , unitMovement = getUnitMovement unitType
    , unitExperience = 0
    , unitLevel = 1
    }

-- Создание здания
createBuilding :: String -> Name -> BuildingType -> Position -> Team -> Building
createBuilding id name buildingType position team = Building
    { buildingEntity = createEntity id name Building position (getBuildingHealth buildingType) team
    , buildingType = buildingType
    , buildingLevel = 1
    , buildingProduction = getBuildingProduction buildingType
    , buildingConstruction = False
    , buildingProgress = 100
    }

-- Создание ресурса
createResource :: String -> Name -> ResourceType -> Position -> Int -> Resource
createResource id name resourceType position amount = Resource
    { resourceEntity = createEntity id name Resource position 100 0
    , resourceType = resourceType
    , resourceAmount = amount
    , resourceMaxAmount = amount
    , resourceRenewable = isResourceRenewable resourceType
    }

-- Создание местности
createTerrain :: String -> Name -> TerrainType -> Position -> Terrain
createTerrain id name terrainType position = Terrain
    { terrainEntity = createEntity id name Terrain position 100 0
    , terrainType = terrainType
    , terrainMovementCost = getTerrainMovementCost terrainType
    , terrainDefenseBonus = getTerrainDefenseBonus terrainType
    }
```

#### 4. Вспомогательные функции
```haskell
-- Получение характеристик юнитов
getUnitHealth :: UnitType -> Health
getUnitHealth Warrior = 100
getUnitHealth Archer = 80
getUnitHealth Mage = 60
getUnitHealth Knight = 120

getUnitAttack :: UnitType -> Int
getUnitAttack Warrior = 25
getUnitAttack Archer = 30
getUnitAttack Mage = 40
getUnitAttack Knight = 35

getUnitDefense :: UnitType -> Int
getUnitDefense Warrior = 15
getUnitDefense Archer = 10
getUnitDefense Mage = 5
getUnitDefense Knight = 25

getUnitMovement :: UnitType -> Int
getUnitMovement Warrior = 2
getUnitMovement Archer = 2
getUnitMovement Mage = 1
getUnitMovement Knight = 2

-- Получение характеристик зданий
getBuildingHealth :: BuildingType -> Health
getBuildingHealth TownHall = 200
getBuildingHealth Barracks = 150
getBuildingHealth Farm = 100
getBuildingHealth Mine = 120
getBuildingHealth Castle = 300

getBuildingProduction :: BuildingType -> Int
getBuildingProduction TownHall = 0
getBuildingProduction Barracks = 0
getBuildingProduction Farm = 10
getBuildingProduction Mine = 8
getBuildingProduction Castle = 0

-- Получение характеристик местности
getTerrainMovementCost :: TerrainType -> Int
getTerrainMovementCost Plains = 1
getTerrainMovementCost Forest = 2
getTerrainMovementCost Mountain = 3
getTerrainMovementCost Water = 4
getTerrainMovementCost Desert = 2

getTerrainDefenseBonus :: TerrainType -> Int
getTerrainDefenseBonus Plains = 0
getTerrainDefenseBonus Forest = 2
getTerrainDefenseBonus Mountain = 3
getTerrainDefenseBonus Water = 1
getTerrainDefenseBonus Desert = 0

-- Проверка возобновляемости ресурсов
isResourceRenewable :: ResourceType -> Bool
isResourceRenewable Gold = False
isResourceRenewable Wood = True
isResourceRenewable Stone = False
isResourceRenewable Food = True
isResourceRenewable Iron = False
```

#### 5. Функции для работы с позициями
```haskell
-- Расчет расстояния между позициями
distance :: Position -> Position -> Double
distance (x1, y1) (x2, y2) = sqrt (fromIntegral (dx * dx + dy * dy))
    where
        dx = x2 - x1
        dy = y2 - y1

-- Проверка, находится ли позиция в пределах карты
isValidPosition :: Position -> Int -> Int -> Bool
isValidPosition (x, y) maxX maxY = x >= 0 && x < maxX && y >= 0 && y < maxY

-- Получение соседних позиций
getNeighborPositions :: Position -> [Position]
getNeighborPositions (x, y) = 
    [ (x-1, y), (x+1, y), (x, y-1), (x, y+1)
    , (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)
    ]

-- Получение позиций в радиусе
getPositionsInRadius :: Position -> Int -> [Position]
getPositionsInRadius center radius = 
    [ (x, y) | x <- [x0 - radius .. x0 + radius]
             , y <- [y0 - radius .. y0 + radius]
             , distance center (x, y) <= fromIntegral radius
    ]
    where (x0, y0) = center

-- Сложение позиций
addPositions :: Position -> Position -> Position
addPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Умножение позиции на скаляр
multiplyPosition :: Position -> Int -> Position
multiplyPosition (x, y) scalar = (x * scalar, y * scalar)
```

#### 6. Функции для работы с сущностями
```haskell
-- Проверка, может ли сущность двигаться
canMove :: GameEntity -> Position -> Bool
canMove entity newPos = 
    entityAlive entity && 
    distance (entityPosition entity) newPos <= 2.0

-- Движение сущности
moveEntity :: GameEntity -> Position -> GameEntity
moveEntity entity newPos = entity { entityPosition = newPos }

-- Получение урона
takeDamage :: GameEntity -> Int -> GameEntity
takeDamage entity damage = 
    let newHealth = max 0 (entityHealth entity - damage)
        newAlive = newHealth > 0
    in entity { entityHealth = newHealth, entityAlive = newAlive }

-- Исцеление сущности
healEntity :: GameEntity -> Int -> GameEntity
healEntity entity healAmount = 
    let newHealth = min (entityMaxHealth entity) (entityHealth entity + healAmount)
    in entity { entityHealth = newHealth }

-- Проверка, находится ли сущность в радиусе
isInRadius :: GameEntity -> Position -> Double -> Bool
isInRadius entity center radius = 
    distance (entityPosition entity) center <= radius

-- Получение сущностей в радиусе
getEntitiesInRadius :: [GameEntity] -> Position -> Double -> [GameEntity]
getEntitiesInRadius entities center radius = 
    filter (\entity -> isInRadius entity center radius) entities

-- Фильтрация сущностей по типу
filterEntitiesByType :: [GameEntity] -> EntityType -> [GameEntity]
filterEntitiesByType entities entityType = 
    filter (\entity -> entityType entity == entityType) entities

-- Фильтрация сущностей по команде
filterEntitiesByTeam :: [GameEntity] -> Team -> [GameEntity]
filterEntitiesByTeam entities team = 
    filter (\entity -> entityTeam entity == team) entities

-- Фильтрация живых сущностей
filterAliveEntities :: [GameEntity] -> [GameEntity]
filterAliveEntities entities = 
    filter entityAlive entities

-- Сортировка сущностей по здоровью
sortEntitiesByHealth :: [GameEntity] -> [GameEntity]
sortEntitiesByHealth entities = 
    sortBy (\e1 e2 -> compare (entityHealth e2) (entityHealth e1)) entities

-- Сортировка сущностей по расстоянию от позиции
sortEntitiesByDistance :: [GameEntity] -> Position -> [GameEntity]
sortEntitiesByDistance entities center = 
    sortBy (\e1 e2 -> compare (distance (entityPosition e1) center) 
                                   (distance (entityPosition e2) center)) entities
```

#### 7. Функции для работы с юнитами
```haskell
-- Повышение уровня юнита
levelUp :: Unit -> Unit
levelUp unit = unit
    { unitLevel = unitLevel unit + 1
    , unitAttack = unitAttack unit + 5
    , unitDefense = unitDefense unit + 3
    , unitMovement = unitMovement unit + 1
    , unitExperience = 0
    }

-- Добавление опыта юниту
addExperience :: Unit -> Int -> Unit
addExperience unit exp = 
    let newExp = unitExperience unit + exp
        newLevel = (newExp `div` 100) + 1
        levelDiff = newLevel - unitLevel unit
    in if levelDiff > 0 
       then iterate levelUp unit !! levelDiff
       else unit { unitExperience = newExp }

-- Атака юнита
attackUnit :: Unit -> Unit -> (Unit, Unit)
attackUnit attacker defender = 
    let damage = max 1 (unitAttack attacker - unitDefense defender)
        newDefender = takeDamage (unitEntity defender) damage
        newDefenderUnit = defender { unitEntity = newDefender }
        expGained = if entityAlive newDefender then 10 else 50
        newAttacker = addExperience attacker expGained
    in (newAttacker, newDefenderUnit)

-- Проверка, может ли юнит атаковать
canAttack :: Unit -> Unit -> Bool
canAttack attacker defender = 
    let distance = distance (entityPosition (unitEntity attacker)) 
                           (entityPosition (unitEntity defender))
        attackRange = case unitType attacker of
            Warrior -> 1.0
            Archer -> 3.0
            Mage -> 2.0
            Knight -> 1.0
    in distance <= attackRange && 
       entityTeam (unitEntity attacker) /= entityTeam (unitEntity defender) &&
       entityAlive (unitEntity defender)

-- Получение возможных ходов юнита
getPossibleMoves :: Unit -> Int -> Int -> [Position]
getPossibleMoves unit maxX maxY = 
    let currentPos = entityPosition (unitEntity unit)
        maxMovement = unitMovement unit
        allPositions = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
    in filter (\pos -> distance currentPos pos <= fromIntegral maxMovement) allPositions
```

#### 8. Функции для работы с зданиями
```haskell
-- Улучшение здания
upgradeBuilding :: Building -> Building
upgradeBuilding building = building
    { buildingLevel = buildingLevel building + 1
    , buildingProduction = buildingProduction building + 2
    }

-- Проверка, может ли здание производить ресурсы
canProduce :: Building -> Bool
canProduce building = 
    not (buildingConstruction building) && 
    buildingProduction building > 0

-- Производство ресурсов зданием
produceResources :: Building -> ResourceType -> Int
produceResources building resourceType = 
    if canProduce building 
    then buildingProduction building * buildingLevel building
    else 0

-- Строительство здания
constructBuilding :: Building -> Building
constructBuilding building = 
    if buildingConstruction building && buildingProgress building < 100
    then building { buildingProgress = buildingProgress building + 10 }
    else building

-- Проверка завершения строительства
isConstructionComplete :: Building -> Bool
isConstructionComplete building = 
    buildingProgress building >= 100

-- Завершение строительства
completeConstruction :: Building -> Building
completeConstruction building = 
    if isConstructionComplete building
    then building { buildingConstruction = False }
    else building
```

#### 9. Функции для работы с ресурсами
```haskell
-- Добавление ресурсов
addResource :: Resource -> Int -> Resource
addResource resource amount = 
    let newAmount = min (resourceMaxAmount resource) (resourceAmount resource + amount)
    in resource { resourceAmount = newAmount }

-- Удаление ресурсов
removeResource :: Resource -> Int -> Maybe Resource
removeResource resource amount = 
    if resourceAmount resource >= amount
    then Just (resource { resourceAmount = resourceAmount resource - amount })
    else Nothing

-- Проверка достаточности ресурсов
hasEnoughResource :: Resource -> Int -> Bool
hasEnoughResource resource amount = resourceAmount resource >= amount

-- Обновление возобновляемых ресурсов
updateRenewableResources :: Resource -> Resource
updateRenewableResources resource = 
    if resourceRenewable resource
    then let newAmount = min (resourceMaxAmount resource) 
                            (resourceAmount resource + 1)
         in resource { resourceAmount = newAmount }
    else resource

-- Слияние ресурсов одного типа
mergeResources :: Resource -> Resource -> Maybe Resource
mergeResources r1 r2 = 
    if resourceType r1 == resourceType r2
    then let totalAmount = resourceAmount r1 + resourceAmount r2
             maxAmount = max (resourceMaxAmount r1) (resourceMaxAmount r2)
         in Just (r1 { resourceAmount = totalAmount, resourceMaxAmount = maxAmount })
    else Nothing
```

#### 10. Основные игровые функции
```haskell
-- Создание игрового мира
createGameWorld :: Int -> Int -> [GameEntity]
createGameWorld width height = 
    let terrainEntities = [ createTerrain ("terrain_" ++ show x ++ "_" ++ show y) 
                                      (getTerrainName (getTerrainType x y))
                                      (getTerrainType x y) (x, y)
                          | x <- [0..width-1], y <- [0..height-1] ]
        terrainType = getTerrainType
        terrainName = getTerrainName
    in terrainEntities

-- Получение типа местности по координатам
getTerrainType :: Int -> Int -> TerrainType
getTerrainType x y = 
    let noise = (x * 73856093 + y * 19349663) `mod` 100
    in if noise < 30 then Plains
       else if noise < 60 then Forest
       else if noise < 80 then Mountain
       else if noise < 90 then Water
       else Desert

-- Получение названия местности
getTerrainName :: TerrainType -> Name
getTerrainName Plains = "Равнина"
getTerrainName Forest = "Лес"
getTerrainName Mountain = "Горы"
getTerrainName Water = "Вода"
getTerrainName Desert = "Пустыня"

-- Обновление игрового мира
updateGameWorld :: [GameEntity] -> [GameEntity]
updateGameWorld entities = 
    let updatedUnits = map updateUnit (filter isUnit entities)
        updatedBuildings = map updateBuilding (filter isBuilding entities)
        updatedResources = map updateResource (filter isResource entities)
        otherEntities = filter (\e -> not (isUnit e || isBuilding e || isResource e)) entities
    in updatedUnits ++ updatedBuildings ++ updatedResources ++ otherEntities

-- Вспомогательные функции для фильтрации
isUnit :: GameEntity -> Bool
isUnit entity = entityType entity == Unit

isBuilding :: GameEntity -> Bool
isBuilding entity = entityType entity == Building

isResource :: GameEntity -> Bool
isResource entity = entityType entity == Resource

-- Обновление юнита
updateUnit :: GameEntity -> GameEntity
updateUnit entity = entity  -- Базовая реализация

-- Обновление здания
updateBuilding :: GameEntity -> GameEntity
updateBuilding entity = entity  -- Базовая реализация

-- Обновление ресурса
updateResource :: GameEntity -> GameEntity
updateResource entity = entity  -- Базовая реализация

-- Главная функция игры
main :: IO ()
main = do
    putStrLn "Добро пожаловать в игру 'Королевство'!"
    
    -- Создаем игровой мир
    let world = createGameWorld 20 20
    putStrLn $ "Создан игровой мир размером 20x20 с " ++ show (length world) ++ " сущностями"
    
    -- Создаем игрока
    let player = createUnit "player_1" "Игрок" Warrior (10, 10) 1
    putStrLn $ "Создан игрок: " ++ entityName (unitEntity player)
    
    -- Создаем здание
    let building = createBuilding "building_1" "Главный замок" Castle (10, 11) 1
    putStrLn $ "Создано здание: " ++ entityName (buildingEntity building)
    
    -- Создаем ресурс
    let resource = createResource "resource_1" "Золотая жила" Gold (5, 5) 1000
    putStrLn $ "Создан ресурс: " ++ entityName (resourceEntity resource)
    
    putStrLn "Игра инициализирована успешно!"
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичные функции:

### 1. **Гонки на выживание**
- Функции: физика движения, коллизии, система очков
- Типы: автомобили, трассы, препятствия, гонщики
- Особенности: расчет траекторий, определение победителей

### 2. **Космическая колонизация**
- Функции: генерация планет, экономика, исследования
- Типы: планеты, корабли, колонии, технологии
- Особенности: космические расчеты, торговые маршруты

### 3. **Подземелье и драконы**
- Функции: генерация уровней, ИИ монстров, боевая система
- Типы: персонажи, монстры, заклинания, квесты
- Особенности: процедурная генерация, система опыта

### 4. **Город-государство**
- Функции: симуляция горожан, экономика, политика
- Типы: горожане, здания, экономика, события
- Особенности: демографические расчеты, экономические модели

### 5. **Пиратская стратегия**
- Функции: морские сражения, навигация, торговля
- Типы: корабли, острова, сокровища, торговые маршруты
- Особенности: морские расчеты, навигационные алгоритмы

### 6. **Фермерское хозяйство**
- Функции: симуляция растений, животные, рынок
- Типы: фермы, животные, культуры, рынок
- Особенности: сельскохозяйственные циклы, рыночные цены

### 7. **Киберпанк-тактика**
- Функции: сетевые атаки, цифровая экономика, хакерские системы
- Типы: хакеры, сети, программы, корпорации
- Особенности: криптографические алгоритмы, сетевые протоколы

### 8. **Средневековая осада**
- Функции: военные расчеты, осадные орудия, тактика
- Типы: замки, армии, орудия, укрепления
- Особенности: баллистические расчеты, тактические алгоритмы

### 9. **Зомби-выживание**
- Функции: ИИ зомби, система выживания, строительство
- Типы: зомби, убежища, ресурсы, оружие
- Особенности: алгоритмы поиска пути, симуляция толпы

### 10. **Фэнтези-война**
- Функции: магические эффекты, боевые расчеты, ИИ существ
- Типы: маги, существа, заклинания, армии
- Особенности: магические формулы, тактические алгоритмы

## Требования к реализации

### Обязательные требования:
1. **Создать базовые типы данных** для игровых сущностей
2. **Реализовать основные функции** для работы с сущностями
3. **Создать функции для работы с позициями** и расстояниями
4. **Реализовать функции для работы с юнитами** и зданиями
5. **Создать функции для работы с ресурсами** и экономикой
6. **Демонстрировать работу** всех основных функций

### Дополнительные требования:
1. **Добавить функции высшего порядка** для обработки списков
2. **Реализовать рекурсивные алгоритмы** для сложных вычислений
3. **Создать функции для работы с файлами** и сохранением
4. **Добавить обработку ошибок** и валидацию данных

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Базовые типы данных** | 3 | Определение типов и структур |
| **Основные функции** | 4 | Функции для работы с сущностями |
| **Функции позиций** | 3 | Работа с координатами и расстояниями |
| **Функции юнитов** | 3 | Логика юнитов и боевая система |
| **Функции зданий** | 2 | Логика зданий и строительства |
| **Функции ресурсов** | 2 | Экономическая система |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Что такое чистые функции в Haskell?
2. Как работает паттерн-матчинг?
3. В чем разница между типами и классами типов?
4. Как использовать рекурсию вместо циклов?
5. Что такое каррирование функций?
6. Как работать со списками в Haskell?
7. Как создавать собственные типы данных?

## Заключение

В данной лабораторной работе вы изучили основы функционального программирования в Haskell на примере создания игровых систем. Вы научились:

- Создавать типы данных и структуры
- Работать с функциями и паттерн-матчингом
- Использовать рекурсию и списки
- Создавать игровую логику в функциональном стиле
- Применять принципы ФП к игровой разработке

Полученные знания позволят вам создавать элегантные и эффективные игровые системы на Haskell.
