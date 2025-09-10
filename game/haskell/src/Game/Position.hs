module Game.Position where

-- | Тип для представления позиции на игровом поле
data Position = Position 
    { x :: Int  -- ^ Координата X
    , y :: Int  -- ^ Координата Y
    } deriving (Eq, Show)

-- | Создает позицию по умолчанию (0, 0)
defaultPosition :: Position
defaultPosition = Position 0 0

-- | Вычисляет евклидово расстояние между двумя позициями
distance :: Position -> Position -> Double
distance (Position x1 y1) (Position x2 y2) = 
    sqrt (fromIntegral (dx * dx + dy * dy))
    where
        dx = x2 - x1
        dy = y2 - y1

-- | Проверяет, являются ли позиции соседними
isAdjacent :: Position -> Position -> Bool
isAdjacent (Position x1 y1) (Position x2 y2) = 
    abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1

-- | Получает список всех соседних позиций
getNeighbors :: Position -> [Position]
getNeighbors pos = 
    [Position (x pos + dx) (y pos + dy) 
    | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- | Проверяет, находится ли позиция в пределах игрового поля
isValidPosition :: Int -> Int -> Position -> Bool
isValidPosition maxX maxY (Position x y) = 
    x >= 0 && x < maxX && y >= 0 && y < maxY

-- | Получает позиции в заданном радиусе
getPositionsInRadius :: Position -> Int -> [Position]
getPositionsInRadius center radius = 
    [Position (x center + dx) (y center + dy)
    | dx <- [-radius..radius], dy <- [-radius..radius]
    , dx * dx + dy * dy <= radius * radius]

-- | Вычисляет манхэттенское расстояние
manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x1 y1) (Position x2 y2) = 
    abs (x2 - x1) + abs (y2 - y1)

-- | Проверяет, находится ли позиция на одной линии
isOnSameLine :: Position -> Position -> Bool
isOnSameLine (Position x1 y1) (Position x2 y2) = 
    x1 == x2 || y1 == y2

-- | Получает позиции между двумя позициями (включительно)
getPositionsBetween :: Position -> Position -> [Position]
getPositionsBetween start end
    | x start == x end = 
        [Position (x start) y | y <- [min (y start) (y end)..max (y start) (y end)]]
    | y start == y end = 
        [Position x (y start) | x <- [min (x start) (x end)..max (x start) (x end)]]
    | otherwise = [start, end]  -- Если не на одной линии, возвращаем только начальную и конечную

-- | Создает позицию из кортежа
fromTuple :: (Int, Int) -> Position
fromTuple (x, y) = Position x y

-- | Преобразует позицию в кортеж
toTuple :: Position -> (Int, Int)
toTuple (Position x y) = (x, y)

-- | Смещает позицию на заданное расстояние
offset :: Position -> Int -> Int -> Position
offset (Position x y) dx dy = Position (x + dx) (y + dy)

-- | Проверяет, находится ли позиция в прямоугольной области
isInRectangle :: Position -> Position -> Position -> Bool
isInRectangle (Position x1 y1) (Position x2 y2) (Position x y) = 
    x >= min x1 x2 && x <= max x1 x2 && 
    y >= min y1 y2 && y <= max y1 y2
