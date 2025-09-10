# Лабораторная работа 6: Игровое поле и движение

## Цель работы
Изучить принципы создания игрового поля и системы движения в Java на примере разработки игровых систем. Научиться реализовывать алгоритмы поиска пути, управление позициями и взаимодействие с игровым полем.

## Теоретические основы

### Игровое поле
- **Двумерная сетка** - представление игрового мира
- **Клетки и позиции** - координаты объектов на поле
- **Типы местности** - различные свойства клеток
- **Коллизии** - проверка столкновений объектов

### Алгоритмы поиска пути
- **Breadth-First Search (BFS)** - поиск в ширину
- **A* (A-star)** - эвристический поиск пути
- **Dijkstra** - поиск кратчайшего пути
- **Pathfinding** - нахождение оптимального маршрута

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система игрового поля

#### 1. Класс `Position` (Позиция)
```java
public class Position {
    private final int x;
    private final int y;
    
    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    public int getX() { return x; }
    public int getY() { return y; }
    
    public double distanceTo(Position other) {
        int dx = this.x - other.x;
        int dy = this.y - other.y;
        return Math.sqrt(dx * dx + dy * dy);
    }
    
    public int manhattanDistance(Position other) {
        return Math.abs(this.x - other.x) + Math.abs(this.y - other.y);
    }
    
    public List<Position> getNeighbors() {
        List<Position> neighbors = new ArrayList<>();
        neighbors.add(new Position(x + 1, y));
        neighbors.add(new Position(x - 1, y));
        neighbors.add(new Position(x, y + 1));
        neighbors.add(new Position(x, y - 1));
        return neighbors;
    }
    
    public List<Position> getNeighborsDiagonal() {
        List<Position> neighbors = getNeighbors();
        neighbors.add(new Position(x + 1, y + 1));
        neighbors.add(new Position(x + 1, y - 1));
        neighbors.add(new Position(x - 1, y + 1));
        neighbors.add(new Position(x - 1, y - 1));
        return neighbors;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Position position = (Position) obj;
        return x == position.x && y == position.y;
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
    
    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
```

#### 2. Перечисление `TerrainType` (Тип местности)
```java
public enum TerrainType {
    GRASS("Трава", 1, true, 1.0),
    FOREST("Лес", 2, true, 1.5),
    MOUNTAIN("Гора", 3, false, 2.0),
    WATER("Вода", 4, false, 2.5),
    ROAD("Дорога", 1, true, 0.8),
    WALL("Стена", 5, false, 3.0),
    BRIDGE("Мост", 1, true, 0.9);
    
    private final String name;
    private final int defenseBonus;
    private final boolean passable;
    private final double movementCost;
    
    TerrainType(String name, int defenseBonus, boolean passable, double movementCost) {
        this.name = name;
        this.defenseBonus = defenseBonus;
        this.passable = passable;
        this.movementCost = movementCost;
    }
    
    public String getName() { return name; }
    public int getDefenseBonus() { return defenseBonus; }
    public boolean isPassable() { return passable; }
    public double getMovementCost() { return movementCost; }
}
```

#### 3. Класс `Cell` (Клетка)
```java
public class Cell {
    private Position position;
    private TerrainType terrainType;
    private GameEntity entity;
    private boolean isOccupied;
    private int height;
    
    public Cell(Position position, TerrainType terrainType) {
        this.position = position;
        this.terrainType = terrainType;
        this.entity = null;
        this.isOccupied = false;
        this.height = 0;
    }
    
    public boolean canMoveTo() {
        return terrainType.isPassable() && !isOccupied;
    }
    
    public double getMovementCost() {
        return terrainType.getMovementCost() + (height * 0.1);
    }
    
    public void setEntity(GameEntity entity) {
        this.entity = entity;
        this.isOccupied = (entity != null);
    }
    
    public void clearEntity() {
        this.entity = null;
        this.isOccupied = false;
    }
    
    public boolean isOccupied() { return isOccupied; }
    public Position getPosition() { return position; }
    public TerrainType getTerrainType() { return terrainType; }
    public GameEntity getEntity() { return entity; }
    public int getHeight() { return height; }
    
    public void setHeight(int height) { this.height = height; }
    public void setTerrainType(TerrainType terrainType) { this.terrainType = terrainType; }
}
```

#### 4. Класс `GameBoard` (Игровое поле)
```java
public class GameBoard {
    private final int width;
    private final int height;
    private final Cell[][] cells;
    private final Map<Position, GameEntity> entityMap;
    
    public GameBoard(int width, int height) {
        this.width = width;
        this.height = height;
        this.cells = new Cell[width][height];
        this.entityMap = new HashMap<>();
        
        initializeBoard();
    }
    
    private void initializeBoard() {
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                Position pos = new Position(x, y);
                TerrainType terrain = getRandomTerrain();
                cells[x][y] = new Cell(pos, terrain);
            }
        }
    }
    
    private TerrainType getRandomTerrain() {
        TerrainType[] terrains = TerrainType.values();
        double rand = Math.random();
        
        if (rand < 0.6) return TerrainType.GRASS;
        if (rand < 0.75) return TerrainType.FOREST;
        if (rand < 0.85) return TerrainType.MOUNTAIN;
        if (rand < 0.95) return TerrainType.WATER;
        return TerrainType.ROAD;
    }
    
    public boolean isValidPosition(Position position) {
        return position.getX() >= 0 && position.getX() < width &&
               position.getY() >= 0 && position.getY() < height;
    }
    
    public Cell getCell(Position position) {
        if (!isValidPosition(position)) return null;
        return cells[position.getX()][position.getY()];
    }
    
    public boolean placeEntity(GameEntity entity, Position position) {
        if (!isValidPosition(position)) return false;
        
        Cell cell = getCell(position);
        if (cell == null || !cell.canMoveTo()) return false;
        
        cell.setEntity(entity);
        entityMap.put(position, entity);
        return true;
    }
    
    public void removeEntity(Position position) {
        if (!isValidPosition(position)) return;
        
        Cell cell = getCell(position);
        if (cell != null) {
            cell.clearEntity();
            entityMap.remove(position);
        }
    }
    
    public boolean moveEntity(GameEntity entity, Position newPosition) {
        Position oldPosition = entity.getPosition();
        
        if (!isValidPosition(newPosition)) return false;
        
        Cell newCell = getCell(newPosition);
        if (newCell == null || !newCell.canMoveTo()) return false;
        
        // Удаляем с старой позиции
        removeEntity(oldPosition);
        
        // Размещаем на новой позиции
        return placeEntity(entity, newPosition);
    }
    
    public List<Position> getPassablePositions() {
        List<Position> passable = new ArrayList<>();
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                Position pos = new Position(x, y);
                Cell cell = getCell(pos);
                if (cell != null && cell.canMoveTo()) {
                    passable.add(pos);
                }
            }
        }
        return passable;
    }
    
    public int getWidth() { return width; }
    public int getHeight() { return height; }
    public Map<Position, GameEntity> getEntityMap() { return entityMap; }
}
```

#### 5. Класс `PathFinder` (Поиск пути)
```java
public class PathFinder {
    private final GameBoard gameBoard;
    
    public PathFinder(GameBoard gameBoard) {
        this.gameBoard = gameBoard;
    }
    
    public List<Position> findPath(Position start, Position target) {
        return findPathAStar(start, target);
    }
    
    public List<Position> findPathBFS(Position start, Position target) {
        if (!gameBoard.isValidPosition(start) || !gameBoard.isValidPosition(target)) {
            return new ArrayList<>();
        }
        
        Queue<Position> queue = new LinkedList<>();
        Map<Position, Position> cameFrom = new HashMap<>();
        Set<Position> visited = new HashSet<>();
        
        queue.offer(start);
        visited.add(start);
        
        while (!queue.isEmpty()) {
            Position current = queue.poll();
            
            if (current.equals(target)) {
                return reconstructPath(cameFrom, current);
            }
            
            for (Position neighbor : current.getNeighbors()) {
                if (!gameBoard.isValidPosition(neighbor) || visited.contains(neighbor)) {
                    continue;
                }
                
                Cell cell = gameBoard.getCell(neighbor);
                if (cell != null && cell.canMoveTo()) {
                    queue.offer(neighbor);
                    visited.add(neighbor);
                    cameFrom.put(neighbor, current);
                }
            }
        }
        
        return new ArrayList<>(); // Путь не найден
    }
    
    public List<Position> findPathAStar(Position start, Position target) {
        if (!gameBoard.isValidPosition(start) || !gameBoard.isValidPosition(target)) {
            return new ArrayList<>();
        }
        
        PriorityQueue<PathNode> openSet = new PriorityQueue<>();
        Map<Position, Position> cameFrom = new HashMap<>();
        Map<Position, Double> gScore = new HashMap<>();
        Map<Position, Double> fScore = new HashMap<>();
        Set<Position> closedSet = new HashSet<>();
        
        openSet.offer(new PathNode(start, 0));
        gScore.put(start, 0.0);
        fScore.put(start, heuristic(start, target));
        
        while (!openSet.isEmpty()) {
            Position current = openSet.poll().position;
            
            if (current.equals(target)) {
                return reconstructPath(cameFrom, current);
            }
            
            closedSet.add(current);
            
            for (Position neighbor : current.getNeighbors()) {
                if (!gameBoard.isValidPosition(neighbor) || closedSet.contains(neighbor)) {
                    continue;
                }
                
                Cell cell = gameBoard.getCell(neighbor);
                if (cell == null || !cell.canMoveTo()) continue;
                
                double tentativeGScore = gScore.get(current) + cell.getMovementCost();
                
                if (!gScore.containsKey(neighbor) || tentativeGScore < gScore.get(neighbor)) {
                    cameFrom.put(neighbor, current);
                    gScore.put(neighbor, tentativeGScore);
                    fScore.put(neighbor, tentativeGScore + heuristic(neighbor, target));
                    
                    PathNode newNode = new PathNode(neighbor, fScore.get(neighbor));
                    if (!openSet.contains(newNode)) {
                        openSet.offer(newNode);
                    }
                }
            }
        }
        
        return new ArrayList<>(); // Путь не найден
    }
    
    private double heuristic(Position from, Position to) {
        return from.manhattanDistance(to);
    }
    
    private List<Position> reconstructPath(Map<Position, Position> cameFrom, Position current) {
        List<Position> path = new ArrayList<>();
        path.add(current);
        
        while (cameFrom.containsKey(current)) {
            current = cameFrom.get(current);
            path.add(0, current);
        }
        
        return path;
    }
    
    private static class PathNode implements Comparable<PathNode> {
        private final Position position;
        private final double fScore;
        
        public PathNode(Position position, double fScore) {
            this.position = position;
            this.fScore = fScore;
        }
        
        @Override
        public int compareTo(PathNode other) {
            return Double.compare(this.fScore, other.fScore);
        }
        
        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            PathNode pathNode = (PathNode) obj;
            return Objects.equals(position, pathNode.position);
        }
        
        @Override
        public int hashCode() {
            return Objects.hash(position);
        }
    }
}
```

#### 6. Класс `MovementSystem` (Система движения)
```java
public class MovementSystem {
    private final GameBoard gameBoard;
    private final PathFinder pathFinder;
    
    public MovementSystem(GameBoard gameBoard) {
        this.gameBoard = gameBoard;
        this.pathFinder = new PathFinder(gameBoard);
    }
    
    public boolean canMoveTo(Unit unit, Position target) {
        if (!gameBoard.isValidPosition(target)) return false;
        
        Cell targetCell = gameBoard.getCell(target);
        if (targetCell == null || !targetCell.canMoveTo()) return false;
        
        double distance = unit.getPosition().distanceTo(target);
        return distance <= unit.getMovementRange();
    }
    
    public List<Position> getReachablePositions(Unit unit) {
        List<Position> reachable = new ArrayList<>();
        Position unitPos = unit.getPosition();
        int range = unit.getMovementRange();
        
        for (int x = -range; x <= range; x++) {
            for (int y = -range; y <= range; y++) {
                Position pos = new Position(unitPos.getX() + x, unitPos.getY() + y);
                
                if (canMoveTo(unit, pos)) {
                    reachable.add(pos);
                }
            }
        }
        
        return reachable;
    }
    
    public boolean moveUnit(Unit unit, Position target) {
        if (!canMoveTo(unit, target)) return false;
        
        Position oldPosition = unit.getPosition();
        boolean success = gameBoard.moveEntity(unit, target);
        
        if (success) {
            unit.setPosition(target);
            System.out.println(unit.getName() + " переместился с " + oldPosition + " на " + target);
        }
        
        return success;
    }
    
    public boolean moveUnitAlongPath(Unit unit, Position target) {
        List<Position> path = pathFinder.findPath(unit.getPosition(), target);
        
        if (path.isEmpty()) {
            System.out.println("Путь к " + target + " не найден для " + unit.getName());
            return false;
        }
        
        // Проверяем, можем ли мы пройти весь путь
        double totalCost = 0;
        for (int i = 0; i < path.size() - 1; i++) {
            Cell cell = gameBoard.getCell(path.get(i));
            if (cell != null) {
                totalCost += cell.getMovementCost();
            }
        }
        
        if (totalCost > unit.getMovementRange()) {
            System.out.println("Цель слишком далеко для " + unit.getName());
            return false;
        }
        
        // Перемещаемся по пути
        for (Position waypoint : path) {
            if (!waypoint.equals(unit.getPosition())) {
                if (!moveUnit(unit, waypoint)) {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    public List<Position> getOptimalPath(Unit unit, Position target) {
        return pathFinder.findPath(unit.getPosition(), target);
    }
    
    public double getPathCost(List<Position> path) {
        if (path.isEmpty()) return 0;
        
        double totalCost = 0;
        for (Position pos : path) {
            Cell cell = gameBoard.getCell(pos);
            if (cell != null) {
                totalCost += cell.getMovementCost();
            }
        }
        
        return totalCost;
    }
}
```

#### 7. Обновленный класс `Unit` с системой движения
```java
public abstract class Unit extends GameEntity {
    protected int health;
    protected int maxHealth;
    protected int attack;
    protected int defense;
    protected int movementRange;
    protected int experience;
    protected int level;
    protected MovementSystem movementSystem;
    
    public Unit(String name, Position position, int health, int attack, int defense, int movementRange) {
        super(name, position);
        this.maxHealth = health;
        this.health = health;
        this.attack = attack;
        this.defense = defense;
        this.movementRange = movementRange;
        this.experience = 0;
        this.level = 1;
    }
    
    public void setMovementSystem(MovementSystem movementSystem) {
        this.movementSystem = movementSystem;
    }
    
    public boolean moveTo(Position target) {
        if (movementSystem == null) return false;
        return movementSystem.moveUnit(this, target);
    }
    
    public boolean moveAlongPath(Position target) {
        if (movementSystem == null) return false;
        return movementSystem.moveUnitAlongPath(this, target);
    }
    
    public List<Position> getReachablePositions() {
        if (movementSystem == null) return new ArrayList<>();
        return movementSystem.getReachablePositions(this);
    }
    
    public List<Position> getPathTo(Position target) {
        if (movementSystem == null) return new ArrayList<>();
        return movementSystem.getOptimalPath(this, target);
    }
    
    public boolean canMoveTo(Position target) {
        if (movementSystem == null) return false;
        return movementSystem.canMoveTo(this, target);
    }
    
    // Остальные методы остаются без изменений
    public int getMovementRange() { return movementRange; }
    public void setMovementRange(int movementRange) { this.movementRange = movementRange; }
    
    @Override
    public void update() {
        if (health < maxHealth * 0.3) {
            heal(5);
        }
    }
}
```

#### 8. Главный класс с демонстрацией движения
```java
public class Game {
    private GameBoard gameBoard;
    private MovementSystem movementSystem;
    private List<Unit> units;
    
    public Game() {
        this.gameBoard = new GameBoard(10, 10);
        this.movementSystem = new MovementSystem(gameBoard);
        this.units = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание юнитов
        Position pos1 = new Position(1, 1);
        Position pos2 = new Position(2, 1);
        
        Unit warrior = new Warrior("Александр", pos1);
        Unit archer = new Archer("Леголас", pos2);
        
        warrior.setMovementSystem(movementSystem);
        archer.setMovementSystem(movementSystem);
        
        units.add(warrior);
        units.add(archer);
        
        // Размещение юнитов на поле
        gameBoard.placeEntity(warrior, pos1);
        gameBoard.placeEntity(archer, pos2);
        
        System.out.println("Игра инициализирована");
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Демонстрация движения
        demonstrateMovement();
        
        // Демонстрация поиска пути
        demonstratePathfinding();
        
        displayGameState();
    }
    
    private void demonstrateMovement() {
        Unit warrior = units.get(0);
        Position target = new Position(5, 5);
        
        System.out.println("Попытка перемещения " + warrior.getName() + " на " + target);
        
        if (warrior.canMoveTo(target)) {
            warrior.moveTo(target);
        } else {
            System.out.println("Невозможно переместиться на " + target);
        }
    }
    
    private void demonstratePathfinding() {
        Unit archer = units.get(1);
        Position target = new Position(8, 8);
        
        System.out.println("Поиск пути для " + archer.getName() + " к " + target);
        
        List<Position> path = archer.getPathTo(target);
        if (!path.isEmpty()) {
            System.out.println("Найден путь: " + path);
            archer.moveAlongPath(target);
        } else {
            System.out.println("Путь не найден");
        }
    }
    
    private void displayGameState() {
        System.out.println("\n=== Состояние игры ===");
        
        System.out.println("Юниты:");
        for (Unit unit : units) {
            Position pos = unit.getPosition();
            Cell cell = gameBoard.getCell(pos);
            String terrain = cell != null ? cell.getTerrainType().getName() : "Неизвестно";
            System.out.println("  " + unit.getName() + " на " + pos + " (местность: " + terrain + ")");
        }
        
        System.out.println("Доступные позиции для первого юнита:");
        List<Position> reachable = units.get(0).getReachablePositions();
        reachable.stream().limit(10).forEach(pos -> System.out.println("  " + pos));
        
        if (reachable.size() > 10) {
            System.out.println("  ... и еще " + (reachable.size() - 10) + " позиций");
        }
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        
        // Играем несколько ходов
        for (int i = 0; i < 3; i++) {
            game.playTurn();
            System.out.println();
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему игрового поля и движения:

### 1. **Гонки на выживание**
- Поле: трасса с препятствиями, чекпоинты
- Движение: транспортные средства, физика движения
- Алгоритмы: поиск оптимальной траектории

### 2. **Космическая колонизация**
- Поле: космические объекты, планеты, астероиды
- Движение: космические корабли, орбитальные пути
- Алгоритмы: гравитационные расчеты, космическая навигация

### 3. **Подземелье и драконы**
- Поле: подземелья, комнаты, коридоры
- Движение: персонажи, монстры, телепортация
- Алгоритмы: исследование подземелий, поиск сокровищ

### 4. **Город-государство**
- Поле: городские районы, улицы, здания
- Движение: граждане, торговцы, армия
- Алгоритмы: городская навигация, планирование маршрутов

### 5. **Пиратская стратегия**
- Поле: острова, моря, порты
- Движение: корабли, команды, торговые пути
- Алгоритмы: морская навигация, поиск сокровищ

### 6. **Фермерское хозяйство**
- Поле: поля, фермы, рынки
- Движение: фермеры, животные, техника
- Алгоритмы: планирование сельскохозяйственных работ

### 7. **Киберпанк-тактика**
- Поле: киберсети, узлы, брандмауэры
- Движение: хакеры, вирусы, программы
- Алгоритмы: взлом систем, обход защиты

### 8. **Средневековая осада**
- Поле: замки, укрепления, осадные сооружения
- Движение: армии, осадные орудия, защитники
- Алгоритмы: тактическое планирование, осадные операции

### 9. **Зомби-выживание**
- Поле: города, убежища, опасные зоны
- Движение: выжившие, зомби, ресурсы
- Алгоритмы: поиск безопасных путей, избегание угроз

### 10. **Фэнтези-война**
- Поле: магические земли, порталы, артефакты
- Движение: магические существа, герои, армии
- Алгоритмы: магическая навигация, поиск артефактов

## Требования к реализации

### Обязательные требования:
1. **Создать систему игрового поля** с клетками и местностью
2. **Реализовать алгоритм поиска пути** (BFS или A*)
3. **Создать систему движения** для игровых объектов
4. **Реализовать проверку коллизий** и доступности позиций
5. **Демонстрировать работу** алгоритмов поиска пути
6. **Создать различные типы местности** с разными свойствами

### Дополнительные требования:
1. **Реализовать несколько алгоритмов** поиска пути
2. **Добавить динамические препятствия** на поле
3. **Создать систему команд** для группового движения
4. **Реализовать оптимизацию** для больших полей

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Игровое поле** | 4 | Создание и управление игровым полем |
| **Алгоритмы поиска** | 4 | Реализация алгоритмов поиска пути |
| **Система движения** | 3 | Управление движением объектов |
| **Коллизии** | 2 | Проверка столкновений и доступности |
| **Местность** | 2 | Различные типы местности |
| **Демонстрация** | 2 | Работающий пример движения |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. В чем разница между BFS и A* алгоритмами?
2. Как правильно организовать игровое поле?
3. Зачем нужны различные типы местности?
4. Как оптимизировать поиск пути для больших полей?
5. Что такое эвристическая функция в A*?
6. Как обрабатывать коллизии на игровом поле?
7. Как применить алгоритмы поиска пути для игр?

## Заключение

В данной лабораторной работе вы изучили принципы создания игрового поля и системы движения в Java на примере создания игровых систем. Вы научились:

- Создавать игровое поле с различными типами местности
- Реализовывать алгоритмы поиска пути
- Управлять движением игровых объектов
- Обрабатывать коллизии и доступность позиций

Полученные знания позволят вам создавать сложные игровые миры с интеллектуальным движением объектов.
