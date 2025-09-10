---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Лабораторная работа 5: Исключения и обработка ошибок

## Цель работы
Изучить механизмы обработки исключений в Java на примере разработки игровых систем. Научиться создавать пользовательские исключения, использовать try-catch блоки и реализовать логирование ошибок.

## Теоретические основы

### Исключения в Java
- **Checked Exceptions** - должны быть обработаны или объявлены
- **Unchecked Exceptions** - RuntimeException и его наследники
- **Try-catch-finally** - блоки для обработки исключений
- **Throws** - объявление исключений, которые может выбросить метод

### Пользовательские исключения
- **Наследование от Exception** для checked исключений
- **Наследование от RuntimeException** для unchecked исключений
- **Конструкторы** с сообщениями об ошибках
- **Логирование** исключений для отладки

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система исключений

#### 1. Пользовательские исключения

##### `GameException` (базовое исключение)
```java
public class GameException extends Exception {
    private final String errorCode;
    private final LocalDateTime timestamp;
    
    public GameException(String message) {
        super(message);
        this.errorCode = "GAME_ERROR";
        this.timestamp = LocalDateTime.now();
    }
    
    public GameException(String message, String errorCode) {
        super(message);
        this.errorCode = errorCode;
        this.timestamp = LocalDateTime.now();
    }
    
    public GameException(String message, Throwable cause) {
        super(message, cause);
        this.errorCode = "GAME_ERROR";
        this.timestamp = LocalDateTime.now();
    }
    
    public String getErrorCode() { return errorCode; }
    public LocalDateTime getTimestamp() { return timestamp; }
    
    @Override
    public String toString() {
        return String.format("[%s] %s: %s", errorCode, timestamp, getMessage());
    }
}
```

##### `InvalidPositionException`
```java
public class InvalidPositionException extends GameException {
    private final Position invalidPosition;
    
    public InvalidPositionException(Position position) {
        super("Некорректная позиция: " + position);
        this.invalidPosition = position;
    }
    
    public InvalidPositionException(Position position, String reason) {
        super("Некорректная позиция " + position + ": " + reason);
        this.invalidPosition = position;
    }
    
    public Position getInvalidPosition() { return invalidPosition; }
}
```

##### `InsufficientResourceException`
```java
public class InsufficientResourceException extends GameException {
    private final String resourceName;
    private final int required;
    private final int available;
    
    public InsufficientResourceException(String resourceName, int required, int available) {
        super(String.format("Недостаточно ресурса %s: требуется %d, доступно %d", 
                          resourceName, required, available));
        this.resourceName = resourceName;
        this.required = required;
        this.available = available;
    }
    
    public String getResourceName() { return resourceName; }
    public int getRequired() { return required; }
    public int getAvailable() { return available; }
}
```

##### `UnitActionException`
```java
public class UnitActionException extends GameException {
    private final String unitName;
    private final String action;
    
    public UnitActionException(String unitName, String action, String reason) {
        super(String.format("Юнит %s не может выполнить действие %s: %s", 
                          unitName, action, reason));
        this.unitName = unitName;
        this.action = action;
    }
    
    public String getUnitName() { return unitName; }
    public String getAction() { return action; }
}
```

#### 2. Класс `GameLogger` для логирования
```java
public class GameLogger {
    private static final Logger logger = Logger.getLogger(GameLogger.class.getName());
    private final String gameId;
    private final List<LogEntry> logHistory;
    
    public GameLogger(String gameId) {
        this.gameId = gameId;
        this.logHistory = new ArrayList<>();
        setupLogger();
    }
    
    private void setupLogger() {
        try {
            FileHandler fileHandler = new FileHandler("game_" + gameId + ".log");
            fileHandler.setFormatter(new SimpleFormatter());
            logger.addHandler(fileHandler);
            logger.setLevel(Level.ALL);
        } catch (IOException e) {
            System.err.println("Ошибка настройки логгера: " + e.getMessage());
        }
    }
    
    public void logInfo(String message) {
        logger.info(String.format("[%s] %s", gameId, message));
        addToHistory("INFO", message);
    }
    
    public void logWarning(String message) {
        logger.warning(String.format("[%s] %s", gameId, message));
        addToHistory("WARNING", message);
    }
    
    public void logError(String message, Throwable exception) {
        logger.severe(String.format("[%s] %s", gameId, message));
        logger.log(Level.SEVERE, message, exception);
        addToHistory("ERROR", message + " - " + exception.getMessage());
    }
    
    public void logException(GameException exception) {
        String message = String.format("Игровое исключение [%s]: %s", 
                                     exception.getErrorCode(), exception.getMessage());
        logger.severe(message);
        addToHistory("EXCEPTION", message);
    }
    
    private void addToHistory(String level, String message) {
        logHistory.add(new LogEntry(level, message, LocalDateTime.now()));
    }
    
    public List<LogEntry> getLogHistory() { return new ArrayList<>(logHistory); }
    
    public void clearHistory() { logHistory.clear(); }
    
    private static class LogEntry {
        private final String level;
        private final String message;
        private final LocalDateTime timestamp;
        
        public LogEntry(String level, String message, LocalDateTime timestamp) {
            this.level = level;
            this.message = message;
            this.timestamp = timestamp;
        }
        
        // Геттеры
        public String getLevel() { return level; }
        public String getMessage() { return message; }
        public LocalDateTime getTimestamp() { return timestamp; }
    }
}
```

#### 3. Класс `GameValidator` для валидации
```java
public class GameValidator {
    private final GameLogger logger;
    
    public GameValidator(GameLogger logger) {
        this.logger = logger;
    }
    
    public void validatePosition(Position position, int maxX, int maxY) 
            throws InvalidPositionException {
        try {
            if (position.getX() < 0 || position.getX() >= maxX) {
                throw new InvalidPositionException(position, 
                    "X координата вне диапазона [0, " + (maxX - 1) + "]");
            }
            if (position.getY() < 0 || position.getY() >= maxY) {
                throw new InvalidPositionException(position, 
                    "Y координата вне диапазона [0, " + (maxY - 1) + "]");
            }
            logger.logInfo("Позиция " + position + " валидна");
        } catch (InvalidPositionException e) {
            logger.logError("Ошибка валидации позиции", e);
            throw e;
        }
    }
    
    public void validateResourceAmount(String resourceName, int required, int available) 
            throws InsufficientResourceException {
        try {
            if (required > available) {
                throw new InsufficientResourceException(resourceName, required, available);
            }
            logger.logInfo("Ресурс " + resourceName + " достаточен: " + required + "/" + available);
        } catch (InsufficientResourceException e) {
            logger.logError("Ошибка проверки ресурсов", e);
            throw e;
        }
    }
    
    public void validateUnitAction(Unit unit, String action, Position targetPosition) 
            throws UnitActionException {
        try {
            if (!unit.isAlive()) {
                throw new UnitActionException(unit.getName(), action, "Юнит мертв");
            }
            if (unit.getPosition().distanceTo(targetPosition) > unit.getMovementRange()) {
                throw new UnitActionException(unit.getName(), action, 
                    "Цель слишком далеко (расстояние: " + 
                    unit.getPosition().distanceTo(targetPosition) + 
                    ", максимум: " + unit.getMovementRange() + ")");
            }
            logger.logInfo("Действие " + action + " юнита " + unit.getName() + " валидно");
        } catch (UnitActionException e) {
            logger.logError("Ошибка валидации действия юнита", e);
            throw e;
        }
    }
}
```

#### 4. Обновленный класс `GameWorld` с обработкой исключений
```java
public class GameWorld {
    private final GameLogger logger;
    private final GameValidator validator;
    private final Map<Position, GameEntity> entityMap;
    private final List<Unit> units;
    private final List<Building> buildings;
    private final Map<String, Resource> resources;
    private final int maxWidth;
    private final int maxHeight;
    
    public GameWorld(int maxWidth, int maxHeight, String gameId) {
        this.logger = new GameLogger(gameId);
        this.validator = new GameValidator(logger);
        this.entityMap = new HashMap<>();
        this.units = new ArrayList<>();
        this.buildings = new LinkedList<>();
        this.resources = new TreeMap<>();
        this.maxWidth = maxWidth;
        this.maxHeight = maxHeight;
        
        logger.logInfo("Игровой мир создан с размерами " + maxWidth + "x" + maxHeight);
    }
    
    public void addEntity(GameEntity entity) {
        try {
            // Валидация позиции
            validator.validatePosition(entity.getPosition(), maxWidth, maxHeight);
            
            // Проверка занятости позиции
            if (entityMap.containsKey(entity.getPosition())) {
                throw new GameException("Позиция " + entity.getPosition() + " уже занята");
            }
            
            // Добавление сущности
            entityMap.put(entity.getPosition(), entity);
            
            if (entity instanceof Unit) {
                units.add((Unit) entity);
                logger.logInfo("Добавлен юнит " + entity.getName() + " на позицию " + entity.getPosition());
            } else if (entity instanceof Building) {
                buildings.add((Building) entity);
                logger.logInfo("Добавлено здание " + entity.getName() + " на позицию " + entity.getPosition());
            }
            
        } catch (GameException e) {
            logger.logException(e);
            throw e;
        } catch (Exception e) {
            logger.logError("Неожиданная ошибка при добавлении сущности", e);
            throw new GameException("Ошибка добавления сущности: " + e.getMessage(), e);
        }
    }
    
    public void moveUnit(Unit unit, Position newPosition) {
        try {
            // Валидация действия
            validator.validateUnitAction(unit, "перемещение", newPosition);
            
            // Валидация новой позиции
            validator.validatePosition(newPosition, maxWidth, maxHeight);
            
            // Проверка занятости новой позиции
            if (entityMap.containsKey(newPosition)) {
                throw new UnitActionException(unit.getName(), "перемещение", 
                    "Позиция " + newPosition + " занята");
            }
            
            // Перемещение
            Position oldPosition = unit.getPosition();
            entityMap.remove(oldPosition);
            entityMap.put(newPosition, unit);
            unit.setPosition(newPosition);
            
            logger.logInfo("Юнит " + unit.getName() + " перемещен с " + oldPosition + " на " + newPosition);
            
        } catch (GameException e) {
            logger.logException(e);
            throw e;
        } catch (Exception e) {
            logger.logError("Неожиданная ошибка при перемещении юнита", e);
            throw new GameException("Ошибка перемещения юнита: " + e.getMessage(), e);
        }
    }
    
    public void consumeResource(String resourceName, int amount) {
        try {
            Resource resource = resources.get(resourceName);
            if (resource == null) {
                throw new GameException("Ресурс " + resourceName + " не найден");
            }
            
            // Валидация количества
            validator.validateResourceAmount(resourceName, amount, resource.getAmount());
            
            // Потребление ресурса
            resource.consumeAmount(amount);
            logger.logInfo("Потреблено " + amount + " ресурса " + resourceName);
            
        } catch (GameException e) {
            logger.logException(e);
            throw e;
        } catch (Exception e) {
            logger.logError("Неожиданная ошибка при потреблении ресурса", e);
            throw new GameException("Ошибка потребления ресурса: " + e.getMessage(), e);
        }
    }
    
    public void attackUnit(Unit attacker, Unit target) {
        try {
            // Валидация атаки
            validator.validateUnitAction(attacker, "атака", target.getPosition());
            
            if (!target.isAlive()) {
                throw new UnitActionException(attacker.getName(), "атака", 
                    "Цель " + target.getName() + " уже мертва");
            }
            
            // Выполнение атаки
            int damage = attacker.attack(target);
            logger.logInfo("Юнит " + attacker.getName() + " нанес " + damage + 
                          " урона юниту " + target.getName());
            
            if (!target.isAlive()) {
                logger.logInfo("Юнит " + target.getName() + " погиб");
                removeEntity(target.getPosition());
            }
            
        } catch (GameException e) {
            logger.logException(e);
            throw e;
        } catch (Exception e) {
            logger.logError("Неожиданная ошибка при атаке", e);
            throw new GameException("Ошибка атаки: " + e.getMessage(), e);
        }
    }
    
    private void removeEntity(Position position) {
        GameEntity entity = entityMap.remove(position);
        if (entity != null) {
            if (entity instanceof Unit) {
                units.remove(entity);
            } else if (entity instanceof Building) {
                buildings.remove(entity);
            }
            logger.logInfo("Сущность " + entity.getName() + " удалена с позиции " + position);
        }
    }
    
    // Геттеры
    public GameLogger getLogger() { return logger; }
    public Map<Position, GameEntity> getEntityMap() { return entityMap; }
    public List<Unit> getUnits() { return units; }
    public List<Building> getBuildings() { return buildings; }
    public Map<String, Resource> getResources() { return resources; }
}
```

#### 5. Главный класс с обработкой исключений
```java
public class Game {
    private GameWorld world;
    private final GameLogger logger;
    
    public Game(String gameId) {
        this.logger = new GameLogger(gameId);
        this.world = new GameWorld(10, 10, gameId);
        
        try {
            initializeGame();
            logger.logInfo("Игра успешно инициализирована");
        } catch (Exception e) {
            logger.logError("Ошибка инициализации игры", e);
            throw new GameException("Не удалось инициализировать игру", e);
        }
    }
    
    private void initializeGame() {
        try {
            // Создание юнитов
            Position pos1 = new Position(1, 1);
            Position pos2 = new Position(2, 1);
            
            Unit warrior = new Warrior("Александр", pos1);
            Unit archer = new Archer("Леголас", pos2);
            
            world.addEntity(warrior);
            world.addEntity(archer);
            
            // Создание ресурсов
            Resource gold = new BasicResource("Золото", 1000, 10000, 10);
            world.getResources().put("Золото", gold);
            
            logger.logInfo("Игра инициализирована: 2 юнита, 1 ресурс");
            
        } catch (Exception e) {
            logger.logError("Ошибка при создании игровых объектов", e);
            throw new GameException("Ошибка инициализации игровых объектов", e);
        }
    }
    
    public void playTurn() {
        try {
            logger.logInfo("=== Начало хода ===");
            
            // Демонстрация различных операций с обработкой исключений
            demonstrateGameOperations();
            
            logger.logInfo("=== Ход завершен ===");
            
        } catch (GameException e) {
            logger.logException(e);
            handleGameException(e);
        } catch (Exception e) {
            logger.logError("Критическая ошибка в ходе игры", e);
            throw new GameException("Критическая ошибка игры", e);
        }
    }
    
    private void demonstrateGameOperations() {
        try {
            // Попытка перемещения юнита
            Unit warrior = world.getUnits().get(0);
            Position newPos = new Position(3, 3);
            world.moveUnit(warrior, newPos);
            
        } catch (UnitActionException e) {
            logger.logWarning("Не удалось переместить юнита: " + e.getMessage());
        }
        
        try {
            // Попытка атаки
            Unit attacker = world.getUnits().get(0);
            Unit target = world.getUnits().get(1);
            world.attackUnit(attacker, target);
            
        } catch (UnitActionException e) {
            logger.logWarning("Не удалось атаковать: " + e.getMessage());
        }
        
        try {
            // Попытка потребления ресурса
            world.consumeResource("Золото", 100);
            
        } catch (InsufficientResourceException e) {
            logger.logWarning("Недостаточно ресурса: " + e.getMessage());
        }
    }
    
    private void handleGameException(GameException e) {
        switch (e.getErrorCode()) {
            case "INVALID_POSITION":
                logger.logWarning("Обработка ошибки позиции: " + e.getMessage());
                break;
            case "INSUFFICIENT_RESOURCE":
                logger.logWarning("Обработка недостатка ресурсов: " + e.getMessage());
                break;
            case "UNIT_ACTION":
                logger.logWarning("Обработка ошибки действия юнита: " + e.getMessage());
                break;
            default:
                logger.logWarning("Неизвестная ошибка: " + e.getMessage());
        }
    }
    
    public void displayGameState() {
        try {
            System.out.println("\n=== Состояние игры ===");
            System.out.println("Юнитов: " + world.getUnits().size());
            System.out.println("Зданий: " + world.getBuildings().size());
            System.out.println("Ресурсов: " + world.getResources().size());
            
            // Отображение истории логов
            System.out.println("\n=== Последние события ===");
            List<LogEntry> recentLogs = world.getLogger().getLogHistory();
            recentLogs.stream()
                     .filter(log -> log.getLevel().equals("INFO"))
                     .limit(5)
                     .forEach(log -> System.out.println(log.getMessage()));
                     
        } catch (Exception e) {
            logger.logError("Ошибка отображения состояния игры", e);
        }
    }
    
    public static void main(String[] args) {
        try {
            Game game = new Game("KINGDOM_001");
            
            // Играем несколько ходов
            for (int i = 0; i < 3; i++) {
                game.playTurn();
                game.displayGameState();
                System.out.println();
            }
            
        } catch (GameException e) {
            System.err.println("Критическая ошибка игры: " + e.getMessage());
            e.printStackTrace();
        } catch (Exception e) {
            System.err.println("Неожиданная ошибка: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему исключений:

### 1. **Гонки на выживание**
- Исключения: `InvalidTrackException`, `VehicleDamageException`, `FuelException`
- Валидация: трассы, состояния транспорта, топлива

### 2. **Космическая колонизация**
- Исключения: `PlanetException`, `ColonyException`, `TechnologyException`
- Валидация: планет, колоний, технологий

### 3. **Подземелье и драконы**
- Исключения: `DungeonException`, `CharacterException`, `MagicException`
- Валидация: подземелий, персонажей, магии

### 4. **Город-государство**
- Исключения: `CityException`, `PolicyException`, `InfrastructureException`
- Валидация: городов, политик, инфраструктуры

### 5. **Пиратская стратегия**
- Исключения: `ShipException`, `TreasureException`, `PortException`
- Валидация: кораблей, сокровищ, портов

### 6. **Фермерское хозяйство**
- Исключения: `FieldException`, `CropException`, `AnimalException`
- Валидация: полей, культур, животных

### 7. **Киберпанк-тактика**
- Исключения: `CyberException`, `HackException`, `GridException`
- Валидация: кибернетики, хакерских атак, сетей

### 8. **Средневековая осада**
- Исключения: `CastleException`, `SiegeException`, `DefenseException`
- Валидация: замков, осадных орудий, обороны

### 9. **Зомби-выживание**
- Исключения: `SurvivalException`, `ThreatException`, `SafeZoneException`
- Валидация: выживания, угроз, безопасных зон

### 10. **Фэнтези-война**
- Исключения: `FantasyException`, `MagicException`, `CreatureException`
- Валидация: фэнтези, магии, существ

## Требования к реализации

### Обязательные требования:
1. **Создать минимум 4 пользовательских исключения** для выбранной игры
2. **Реализовать класс валидации** с проверками
3. **Создать систему логирования** для отслеживания ошибок
4. **Применить try-catch блоки** во всех критических операциях
5. **Демонстрировать обработку** различных типов исключений
6. **Создать информативные сообщения** об ошибках

### Дополнительные требования:
1. **Реализовать восстановление** после ошибок
2. **Добавить метрики** ошибок и их обработки
3. **Создать систему уведомлений** об ошибках
4. **Реализовать паттерн "Retry"** для повторных попыток

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Пользовательские исключения** | 4 | Создание и использование исключений |
| **Валидация** | 3 | Реализация проверок данных |
| **Логирование** | 3 | Система записи событий и ошибок |
| **Обработка исключений** | 3 | Try-catch блоки и восстановление |
| **Информативность** | 2 | Понятные сообщения об ошибках |
| **Демонстрация** | 2 | Работающий пример обработки ошибок |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. В чем разница между checked и unchecked исключениями?
2. Когда использовать try-catch, а когда throws?
3. Зачем нужны пользовательские исключения?
4. Как правильно организовать логирование ошибок?
5. Что такое валидация и зачем она нужна?
6. Как восстановиться после исключения?
7. Как применить исключения для игровых систем?

## Заключение

В данной лабораторной работе вы изучили механизмы обработки исключений в Java на примере создания игровых систем. Вы научились:

- Создавать пользовательские исключения
- Реализовывать валидацию данных
- Организовывать логирование ошибок
- Обрабатывать исключения в игровой логике

Полученные знания позволят вам создавать надежные игровые системы с качественной обработкой ошибок.
