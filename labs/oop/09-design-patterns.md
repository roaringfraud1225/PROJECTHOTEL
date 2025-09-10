# Лабораторная работа 9: Паттерны проектирования

## Цель работы
Изучить основные паттерны проектирования в Java на примере разработки игровых систем. Научиться применять Singleton, Factory, Observer, Strategy и другие паттерны для создания гибкого и расширяемого кода.

## Теоретические основы

### Паттерны проектирования
- **Порождающие паттерны** - создание объектов
- **Структурные паттерны** - композиция объектов
- **Поведенческие паттерны** - взаимодействие объектов
- **Принципы SOLID** - основы хорошего дизайна

### Применение в играх
- **Гибкость** - легкость изменения поведения
- **Расширяемость** - добавление новых возможностей
- **Переиспользование** - общие решения для разных задач
- **Тестируемость** - простота написания тестов

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Применение паттернов проектирования

#### 1. Singleton (Одиночка) - `GameManager`
```java
public class GameManager {
    private static GameManager instance;
    private GameState gameState;
    private ResourceManager resourceManager;
    private BuildingManager buildingManager;
    private UnitManager unitManager;
    private EventManager eventManager;
    
    private GameManager() {
        this.gameState = GameState.MENU;
        this.resourceManager = new ResourceManager();
        this.buildingManager = new BuildingManager(resourceManager);
        this.unitManager = new UnitManager();
        this.eventManager = new EventManager();
    }
    
    public static GameManager getInstance() {
        if (instance == null) {
            instance = new GameManager();
        }
        return instance;
    }
    
    public void startNewGame() {
        gameState = GameState.PLAYING;
        resourceManager.initializeResources();
        buildingManager.clearBuildings();
        unitManager.clearUnits();
        eventManager.clearEvents();
        System.out.println("Новая игра начата");
    }
    
    public void pauseGame() {
        if (gameState == GameState.PLAYING) {
            gameState = GameState.PAUSED;
            System.out.println("Игра приостановлена");
        }
    }
    
    public void resumeGame() {
        if (gameState == GameState.PAUSED) {
            gameState = GameState.PLAYING;
            System.out.println("Игра возобновлена");
        }
    }
    
    public void endGame() {
        gameState = GameState.GAME_OVER;
        System.out.println("Игра завершена");
    }
    
    public GameState getGameState() { return gameState; }
    public ResourceManager getResourceManager() { return resourceManager; }
    public BuildingManager getBuildingManager() { return buildingManager; }
    public UnitManager getUnitManager() { return unitManager; }
    public EventManager getEventManager() { return eventManager; }
    
    public enum GameState {
        MENU, PLAYING, PAUSED, GAME_OVER
    }
}
```

#### 2. Factory Method (Фабричный метод) - `UnitFactory`
```java
public abstract class UnitFactory {
    protected Map<String, Integer> baseCost;
    
    public UnitFactory() {
        this.baseCost = new HashMap<>();
        initializeBaseCost();
    }
    
    protected abstract void initializeBaseCost();
    protected abstract Unit createUnit(String type, Position position);
    
    public Unit createUnitWithType(String type, Position position) {
        Unit unit = createUnit(type, position);
        if (unit != null) {
            System.out.println("Создан " + type + " на позиции " + position);
        }
        return unit;
    }
    
    public boolean canAffordUnit(String type, ResourceManager resourceManager) {
        Map<String, Integer> cost = getUnitCost(type);
        for (Map.Entry<String, Integer> entry : cost.entrySet()) {
            if (!resourceManager.hasResource(entry.getKey(), entry.getValue())) {
                return false;
            }
        }
        return true;
    }
    
    public Map<String, Integer> getUnitCost(String type) {
        Map<String, Integer> cost = new HashMap<>(baseCost);
        // Модифицируем стоимость в зависимости от типа юнита
        switch (type.toLowerCase()) {
            case "warrior":
                cost.put("Золото", cost.getOrDefault("Золото", 0) + 20);
                break;
            case "archer":
                cost.put("Золото", cost.getOrDefault("Золото", 0) + 30);
                cost.put("Дерево", cost.getOrDefault("Дерево", 0) + 15);
                break;
            case "mage":
                cost.put("Золото", cost.getOrDefault("Золото", 0) + 50);
                cost.put("Мана", cost.getOrDefault("Мана", 0) + 25);
                break;
        }
        return cost;
    }
}

public class MilitaryUnitFactory extends UnitFactory {
    @Override
    protected void initializeBaseCost() {
        baseCost.put("Золото", 10);
        baseCost.put("Еда", 5);
    }
    
    @Override
    protected Unit createUnit(String type, Position position) {
        switch (type.toLowerCase()) {
            case "warrior":
                return new Warrior(position, 100, 15, 5);
            case "archer":
                return new Archer(position, 80, 10, 8);
            case "mage":
                return new Mage(position, 60, 8, 12);
            default:
                System.out.println("Неизвестный тип юнита: " + type);
                return null;
        }
    }
}
```

#### 3. Abstract Factory (Абстрактная фабрика) - `GameEntityFactory`
```java
public interface GameEntityFactory {
    Unit createUnit(String type, Position position);
    Building createBuilding(String type, Position position);
    Resource createResource(String type, int amount);
}

public class KingdomEntityFactory implements GameEntityFactory {
    @Override
    public Unit createUnit(String type, Position position) {
        MilitaryUnitFactory unitFactory = new MilitaryUnitFactory();
        return unitFactory.createUnitWithType(type, position);
    }
    
    @Override
    public Building createBuilding(String type, Position position) {
        BuildingFactory buildingFactory = new BuildingFactory();
        return buildingFactory.createBuilding(type, position);
    }
    
    @Override
    public Resource createResource(String type, Position position) {
        ResourceFactory resourceFactory = new ResourceFactory();
        return resourceFactory.createResource(type, amount);
    }
}

public class BuildingFactory {
    public Building createBuilding(String type, Position position) {
        switch (type.toLowerCase()) {
            case "house":
                return new ResidentialBuilding("Дом", position, 20);
            case "farm":
                return new ProductionBuilding("Ферма", position, "Еда", 10);
            case "barracks":
                return new MilitaryBuilding("Казармы", position, 15, 8);
            case "mine":
                return new ProductionBuilding("Шахта", position, "Железо", 5);
            default:
                System.out.println("Неизвестный тип здания: " + type);
                return null;
        }
    }
}
```

#### 4. Observer (Наблюдатель) - `EventSystem`
```java
public interface GameObserver {
    void onGameEvent(GameEvent event);
}

public abstract class GameEvent {
    private String type;
    private LocalDateTime timestamp;
    private Map<String, Object> data;
    
    public GameEvent(String type) {
        this.type = type;
        this.timestamp = LocalDateTime.now();
        this.data = new HashMap<>();
    }
    
    public void addData(String key, Object value) {
        data.put(key, value);
    }
    
    public Object getData(String key) {
        return data.get(key);
    }
    
    public String getType() { return type; }
    public LocalDateTime getTimestamp() { return timestamp; }
    public Map<String, Object> getData() { return data; }
}

public class UnitMoveEvent extends GameEvent {
    public UnitMoveEvent(Unit unit, Position from, Position to) {
        super("UNIT_MOVE");
        addData("unit", unit);
        addData("from", from);
        addData("to", to);
    }
}

public class ResourceChangeEvent extends GameEvent {
    public ResourceChangeEvent(String resourceName, int oldAmount, int newAmount) {
        super("RESOURCE_CHANGE");
        addData("resourceName", resourceName);
        addData("oldAmount", oldAmount);
        addData("newAmount", newAmount);
    }
}

public class EventManager {
    private Map<String, List<GameObserver>> observers;
    private List<GameEvent> eventHistory;
    
    public EventManager() {
        this.observers = new HashMap<>();
        this.eventHistory = new ArrayList<>();
    }
    
    public void subscribe(String eventType, GameObserver observer) {
        observers.computeIfAbsent(eventType, k -> new ArrayList<>()).add(observer);
    }
    
    public void unsubscribe(String eventType, GameObserver observer) {
        List<GameObserver> eventObservers = observers.get(eventType);
        if (eventObservers != null) {
            eventObservers.remove(observer);
        }
    }
    
    public void publishEvent(GameEvent event) {
        eventHistory.add(event);
        
        List<GameObserver> eventObservers = observers.get(event.getType());
        if (eventObservers != null) {
            for (GameObserver observer : eventObservers) {
                observer.onGameEvent(event);
            }
        }
        
        // Также уведомляем наблюдателей всех событий
        List<GameObserver> allEventObservers = observers.get("*");
        if (allEventObservers != null) {
            for (GameObserver observer : allEventObservers) {
                observer.onGameEvent(event);
            }
        }
    }
    
    public List<GameEvent> getEventHistory() { return eventHistory; }
    public void clearEvents() { eventHistory.clear(); }
}
```

#### 5. Strategy (Стратегия) - `CombatSystem`
```java
public interface CombatStrategy {
    int calculateDamage(Unit attacker, Unit defender);
    boolean canAttack(Unit attacker, Unit defender);
    String getStrategyName();
}

public class MeleeCombatStrategy implements CombatStrategy {
    @Override
    public int calculateDamage(Unit attacker, Unit defender) {
        int baseDamage = attacker.getAttackPower();
        int defense = defender.getDefense();
        int damage = Math.max(1, baseDamage - defense);
        
        // Бонус за близость
        if (isAdjacent(attacker.getPosition(), defender.getPosition())) {
            damage = (int)(damage * 1.2);
        }
        
        return damage;
    }
    
    @Override
    public boolean canAttack(Unit attacker, Unit defender) {
        return isAdjacent(attacker.getPosition(), defender.getPosition());
    }
    
    @Override
    public String getStrategyName() {
        return "Ближний бой";
    }
    
    private boolean isAdjacent(Position pos1, Position pos2) {
        int dx = Math.abs(pos1.getX() - pos2.getX());
        int dy = Math.abs(pos1.getY() - pos2.getY());
        return dx <= 1 && dy <= 1 && (dx + dy > 0);
    }
}

public class RangedCombatStrategy implements CombatStrategy {
    private int range;
    
    public RangedCombatStrategy(int range) {
        this.range = range;
    }
    
    @Override
    public int calculateDamage(Unit attacker, Unit defender) {
        int baseDamage = attacker.getAttackPower();
        int defense = defender.getDefense();
        int damage = Math.max(1, baseDamage - defense);
        
        // Штраф за дальность
        int distance = calculateDistance(attacker.getPosition(), defender.getPosition());
        if (distance > range / 2) {
            damage = (int)(damage * 0.8);
        }
        
        return damage;
    }
    
    @Override
    public boolean canAttack(Unit attacker, Unit defender) {
        int distance = calculateDistance(attacker.getPosition(), defender.getPosition());
        return distance <= range;
    }
    
    @Override
    public String getStrategyName() {
        return "Дальний бой (дистанция: " + range + ")";
    }
    
    private int calculateDistance(Position pos1, Position pos2) {
        int dx = Math.abs(pos1.getX() - pos2.getX());
        int dy = Math.abs(pos1.getY() - pos2.getY());
        return Math.max(dx, dy);
    }
}

public class CombatSystem {
    private Map<String, CombatStrategy> strategies;
    private CombatStrategy defaultStrategy;
    
    public CombatSystem() {
        this.strategies = new HashMap<>();
        this.defaultStrategy = new MeleeCombatStrategy();
        
        // Регистрируем стратегии
        strategies.put("melee", new MeleeCombatStrategy());
        strategies.put("ranged", new RangedCombatStrategy(3));
        strategies.put("magic", new MagicCombatStrategy());
    }
    
    public void setStrategy(String strategyName) {
        CombatStrategy strategy = strategies.get(strategyName);
        if (strategy != null) {
            defaultStrategy = strategy;
        }
    }
    
    public CombatResult executeCombat(Unit attacker, Unit defender) {
        CombatStrategy strategy = getBestStrategy(attacker, defender);
        
        if (!strategy.canAttack(attacker, defender)) {
            return new CombatResult(false, 0, "Атака невозможна");
        }
        
        int damage = strategy.calculateDamage(attacker, defender);
        defender.takeDamage(damage);
        
        String message = String.format("%s атакует %s используя %s. Урон: %d", 
                                     attacker.getName(), defender.getName(), 
                                     strategy.getStrategyName(), damage);
        
        return new CombatResult(true, damage, message);
    }
    
    private CombatStrategy getBestStrategy(Unit attacker, Unit defender) {
        // Выбираем лучшую стратегию для атаки
        for (CombatStrategy strategy : strategies.values()) {
            if (strategy.canAttack(attacker, defender)) {
                return strategy;
            }
        }
        return defaultStrategy;
    }
    
    public void addStrategy(String name, CombatStrategy strategy) {
        strategies.put(name, strategy);
    }
}
```

#### 6. Command (Команда) - `ActionSystem`
```java
public interface GameCommand {
    void execute();
    void undo();
    String getDescription();
}

public class MoveUnitCommand implements GameCommand {
    private Unit unit;
    private Position fromPosition;
    private Position toPosition;
    private boolean executed;
    
    public MoveUnitCommand(Unit unit, Position toPosition) {
        this.unit = unit;
        this.fromPosition = unit.getPosition();
        this.toPosition = toPosition;
        this.executed = false;
    }
    
    @Override
    public void execute() {
        if (!executed) {
            unit.setPosition(toPosition);
            executed = true;
            
            // Публикуем событие
            GameManager.getInstance().getEventManager()
                .publishEvent(new UnitMoveEvent(unit, fromPosition, toPosition));
        }
    }
    
    @Override
    public void undo() {
        if (executed) {
            unit.setPosition(fromPosition);
            executed = false;
            
            // Публикуем событие отмены
            GameManager.getInstance().getEventManager()
                .publishEvent(new UnitMoveEvent(unit, toPosition, fromPosition));
        }
    }
    
    @Override
    public String getDescription() {
        return String.format("Переместить %s из %s в %s", 
                           unit.getName(), fromPosition, toPosition);
    }
}

public class BuildCommand implements GameCommand {
    private Building building;
    private Position position;
    private boolean executed;
    
    public BuildCommand(Building building, Position position) {
        this.building = building;
        this.position = position;
        this.executed = false;
    }
    
    @Override
    public void execute() {
        if (!executed) {
            GameManager.getInstance().getBuildingManager().build(building);
            executed = true;
        }
    }
    
    @Override
    public void undo() {
        if (executed) {
            GameManager.getInstance().getBuildingManager().demolish(position);
            executed = false;
        }
    }
    
    @Override
    public String getDescription() {
        return String.format("Построить %s на позиции %s", 
                           building.getName(), position);
    }
}

public class CommandManager {
    private List<GameCommand> commandHistory;
    private List<GameCommand> undoStack;
    private int maxHistorySize;
    
    public CommandManager(int maxHistorySize) {
        this.commandHistory = new ArrayList<>();
        this.undoStack = new ArrayList<>();
        this.maxHistorySize = maxHistorySize;
    }
    
    public void executeCommand(GameCommand command) {
        command.execute();
        commandHistory.add(command);
        
        // Ограничиваем размер истории
        if (commandHistory.size() > maxHistorySize) {
            commandHistory.remove(0);
        }
        
        // Очищаем стек отмены при выполнении новой команды
        undoStack.clear();
    }
    
    public void undo() {
        if (!commandHistory.isEmpty()) {
            GameCommand command = commandHistory.remove(commandHistory.size() - 1);
            command.undo();
            undoStack.add(command);
        }
    }
    
    public void redo() {
        if (!undoStack.isEmpty()) {
            GameCommand command = undoStack.remove(undoStack.size() - 1);
            command.execute();
            commandHistory.add(command);
        }
    }
    
    public List<GameCommand> getCommandHistory() { return commandHistory; }
    public List<GameCommand> getUndoStack() { return undoStack; }
    public void clearHistory() { commandHistory.clear(); undoStack.clear(); }
}
```

#### 7. Decorator (Декоратор) - `UnitEnhancements`
```java
public abstract class UnitDecorator extends Unit {
    protected Unit decoratedUnit;
    
    public UnitDecorator(Unit unit) {
        super(unit.getPosition(), unit.getMaxHealth(), unit.getAttackPower(), unit.getDefense());
        this.decoratedUnit = unit;
    }
    
    @Override
    public String getName() {
        return decoratedUnit.getName();
    }
    
    @Override
    public int getCurrentHealth() {
        return decoratedUnit.getCurrentHealth();
    }
    
    @Override
    public int getMaxHealth() {
        return decoratedUnit.getMaxHealth();
    }
    
    @Override
    public int getAttackPower() {
        return decoratedUnit.getAttackPower();
    }
    
    @Override
    public int getDefense() {
        return decoratedUnit.getDefense();
    }
    
    @Override
    public void takeDamage(int damage) {
        decoratedUnit.takeDamage(damage);
    }
    
    @Override
    public void heal(int amount) {
        decoratedUnit.heal(amount);
    }
    
    @Override
    public boolean isAlive() {
        return decoratedUnit.isAlive();
    }
}

public class ArmoredUnit extends UnitDecorator {
    private int armorBonus;
    
    public ArmoredUnit(Unit unit, int armorBonus) {
        super(unit);
        this.armorBonus = armorBonus;
    }
    
    @Override
    public int getDefense() {
        return decoratedUnit.getDefense() + armorBonus;
    }
    
    @Override
    public String getName() {
        return decoratedUnit.getName() + " (Бронированный)";
    }
}

public class EnchantedUnit extends UnitDecorator {
    private int magicBonus;
    
    public EnchantedUnit(Unit unit, int magicBonus) {
        super(unit);
        this.magicBonus = magicBonus;
    }
    
    @Override
    public int getAttackPower() {
        return decoratedUnit.getAttackPower() + magicBonus;
    }
    
    @Override
    public String getName() {
        return decoratedUnit.getName() + " (Зачарованный)";
    }
}
```

#### 8. Главный класс с демонстрацией паттернов
```java
public class Game {
    private GameManager gameManager;
    private EventManager eventManager;
    private CombatSystem combatSystem;
    private CommandManager commandManager;
    private GameEntityFactory entityFactory;
    
    public Game() {
        this.gameManager = GameManager.getInstance();
        this.eventManager = gameManager.getEventManager();
        this.combatSystem = new CombatSystem();
        this.commandManager = new CommandManager(100);
        this.entityFactory = new KingdomEntityFactory();
        
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация игры с паттернами проектирования...");
        
        // Подписываемся на события
        eventManager.subscribe("UNIT_MOVE", new GameLogger());
        eventManager.subscribe("RESOURCE_CHANGE", new GameLogger());
        eventManager.subscribe("*", new GameStatistics());
        
        // Создаем юнитов через фабрику
        Unit warrior = entityFactory.createUnit("warrior", new Position(1, 1));
        Unit archer = entityFactory.createUnit("archer", new Position(2, 1));
        
        // Создаем здания через фабрику
        Building house = entityFactory.createBuilding("house", new Position(3, 3));
        Building farm = entityFactory.createBuilding("farm", new Position(4, 3));
        
        // Добавляем в игру
        gameManager.getUnitManager().addUnit(warrior);
        gameManager.getUnitManager().addUnit(archer);
        
        System.out.println("Игра инициализирована");
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Демонстрация паттерна Command
        demonstrateCommands();
        
        // Демонстрация паттерна Strategy
        demonstrateCombatStrategies();
        
        // Демонстрация паттерна Decorator
        demonstrateDecorators();
        
        // Демонстрация паттерна Observer
        demonstrateEvents();
        
        displayGameState();
    }
    
    private void demonstrateCommands() {
        System.out.println("Демонстрация паттерна Command...");
        
        Unit warrior = gameManager.getUnitManager().getUnits().get(0);
        Position newPosition = new Position(5, 5);
        
        // Выполняем команду
        MoveUnitCommand moveCommand = new MoveUnitCommand(warrior, newPosition);
        commandManager.executeCommand(moveCommand);
        
        // Отменяем команду
        commandManager.undo();
        
        // Повторяем команду
        commandManager.redo();
    }
    
    private void demonstrateCombatStrategies() {
        System.out.println("Демонстрация паттерна Strategy...");
        
        Unit warrior = gameManager.getUnitManager().getUnits().get(0);
        Unit archer = gameManager.getUnitManager().getUnits().get(1);
        
        // Тестируем разные стратегии
        combatSystem.setStrategy("melee");
        CombatResult result1 = combatSystem.executeCombat(warrior, archer);
        System.out.println(result1.getMessage());
        
        combatSystem.setStrategy("ranged");
        CombatResult result2 = combatSystem.executeCombat(archer, warrior);
        System.out.println(result2.getMessage());
    }
    
    private void demonstrateDecorators() {
        System.out.println("Демонстрация паттерна Decorator...");
        
        Unit warrior = gameManager.getUnitManager().getUnits().get(0);
        System.out.println("Базовый воин: " + warrior.getName() + 
                         " (Защита: " + warrior.getDefense() + ")");
        
        // Применяем декораторы
        Unit armoredWarrior = new ArmoredUnit(warrior, 5);
        System.out.println("Бронированный воин: " + armoredWarrior.getName() + 
                         " (Защита: " + armoredWarrior.getDefense() + ")");
        
        Unit enchantedWarrior = new EnchantedUnit(armoredWarrior, 3);
        System.out.println("Зачарованный воин: " + enchantedWarrior.getName() + 
                         " (Атака: " + enchantedWarrior.getAttackPower() + ")");
    }
    
    private void demonstrateEvents() {
        System.out.println("Демонстрация паттерна Observer...");
        
        // Публикуем события
        eventManager.publishEvent(new ResourceChangeEvent("Золото", 100, 150));
        eventManager.publishEvent(new UnitMoveEvent(
            gameManager.getUnitManager().getUnits().get(0),
            new Position(1, 1), new Position(2, 2)
        ));
        
        System.out.println("История событий:");
        List<GameEvent> events = eventManager.getEventHistory();
        events.stream()
            .limit(5)
            .forEach(event -> System.out.println("  " + event.getType() + " - " + event.getTimestamp()));
    }
    
    private void displayGameState() {
        System.out.println("\n=== Состояние игры ===");
        
        System.out.println("Состояние игры: " + gameManager.getGameState());
        System.out.println("Юниты: " + gameManager.getUnitManager().getUnits().size());
        System.out.println("Здания: " + gameManager.getBuildingManager().getBuildings().size());
        System.out.println("Команды в истории: " + commandManager.getCommandHistory().size());
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

// Вспомогательные классы для демонстрации Observer
class GameLogger implements GameObserver {
    @Override
    public void onGameEvent(GameEvent event) {
        System.out.println("[ЛОГ] " + event.getType() + " - " + event.getTimestamp());
    }
}

class GameStatistics implements GameObserver {
    private Map<String, Integer> eventCounts = new HashMap<>();
    
    @Override
    public void onGameEvent(GameEvent event) {
        String type = event.getType();
        eventCounts.put(type, eventCounts.getOrDefault(type, 0) + 1);
        
        if (eventCounts.get(type) % 5 == 0) {
            System.out.println("[СТАТИСТИКА] Событий типа " + type + ": " + eventCounts.get(type));
        }
    }
}

class CombatResult {
    private boolean success;
    private int damage;
    private String message;
    
    public CombatResult(boolean success, int damage, String message) {
        this.success = success;
        this.damage = damage;
        this.message = message;
    }
    
    public boolean isSuccess() { return success; }
    public int getDamage() { return damage; }
    public String getMessage() { return message; }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичные паттерны проектирования:

### 1. **Гонки на выживание**
- Singleton: GameEngine, RaceManager
- Factory: VehicleFactory, TrackFactory
- Strategy: DrivingStrategy, RepairStrategy
- Observer: RaceEvents, VehicleStatus

### 2. **Космическая колонизация**
- Singleton: SpaceStation, ColonyManager
- Factory: SpaceshipFactory, ModuleFactory
- Strategy: ColonizationStrategy, ResearchStrategy
- Observer: SpaceEvents, ColonyStatus

### 3. **Подземелье и драконы**
- Singleton: DungeonMaster, PartyManager
- Factory: MonsterFactory, ItemFactory
- Strategy: CombatStrategy, ExplorationStrategy
- Observer: AdventureEvents, CharacterStatus

### 4. **Город-государство**
- Singleton: CityManager, EconomyManager
- Factory: BuildingFactory, CitizenFactory
- Strategy: DevelopmentStrategy, TradeStrategy
- Observer: CityEvents, PopulationStatus

### 5. **Пиратская стратегия**
- Singleton: PirateFleet, TreasureManager
- Factory: ShipFactory, CrewFactory
- Strategy: NavigationStrategy, CombatStrategy
- Observer: SeaEvents, FleetStatus

### 6. **Фермерское хозяйство**
- Singleton: FarmManager, WeatherManager
- Factory: CropFactory, AnimalFactory
- Strategy: FarmingStrategy, MarketStrategy
- Observer: FarmEvents, CropStatus

### 7. **Киберпанк-тактика**
- Singleton: NetworkManager, HackManager
- Factory: ProgramFactory, ImplantFactory
- Strategy: HackingStrategy, CombatStrategy
- Observer: CyberEvents, SystemStatus

### 8. **Средневековая осада**
- Singleton: SiegeManager, CastleManager
- Factory: SiegeEngineFactory, DefenderFactory
- Strategy: AttackStrategy, DefenseStrategy
- Observer: SiegeEvents, CastleStatus

### 9. **Зомби-выживание**
- Singleton: SurvivalManager, ZombieManager
- Factory: WeaponFactory, ShelterFactory
- Strategy: SurvivalStrategy, CombatStrategy
- Observer: SurvivalEvents, PlayerStatus

### 10. **Фэнтези-война**
- Singleton: MagicManager, ArmyManager
- Factory: SpellFactory, ArtifactFactory
- Strategy: MagicStrategy, BattleStrategy
- Observer: MagicEvents, ArmyStatus

## Требования к реализации

### Обязательные требования:
1. **Реализовать Singleton** для основных менеджеров
2. **Создать Factory** для создания игровых объектов
3. **Применить Strategy** для различных алгоритмов
4. **Использовать Observer** для системы событий
5. **Демонстрировать работу** всех паттернов
6. **Создать дополнительные паттерны** (Command, Decorator)

### Дополнительные требования:
1. **Реализовать Command** для системы команд
2. **Добавить Decorator** для улучшения объектов
3. **Создать Template Method** для алгоритмов
4. **Применить Adapter** для совместимости

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Singleton** | 3 | Реализация одиночки для менеджеров |
| **Factory** | 3 | Создание объектов через фабрики |
| **Strategy** | 3 | Различные алгоритмы поведения |
| **Observer** | 3 | Система событий и уведомлений |
| **Дополнительные паттерны** | 3 | Command, Decorator или другие |
| **Демонстрация** | 2 | Работающий пример всех паттернов |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужен паттерн Singleton?
2. Как Factory упрощает создание объектов?
3. Когда использовать Strategy?
4. Как Observer обеспечивает слабую связанность?
5. Как Command поддерживает отмену операций?
6. Зачем нужен Decorator?
7. Как паттерны улучшают архитектуру игры?

## Заключение

В данной лабораторной работе вы изучили основные паттерны проектирования в Java на примере создания игровых систем. Вы научились:

- Применять Singleton для глобальных менеджеров
- Использовать Factory для создания объектов
- Реализовывать Strategy для алгоритмов
- Создавать Observer для системы событий
- Применять дополнительные паттерны

Полученные знания позволят вам создавать гибкий, расширяемый и поддерживаемый код.
