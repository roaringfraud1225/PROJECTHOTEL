# Лабораторная работа 10: Архитектура приложения

## Цель работы
Изучить принципы создания архитектуры приложения в Java на примере разработки игровых систем. Научиться применять MVC паттерн, разделять ответственность между компонентами и создавать модульную архитектуру.

## Теоретические основы

### Архитектура приложения
- **Разделение ответственности** - каждый компонент имеет свою роль
- **Модульность** - независимые части системы
- **Слабая связанность** - минимизация зависимостей
- **Высокая когезия** - логическое объединение функциональности

### Паттерны архитектуры
- **MVC (Model-View-Controller)** - разделение логики, представления и управления
- **Слоистая архитектура** - разделение на уровни абстракции
- **Микросервисная архитектура** - независимые сервисы
- **Event-Driven Architecture** - архитектура на основе событий

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Архитектура приложения

#### 1. Модель (Model) - `GameModel`
```java
public class GameModel {
    private GameState gameState;
    private Player player;
    private GameWorld gameWorld;
    private GameRules gameRules;
    private List<GameEvent> pendingEvents;
    
    public GameModel() {
        this.gameState = GameState.MENU;
        this.player = new Player("Игрок", PlayerType.HUMAN);
        this.gameWorld = new GameWorld();
        this.gameRules = new GameRules();
        this.pendingEvents = new ArrayList<>();
    }
    
    public void initializeNewGame() {
        gameState = GameState.PLAYING;
        gameWorld.initialize();
        player.initialize();
        pendingEvents.clear();
        System.out.println("Новая игра инициализирована");
    }
    
    public void updateGameState() {
        if (gameState == GameState.PLAYING) {
            gameWorld.update();
            processPendingEvents();
            checkGameConditions();
        }
    }
    
    private void processPendingEvents() {
        Iterator<GameEvent> iterator = pendingEvents.iterator();
        while (iterator.hasNext()) {
            GameEvent event = iterator.next();
            if (event.canExecute()) {
                event.execute();
                iterator.remove();
            }
        }
    }
    
    private void checkGameConditions() {
        if (gameWorld.isGameOver()) {
            gameState = GameState.GAME_OVER;
        } else if (gameWorld.isVictory()) {
            gameState = GameState.VICTORY;
        }
    }
    
    public void addEvent(GameEvent event) {
        pendingEvents.add(event);
    }
    
    public GameState getGameState() { return gameState; }
    public Player getPlayer() { return player; }
    public GameWorld getGameWorld() { return gameWorld; }
    public GameRules getGameRules() { return gameRules; }
    public List<GameEvent> getPendingEvents() { return pendingEvents; }
    
    public enum GameState {
        MENU, PLAYING, PAUSED, GAME_OVER, VICTORY
    }
}
```

#### 2. Представление (View) - `GameView`
```java
public interface GameView {
    void render(GameModel model);
    void showMessage(String message);
    void updateDisplay();
}

public class ConsoleGameView implements GameView {
    private GameDisplay display;
    private MessageLogger messageLogger;
    
    public ConsoleGameView() {
        this.display = new GameDisplay();
        this.messageLogger = new MessageLogger();
    }
    
    @Override
    public void render(GameModel model) {
        display.clear();
        
        // Отображаем заголовок
        display.printHeader("=== КОРОЛЕВСТВО ===");
        
        // Отображаем состояние игры
        display.printGameState(model.getGameState());
        
        // Отображаем игровой мир
        display.printGameWorld(model.getGameWorld());
        
        // Отображаем информацию об игроке
        display.printPlayerInfo(model.getPlayer());
        
        // Отображаем сообщения
        display.printMessages(messageLogger.getRecentMessages());
        
        // Отображаем меню действий
        display.printActionMenu();
    }
    
    @Override
    public void showMessage(String message) {
        messageLogger.addMessage(message);
        System.out.println("[СООБЩЕНИЕ] " + message);
    }
    
    @Override
    public void updateDisplay() {
        // Обновление консольного дисплея
        System.out.print("\033[H\033[2J"); // Очистка экрана (Unix/Linux)
        System.out.flush();
    }
}

public class GameDisplay {
    public void clear() {
        // Очистка дисплея
    }
    
    public void printHeader(String title) {
        System.out.println(title);
        System.out.println("=".repeat(title.length()));
    }
    
    public void printGameState(GameModel.GameState state) {
        System.out.println("Состояние игры: " + state);
        System.out.println();
    }
    
    public void printGameWorld(GameWorld world) {
        System.out.println("--- ИГРОВОЙ МИР ---");
        world.renderToConsole();
        System.out.println();
    }
    
    public void printPlayerInfo(Player player) {
        System.out.println("--- ИГРОК ---");
        System.out.println("Имя: " + player.getName());
        System.out.println("Ресурсы: " + player.getResources());
        System.out.println("Очки: " + player.getScore());
        System.out.println();
    }
    
    public void printMessages(List<String> messages) {
        if (!messages.isEmpty()) {
            System.out.println("--- СООБЩЕНИЯ ---");
            messages.forEach(msg -> System.out.println("• " + msg));
            System.out.println();
        }
    }
    
    public void printActionMenu() {
        System.out.println("--- ДЕЙСТВИЯ ---");
        System.out.println("1. Переместить юнита");
        System.out.println("2. Построить здание");
        System.out.println("3. Атаковать врага");
        System.out.println("4. Управлять ресурсами");
        System.out.println("5. Следующий ход");
        System.out.println("0. Выход");
        System.out.print("Выберите действие: ");
    }
}

public class MessageLogger {
    private List<String> messages;
    private int maxMessages;
    
    public MessageLogger() {
        this.messages = new ArrayList<>();
        this.maxMessages = 10;
    }
    
    public void addMessage(String message) {
        messages.add(message);
        if (messages.size() > maxMessages) {
            messages.remove(0);
        }
    }
    
    public List<String> getRecentMessages() {
        return new ArrayList<>(messages);
    }
    
    public void clearMessages() {
        messages.clear();
    }
}
```

#### 3. Контроллер (Controller) - `GameController`
```java
public class GameController {
    private GameModel model;
    private GameView view;
    private InputHandler inputHandler;
    private ActionExecutor actionExecutor;
    private boolean isRunning;
    
    public GameController(GameModel model, GameView view) {
        this.model = model;
        this.view = view;
        this.inputHandler = new InputHandler();
        this.actionExecutor = new ActionExecutor(model);
        this.isRunning = false;
    }
    
    public void start() {
        isRunning = true;
        view.showMessage("Игра запущена");
        
        while (isRunning) {
            // Обновляем модель
            model.updateGameState();
            
            // Отображаем текущее состояние
            view.render(model);
            
            // Обрабатываем пользовательский ввод
            handleUserInput();
            
            // Проверяем условия завершения
            if (model.getGameState() == GameModel.GameState.GAME_OVER ||
                model.getGameState() == GameModel.GameState.VICTORY) {
                handleGameEnd();
                break;
            }
            
            // Небольшая задержка для стабильности
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
    
    private void handleUserInput() {
        String input = inputHandler.getUserInput();
        if (input != null && !input.trim().isEmpty()) {
            try {
                int choice = Integer.parseInt(input.trim());
                executeUserChoice(choice);
            } catch (NumberFormatException e) {
                view.showMessage("Неверный ввод. Введите число.");
            }
        }
    }
    
    private void executeUserChoice(int choice) {
        switch (choice) {
            case 1:
                handleUnitMovement();
                break;
            case 2:
                handleBuildingConstruction();
                break;
            case 3:
                handleCombat();
                break;
            case 4:
                handleResourceManagement();
                break;
            case 5:
                handleNextTurn();
                break;
            case 0:
                isRunning = false;
                view.showMessage("Игра завершена");
                break;
            default:
                view.showMessage("Неверный выбор. Попробуйте снова.");
        }
    }
    
    private void handleUnitMovement() {
        view.showMessage("Перемещение юнита...");
        // Логика перемещения юнита
        actionExecutor.executeUnitMovement();
    }
    
    private void handleBuildingConstruction() {
        view.showMessage("Строительство здания...");
        // Логика строительства
        actionExecutor.executeBuildingConstruction();
    }
    
    private void handleCombat() {
        view.showMessage("Боевые действия...");
        // Логика боя
        actionExecutor.executeCombat();
    }
    
    private void handleResourceManagement() {
        view.showMessage("Управление ресурсами...");
        // Логика управления ресурсами
        actionExecutor.executeResourceManagement();
    }
    
    private void handleNextTurn() {
        view.showMessage("Следующий ход...");
        // Логика следующего хода
        actionExecutor.executeNextTurn();
    }
    
    private void handleGameEnd() {
        if (model.getGameState() == GameModel.GameState.VICTORY) {
            view.showMessage("Поздравляем! Вы победили!");
        } else {
            view.showMessage("Игра окончена. Попробуйте снова!");
        }
    }
    
    public void stop() {
        isRunning = false;
    }
}
```

#### 4. Обработчик ввода - `InputHandler`
```java
public class InputHandler {
    private Scanner scanner;
    private List<String> inputHistory;
    
    public InputHandler() {
        this.scanner = new Scanner(System.in);
        this.inputHistory = new ArrayList<>();
    }
    
    public String getUserInput() {
        String input = scanner.nextLine();
        if (input != null && !input.trim().isEmpty()) {
            inputHistory.add(input.trim());
        }
        return input;
    }
    
    public String getUserInput(String prompt) {
        System.out.print(prompt);
        return getUserInput();
    }
    
    public int getIntInput(String prompt) {
        while (true) {
            try {
                String input = getUserInput(prompt);
                return Integer.parseInt(input.trim());
            } catch (NumberFormatException e) {
                System.out.println("Пожалуйста, введите число.");
            }
        }
    }
    
    public List<String> getInputHistory() {
        return new ArrayList<>(inputHistory);
    }
    
    public void clearHistory() {
        inputHistory.clear();
    }
    
    public void close() {
        scanner.close();
    }
}
```

#### 5. Исполнитель действий - `ActionExecutor`
```java
public class ActionExecutor {
    private GameModel model;
    private GameRules gameRules;
    
    public ActionExecutor(GameModel model) {
        this.model = model;
        this.gameRules = model.getGameRules();
    }
    
    public void executeUnitMovement() {
        if (!gameRules.canMoveUnit()) {
            model.addEvent(new GameMessageEvent("Перемещение юнита невозможно"));
            return;
        }
        
        // Логика перемещения юнита
        Unit selectedUnit = selectUnit();
        if (selectedUnit != null) {
            Position targetPosition = selectTargetPosition();
            if (targetPosition != null) {
                MoveUnitAction action = new MoveUnitAction(selectedUnit, targetPosition);
                if (action.canExecute()) {
                    action.execute();
                    model.addEvent(new UnitMovedEvent(selectedUnit, targetPosition));
                }
            }
        }
    }
    
    public void executeBuildingConstruction() {
        if (!gameRules.canBuild()) {
            model.addEvent(new GameMessageEvent("Строительство невозможно"));
            return;
        }
        
        // Логика строительства
        BuildingType buildingType = selectBuildingType();
        if (buildingType != null) {
            Position buildPosition = selectBuildPosition();
            if (buildPosition != null) {
                BuildAction action = new BuildAction(buildingType, buildPosition);
                if (action.canExecute()) {
                    action.execute();
                    model.addEvent(new BuildingConstructedEvent(buildingType, buildPosition));
                }
            }
        }
    }
    
    public void executeCombat() {
        if (!gameRules.canFight()) {
            model.addEvent(new GameMessageEvent("Боевые действия невозможны"));
            return;
        }
        
        // Логика боя
        Unit attacker = selectAttackingUnit();
        Unit target = selectTargetUnit();
        if (attacker != null && target != null) {
            CombatAction action = new CombatAction(attacker, target);
            if (action.canExecute()) {
                action.execute();
                model.addEvent(new CombatEvent(attacker, target));
            }
        }
    }
    
    public void executeResourceManagement() {
        // Логика управления ресурсами
        ResourceType resourceType = selectResourceType();
        if (resourceType != null) {
            int amount = selectResourceAmount();
            if (amount > 0) {
                ResourceAction action = new ResourceAction(resourceType, amount);
                if (action.canExecute()) {
                    action.execute();
                    model.addEvent(new ResourceChangedEvent(resourceType, amount));
                }
            }
        }
    }
    
    public void executeNextTurn() {
        // Логика следующего хода
        model.addEvent(new TurnEndedEvent());
        model.getGameWorld().advanceTurn();
    }
    
    // Вспомогательные методы для выбора
    private Unit selectUnit() {
        // Упрощенная реализация - возвращаем первый доступный юнит
        return model.getGameWorld().getUnits().stream()
                .findFirst()
                .orElse(null);
    }
    
    private Position selectTargetPosition() {
        // Упрощенная реализация - случайная позиция
        return new Position(
            (int)(Math.random() * 10),
            (int)(Math.random() * 10)
        );
    }
    
    private BuildingType selectBuildingType() {
        // Упрощенная реализация - случайный тип здания
        BuildingType[] types = BuildingType.values();
        return types[(int)(Math.random() * types.length)];
    }
    
    private Position selectBuildPosition() {
        return selectTargetPosition();
    }
    
    private Unit selectAttackingUnit() {
        return selectUnit();
    }
    
    private Unit selectTargetUnit() {
        return selectUnit();
    }
    
    private ResourceType selectResourceType() {
        // Упрощенная реализация - случайный тип ресурса
        ResourceType[] types = ResourceType.values();
        return types[(int)(Math.random() * types.length)];
    }
    
    private int selectResourceAmount() {
        return (int)(Math.random() * 100) + 1;
    }
}
```

#### 6. Игровые правила - `GameRules`
```java
public class GameRules {
    private Map<String, Boolean> ruleFlags;
    private List<Rule> activeRules;
    
    public GameRules() {
        this.ruleFlags = new HashMap<>();
        this.activeRules = new ArrayList<>();
        initializeRules();
    }
    
    private void initializeRules() {
        // Базовые правила
        ruleFlags.put("canMoveUnit", true);
        ruleFlags.put("canBuild", true);
        ruleFlags.put("canFight", true);
        ruleFlags.put("canTrade", true);
        
        // Добавляем активные правила
        activeRules.add(new MovementRule());
        activeRules.add(new BuildingRule());
        activeRules.add(new CombatRule());
        activeRules.add(new ResourceRule());
    }
    
    public boolean canMoveUnit() {
        return ruleFlags.getOrDefault("canMoveUnit", false) &&
               activeRules.stream().allMatch(rule -> rule.allowsMovement());
    }
    
    public boolean canBuild() {
        return ruleFlags.getOrDefault("canBuild", false) &&
               activeRules.stream().allMatch(rule -> rule.allowsBuilding());
    }
    
    public boolean canFight() {
        return ruleFlags.getOrDefault("canFight", false) &&
               activeRules.stream().allMatch(rule -> rule.allowsCombat());
    }
    
    public boolean canTrade() {
        return ruleFlags.getOrDefault("canTrade", false) &&
               activeRules.stream().allMatch(rule -> rule.allowsTrading());
    }
    
    public void setRule(String ruleName, boolean enabled) {
        ruleFlags.put(ruleName, enabled);
    }
    
    public void addRule(Rule rule) {
        activeRules.add(rule);
    }
    
    public void removeRule(Rule rule) {
        activeRules.remove(rule);
    }
    
    public List<Rule> getActiveRules() { return activeRules; }
}

public interface Rule {
    boolean allowsMovement();
    boolean allowsBuilding();
    boolean allowsCombat();
    boolean allowsTrading();
    String getRuleName();
}

public class MovementRule implements Rule {
    @Override
    public boolean allowsMovement() {
        return true; // Базовое правило - движение разрешено
    }
    
    @Override
    public boolean allowsBuilding() {
        return true;
    }
    
    @Override
    public boolean allowsCombat() {
        return true;
    }
    
    @Override
    public boolean allowsTrading() {
        return true;
    }
    
    @Override
    public String getRuleName() {
        return "Базовое правило движения";
    }
}

public class BuildingRule implements Rule {
    @Override
    public boolean allowsMovement() {
        return true;
    }
    
    @Override
    public boolean allowsBuilding() {
        return true; // Базовое правило - строительство разрешено
    }
    
    @Override
    public boolean allowsCombat() {
        return true;
    }
    
    @Override
    public boolean allowsTrading() {
        return true;
    }
    
    @Override
    public String getRuleName() {
        return "Базовое правило строительства";
    }
}

public class CombatRule implements Rule {
    @Override
    public boolean allowsMovement() {
        return true;
    }
    
    @Override
    public boolean allowsBuilding() {
        return true;
    }
    
    @Override
    public boolean allowsCombat() {
        return true; // Базовое правило - бой разрешен
    }
    
    @Override
    public boolean allowsTrading() {
        return true;
    }
    
    @Override
    public String getRuleName() {
        return "Базовое правило боя";
    }
}

public class ResourceRule implements Rule {
    @Override
    public boolean allowsMovement() {
        return true;
    }
    
    @Override
    public boolean allowsBuilding() {
        return true;
    }
    
    @Override
    public boolean allowsCombat() {
        return true;
    }
    
    @Override
    public boolean allowsTrading() {
        return true; // Базовое правило - торговля разрешена
    }
    
    @Override
    public String getRuleName() {
        return "Базовое правило ресурсов";
    }
}
```

#### 7. Главный класс с демонстрацией архитектуры
```java
public class Game {
    private GameModel model;
    private GameView view;
    private GameController controller;
    
    public Game() {
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация архитектуры игры...");
        
        // Создаем компоненты архитектуры
        this.model = new GameModel();
        this.view = new ConsoleGameView();
        this.controller = new GameController(model, view);
        
        // Инициализируем игру
        model.initializeNewGame();
        
        System.out.println("Архитектура игры инициализирована");
    }
    
    public void start() {
        System.out.println("Запуск игры...");
        controller.start();
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        game.start();
    }
}

// Вспомогательные классы для демонстрации
class GameMessageEvent extends GameEvent {
    public GameMessageEvent(String message) {
        super("GAME_MESSAGE");
        addData("message", message);
    }
}

class UnitMovedEvent extends GameEvent {
    public UnitMovedEvent(Unit unit, Position position) {
        super("UNIT_MOVED");
        addData("unit", unit);
        addData("position", position);
    }
}

class BuildingConstructedEvent extends GameEvent {
    public BuildingConstructedEvent(BuildingType type, Position position) {
        super("BUILDING_CONSTRUCTED");
        addData("type", type);
        addData("position", position);
    }
}

class CombatEvent extends GameEvent {
    public CombatEvent(Unit attacker, Unit target) {
        super("COMBAT");
        addData("attacker", attacker);
        addData("target", target);
    }
}

class ResourceChangedEvent extends GameEvent {
    public ResourceChangedEvent(ResourceType type, int amount) {
        super("RESOURCE_CHANGED");
        addData("type", type);
        addData("amount", amount);
    }
}

class TurnEndedEvent extends GameEvent {
    public TurnEndedEvent() {
        super("TURN_ENDED");
    }
}

// Перечисления для типов
enum BuildingType {
    HOUSE, FARM, BARRACKS, MINE
}

enum ResourceType {
    GOLD, WOOD, STONE, FOOD
}

// Базовые классы для действий
abstract class GameAction {
    protected boolean executed;
    
    public abstract boolean canExecute();
    public abstract void execute();
    
    public boolean isExecuted() { return executed; }
}

class MoveUnitAction extends GameAction {
    private Unit unit;
    private Position targetPosition;
    
    public MoveUnitAction(Unit unit, Position targetPosition) {
        this.unit = unit;
        this.targetPosition = targetPosition;
    }
    
    @Override
    public boolean canExecute() {
        return unit != null && targetPosition != null && !executed;
    }
    
    @Override
    public void execute() {
        if (canExecute()) {
            unit.setPosition(targetPosition);
            executed = true;
        }
    }
}

class BuildAction extends GameAction {
    private BuildingType buildingType;
    private Position position;
    
    public BuildAction(BuildingType buildingType, Position position) {
        this.buildingType = buildingType;
        this.position = position;
    }
    
    @Override
    public boolean canExecute() {
        return buildingType != null && position != null && !executed;
    }
    
    @Override
    public void execute() {
        if (canExecute()) {
            // Логика строительства
            executed = true;
        }
    }
}

class CombatAction extends GameAction {
    private Unit attacker;
    private Unit target;
    
    public CombatAction(Unit attacker, Unit target) {
        this.attacker = attacker;
        this.target = target;
    }
    
    @Override
    public boolean canExecute() {
        return attacker != null && target != null && !executed;
    }
    
    @Override
    public void execute() {
        if (canExecute()) {
            // Логика боя
            executed = true;
        }
    }
}

class ResourceAction extends GameAction {
    private ResourceType resourceType;
    private int amount;
    
    public ResourceAction(ResourceType resourceType, int amount) {
        this.resourceType = resourceType;
        this.amount = amount;
    }
    
    @Override
    public boolean canExecute() {
        return resourceType != null && amount > 0 && !executed;
    }
    
    @Override
    public void execute() {
        if (canExecute()) {
            // Логика изменения ресурсов
            executed = true;
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную архитектуру:

### 1. **Гонки на выживание**
- Model: RaceModel, VehicleModel, TrackModel
- View: ConsoleView, StatisticsView
- Controller: RaceController, InputController
- Правила: RacingRules, PhysicsRules

### 2. **Космическая колонизация**
- Model: ColonyModel, SpaceModel, ResearchModel
- View: SpaceView, ColonyView
- Controller: ColonyController, ResearchController
- Правила: SpaceRules, ColonizationRules

### 3. **Подземелье и драконы**
- Model: DungeonModel, CharacterModel, QuestModel
- View: DungeonView, CharacterView
- Controller: AdventureController, CombatController
- Правила: AdventureRules, CombatRules

### 4. **Город-государство**
- Model: CityModel, EconomyModel, PopulationModel
- View: CityView, EconomyView
- Controller: CityController, EconomyController
- Правила: CityRules, EconomicRules

### 5. **Пиратская стратегия**
- Model: FleetModel, TreasureModel, SeaModel
- View: SeaView, FleetView
- Controller: FleetController, NavigationController
- Правила: SeaRules, PirateRules

### 6. **Фермерское хозяйство**
- Model: FarmModel, CropModel, WeatherModel
- View: FarmView, WeatherView
- Controller: FarmController, WeatherController
- Правила: FarmingRules, WeatherRules

### 7. **Киберпанк-тактика**
- Model: NetworkModel, HackModel, ImplantModel
- View: CyberView, NetworkView
- Controller: HackController, NetworkController
- Правила: CyberRules, HackingRules

### 8. **Средневековая осада**
- Model: SiegeModel, CastleModel, ArmyModel
- View: SiegeView, CastleView
- Controller: SiegeController, ArmyController
- Правила: SiegeRules, MilitaryRules

### 9. **Зомби-выживание**
- Model: SurvivalModel, ZombieModel, ShelterModel
- View: SurvivalView, ZombieView
- Controller: SurvivalController, CombatController
- Правила: SurvivalRules, ZombieRules

### 10. **Фэнтези-война**
- Model: MagicModel, ArmyModel, ArtifactModel
- View: MagicView, BattleView
- Controller: MagicController, BattleController
- Правила: MagicRules, BattleRules

## Требования к реализации

### Обязательные требования:
1. **Реализовать MVC архитектуру** с четким разделением ответственности
2. **Создать модульную структуру** с независимыми компонентами
3. **Реализовать систему правил** для управления игровой логики
4. **Создать обработчик ввода** для пользовательского взаимодействия
5. **Демонстрировать работу** всех компонентов архитектуры
6. **Создать исполнитель действий** для игровых операций

### Дополнительные требования:
1. **Реализовать слоистую архитектуру** с уровнями абстракции
2. **Добавить систему событий** для компонентов
3. **Создать конфигурационные файлы** для настроек
4. **Реализовать логирование** действий и событий

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **MVC архитектура** | 4 | Четкое разделение Model-View-Controller |
| **Модульность** | 3 | Независимые компоненты системы |
| **Система правил** | 3 | Управление игровой логикой |
| **Обработка ввода** | 3 | Пользовательское взаимодействие |
| **Исполнитель действий** | 2 | Выполнение игровых операций |
| **Демонстрация** | 2 | Работающий пример архитектуры |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужна MVC архитектура?
2. Как разделить ответственность между компонентами?
3. Что такое модульность в программировании?
4. Как система правил управляет игровой логикой?
5. Как обрабатывать пользовательский ввод?
6. Зачем нужен исполнитель действий?
7. Как создать расширяемую архитектуру?

## Заключение

В данной лабораторной работе вы изучили принципы создания архитектуры приложения в Java на примере создания игровых систем. Вы научились:

- Применять MVC паттерн для разделения ответственности
- Создавать модульную и расширяемую архитектуру
- Реализовывать систему правил для игровой логики
- Обрабатывать пользовательский ввод
- Создавать исполнителя действий

Полученные знания позволят вам создавать хорошо структурированные и поддерживаемые приложения.
