---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Многопоточность
## Лекция 11: Параллельное выполнение

**Преподаватель:** [Ваше имя]  
**Группа:** 203  
**Семестр:** Осенний 2024

---

# План лекции

1. **Что такое архитектура приложения?**
2. **MVC паттерн**
3. **Разделение ответственности**
4. **Модульность и слои**
5. **Dependency Injection**
6. **Практический пример: Архитектура игры**

---

# Что такое архитектура приложения?

## Определение:
**Архитектура приложения** — это структура и организация компонентов программного обеспечения, определяющая их взаимодействие и отношения.

## Цели:
- **Читаемость** и понимание кода
- **Поддерживаемость** и расширяемость
- **Тестируемость** компонентов
- **Переиспользование** кода
- **Масштабируемость** системы

---

# Принципы хорошей архитектуры

## SOLID принципы:
- **S** — Single Responsibility (единственная ответственность)
- **O** — Open/Closed (открыт для расширения, закрыт для изменения)
- **L** — Liskov Substitution (подстановка Лисков)
- **I** — Interface Segregation (разделение интерфейсов)
- **D** — Dependency Inversion (инверсия зависимостей)

## Дополнительные принципы:
- **DRY** — Don't Repeat Yourself
- **KISS** — Keep It Simple, Stupid
- **YAGNI** — You Aren't Gonna Need It

---

# MVC паттерн

## Что такое MVC?
**MVC** (Model-View-Controller) — архитектурный паттерн, разделяющий приложение на три основных компонента.

## Компоненты:
- **Model** — данные и бизнес-логика
- **View** — представление данных пользователю
- **Controller** — обработка пользовательского ввода

---

# Структура MVC

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│      View       │    │   Controller    │    │      Model      │
│                 │    │                 │    │                 │
│ - Отображение   │◄──►│ - Обработка     │◄──►│ - Данные        │
│ - UI элементы   │    │   ввода         │    │ - Бизнес-логика │
│ - Стили         │    │ - Обновление    │    │ - Состояние     │
│                 │    │   модели        │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Взаимодействие:
1. **Пользователь** взаимодействует с View
2. **View** передает действия в Controller
3. **Controller** обновляет Model
4. **Model** уведомляет View об изменениях
5. **View** обновляется

---

# Model (Модель)

## Ответственность:
- **Хранение данных** приложения
- **Бизнес-логика** и правила
- **Валидация** данных
- **Уведомление** об изменениях

```java
public class GameModel {
    private GameState gameState;
    private List<GameObserver> observers;
    
    public GameModel() {
        this.gameState = new GameState();
        this.observers = new ArrayList<>();
    }
    
    public void moveUnit(Unit unit, Position newPosition) {
        // Проверка возможности движения
        if (isValidMove(unit, newPosition)) {
            // Обновление состояния
            gameState.moveUnit(unit, newPosition);
            
            // Уведомление наблюдателей
            notifyObservers(new UnitMovedEvent(unit, newPosition));
        }
    }
    
    public void addObserver(GameObserver observer) {
        observers.add(observer);
    }
    
    private void notifyObservers(GameEvent event) {
        for (GameObserver observer : observers) {
            observer.onGameEvent(event);
        }
    }
}
```

---

# View (Представление)

## Ответственность:
- **Отображение данных** пользователю
- **UI элементы** и их стили
- **Обработка пользовательского ввода**
- **Обновление** при изменении данных

```java
public class GameView implements GameObserver {
    private Stage primaryStage;
    private GridPane gameBoard;
    private VBox infoPanel;
    private VBox actionPanel;
    
    public GameView() {
        initializeUI();
    }
    
    private void initializeUI() {
        primaryStage = new Stage();
        primaryStage.setTitle("Пошаговая стратегия");
        
        BorderPane root = new BorderPane();
        
        // Создание игрового поля
        gameBoard = createGameBoard();
        root.setCenter(gameBoard);
        
        // Создание информационной панели
        infoPanel = createInfoPanel();
        root.setLeft(infoPanel);
        
        // Создание панели действий
        actionPanel = createActionPanel();
        root.setRight(actionPanel);
        
        Scene scene = new Scene(root);
        primaryStage.setScene(scene);
    }
    
    @Override
    public void onGameEvent(GameEvent event) {
        if (event instanceof UnitMovedEvent) {
            updateUnitPosition((UnitMovedEvent) event);
        }
    }
    
    private void updateUnitPosition(UnitMovedEvent event) {
        // Обновление позиции юнита на поле
        Platform.runLater(() -> {
            // Обновление UI в потоке JavaFX
            refreshGameBoard();
        });
    }
}
```

---

# Controller (Контроллер)

## Ответственность:
- **Обработка пользовательского ввода**
- **Координация** между Model и View
- **Валидация** входных данных
- **Вызов** методов модели

```java
public class GameController {
    private GameModel model;
    private GameView view;
    
    public GameController(GameModel model, GameView view) {
        this.model = model;
        this.view = view;
        
        // Подписка на события модели
        model.addObserver(view);
        
        // Настройка обработчиков событий
        setupEventHandlers();
    }
    
    private void setupEventHandlers() {
        // Обработка кликов по игровому полю
        view.getGameBoard().setOnMouseClicked(e -> {
            Position clickedPosition = getPositionFromMouseEvent(e);
            handleCellClick(clickedPosition);
        });
        
        // Обработка кнопок действий
        view.getMoveButton().setOnAction(e -> handleMoveAction());
        view.getAttackButton().setOnAction(e -> handleAttackAction());
        view.getBuildButton().setOnAction(e -> handleBuildAction());
    }
    
    private void handleCellClick(Position position) {
        Unit selectedUnit = view.getSelectedUnit();
        if (selectedUnit != null) {
            // Попытка движения юнита
            model.moveUnit(selectedUnit, position);
        } else {
            // Выбор юнита на позиции
            Unit unitAtPosition = model.getUnitAt(position);
            if (unitAtPosition != null) {
                view.selectUnit(unitAtPosition);
            }
        }
    }
    
    private void handleMoveAction() {
        Unit selectedUnit = view.getSelectedUnit();
        Position targetPosition = view.getTargetPosition();
        
        if (selectedUnit != null && targetPosition != null) {
            model.moveUnit(selectedUnit, targetPosition);
        }
    }
}
```

---

# Разделение ответственности

## Принцип единственной ответственности:
Каждый класс должен иметь только одну причину для изменения.

```java
// ❌ Плохо: класс делает слишком много
public class GameManager {
    public void moveUnit() { /* ... */ }
    public void attackUnit() { /* ... */ }
    public void saveGame() { /* ... */ }
    public void loadGame() { /* ... */ }
    public void renderUI() { /* ... */ }
    public void handleInput() { /* ... */ }
}

// ✅ Хорошо: каждый класс имеет одну ответственность
public class UnitMovementService {
    public void moveUnit(Unit unit, Position newPosition) { /* ... */ }
}

public class CombatService {
    public void attackUnit(Unit attacker, Unit target) { /* ... */ }
}

public class SaveLoadService {
    public void saveGame(GameState state, String filename) { /* ... */ }
    public GameState loadGame(String filename) { /* ... */ }
}

public class UIRenderer {
    public void renderGameBoard(GameState state) { /* ... */ }
}

public class InputHandler {
    public void handleMouseClick(MouseEvent event) { /* ... */ }
}
```

---

# Модульность и слои

## Архитектура по слоям:
```
┌─────────────────────────────────────┐
│           Presentation Layer        │ ← UI, контроллеры
├─────────────────────────────────────┤
│           Business Layer            │ ← Бизнес-логика, сервисы
├─────────────────────────────────────┤
│           Data Layer                │ ← Доступ к данным, репозитории
└─────────────────────────────────────┘
```

## Преимущества:
- **Изоляция** изменений
- **Тестируемость** компонентов
- **Переиспользование** кода
- **Простота** понимания

---

# Пример слоистой архитектуры

```java
// Presentation Layer
public class GameUI {
    private GameController controller;
    
    public void showGameBoard(GameState state) {
        // Отображение игрового поля
    }
    
    public void showUnitInfo(Unit unit) {
        // Отображение информации о юните
    }
}

// Business Layer
public class GameService {
    private UnitRepository unitRepository;
    private CombatService combatService;
    
    public void moveUnit(int unitId, Position newPosition) {
        Unit unit = unitRepository.findById(unitId);
        if (unit != null && isValidMove(unit, newPosition)) {
            unit.setPosition(newPosition);
            unitRepository.save(unit);
        }
    }
    
    public void attackUnit(int attackerId, int targetId) {
        Unit attacker = unitRepository.findById(attackerId);
        Unit target = unitRepository.findById(targetId);
        
        if (attacker != null && target != null) {
            combatService.performAttack(attacker, target);
            unitRepository.save(attacker);
            unitRepository.save(target);
        }
    }
}

// Data Layer
public class UnitRepository {
    private List<Unit> units = new ArrayList<>();
    
    public Unit findById(int id) {
        return units.stream()
            .filter(unit -> unit.getId() == id)
            .findFirst()
            .orElse(null);
    }
    
    public void save(Unit unit) {
        // Сохранение юнита
    }
}
```

---

# Dependency Injection

## Что такое DI?
**Dependency Injection** — паттерн, при котором зависимости объекта передаются извне, а не создаются внутри объекта.

## Преимущества:
- **Слабая связанность** между компонентами
- **Легкое тестирование** с mock объектами
- **Гибкость** в конфигурации
- **Переиспользование** компонентов

---

# Реализация Dependency Injection

```java
// Интерфейсы для сервисов
public interface UnitService {
    void moveUnit(Unit unit, Position newPosition);
    void attackUnit(Unit attacker, Unit target);
}

public interface SaveLoadService {
    void saveGame(GameState state, String filename);
    GameState loadGame(String filename);
}

// Реализации сервисов
public class UnitServiceImpl implements UnitService {
    @Override
    public void moveUnit(Unit unit, Position newPosition) {
        // Реализация движения
    }
    
    @Override
    public void attackUnit(Unit attacker, Unit target) {
        // Реализация атаки
    }
}

public class SaveLoadServiceImpl implements SaveLoadService {
    @Override
    public void saveGame(GameState state, String filename) {
        // Реализация сохранения
    }
    
    @Override
    public GameState loadGame(String filename) {
        // Реализация загрузки
    }
}

// Контроллер с внедрением зависимостей
public class GameController {
    private final UnitService unitService;
    private final SaveLoadService saveLoadService;
    
    public GameController(UnitService unitService, SaveLoadService saveLoadService) {
        this.unitService = unitService;
        this.saveLoadService = saveLoadService;
    }
    
    public void handleMoveUnit(Unit unit, Position newPosition) {
        unitService.moveUnit(unit, newPosition);
    }
    
    public void handleSaveGame(String filename) {
        GameState currentState = getCurrentGameState();
        saveLoadService.saveGame(currentState, filename);
    }
}
```

---

# Конфигурация зависимостей

```java
public class DependencyContainer {
    private static DependencyContainer instance;
    private Map<Class<?>, Object> services;
    
    private DependencyContainer() {
        services = new HashMap<>();
        initializeServices();
    }
    
    public static DependencyContainer getInstance() {
        if (instance == null) {
            instance = new DependencyContainer();
        }
        return instance;
    }
    
    private void initializeServices() {
        // Регистрация сервисов
        services.put(UnitService.class, new UnitServiceImpl());
        services.put(SaveLoadService.class, new SaveLoadServiceImpl());
        services.put(CombatService.class, new CombatServiceImpl());
        services.put(AIService.class, new AIServiceImpl());
    }
    
    @SuppressWarnings("unchecked")
    public <T> T getService(Class<T> serviceClass) {
        return (T) services.get(serviceClass);
    }
}

// Использование
public class GameApplication {
    public static void main(String[] args) {
        DependencyContainer container = DependencyContainer.getInstance();
        
        UnitService unitService = container.getService(UnitService.class);
        SaveLoadService saveLoadService = container.getService(SaveLoadService.class);
        
        GameController controller = new GameController(unitService, saveLoadService);
        GameView view = new GameView();
        
        // Запуск приложения
        Game game = new Game(controller, view);
        game.start();
    }
}
```

---

# Практический пример: Архитектура игры

```java
// Главный класс приложения
public class Game {
    private GameModel model;
    private GameView view;
    private GameController controller;
    private GameService gameService;
    
    public Game() {
        initializeComponents();
        setupDependencies();
    }
    
    private void initializeComponents() {
        // Создание компонентов
        model = new GameModel();
        view = new GameView();
        controller = new GameController();
        gameService = new GameService();
    }
    
    private void setupDependencies() {
        // Настройка зависимостей
        controller.setModel(model);
        controller.setView(view);
        controller.setGameService(gameService);
        
        view.setController(controller);
        model.addObserver(view);
    }
    
    public void start() {
        // Запуск игры
        view.show();
        model.startNewGame();
    }
}

// Структура пакетов
/*
game/
├── model/           # Модели данных
│   ├── GameState.java
│   ├── Unit.java
│   └── Position.java
├── view/            # Представление
│   ├── GameView.java
│   ├── GameBoard.java
│   └── UnitInfoPanel.java
├── controller/      # Контроллеры
│   ├── GameController.java
│   └── UnitController.java
├── service/         # Бизнес-логика
│   ├── GameService.java
│   ├── UnitService.java
│   └── CombatService.java
├── repository/      # Доступ к данным
│   ├── UnitRepository.java
│   └── GameRepository.java
└── util/            # Утилиты
    ├── GameLogger.java
    └── ConfigManager.java
*/
```

---

# Тестирование архитектуры

```java
// Тест контроллера с mock объектами
public class GameControllerTest {
    private GameController controller;
    private GameModel mockModel;
    private GameView mockView;
    
    @Before
    public void setUp() {
        mockModel = mock(GameModel.class);
        mockView = mock(GameView.class);
        controller = new GameController(mockModel, mockView);
    }
    
    @Test
    public void testMoveUnit() {
        // Arrange
        Unit unit = new Unit("Test", 100, 10, 5, new Position(0, 0));
        Position newPosition = new Position(1, 1);
        
        when(mockModel.isValidMove(unit, newPosition)).thenReturn(true);
        
        // Act
        controller.handleMoveUnit(unit, newPosition);
        
        // Assert
        verify(mockModel).moveUnit(unit, newPosition);
    }
    
    @Test
    public void testInvalidMove() {
        // Arrange
        Unit unit = new Unit("Test", 100, 10, 5, new Position(0, 0));
        Position newPosition = new Position(1, 1);
        
        when(mockModel.isValidMove(unit, newPosition)).thenReturn(false);
        
        // Act
        controller.handleMoveUnit(unit, newPosition);
        
        // Assert
        verify(mockModel, never()).moveUnit(any(), any());
        verify(mockView).showError("Невозможно выполнить ход");
    }
}
```

---

# Лучшие практики архитектуры

## ✅ Что делать:
- **Разделять ответственности** между компонентами
- **Использовать интерфейсы** для слабой связанности
- **Внедрять зависимости** извне
- **Следовать принципам SOLID**
- **Тестировать** каждый слой отдельно

## ❌ Чего избегать:
- **Смешивать слои** архитектуры
- **Создавать сильные зависимости** между компонентами
- **Игнорировать** принципы SOLID
- **Забывать про** тестируемость
- **Создавать** слишком сложные структуры

---

# Домашнее задание

## Задача 1:
Реорганизовать существующий код по MVC архитектуре

## Задача 2:
Реализовать Dependency Injection для основных сервисов

## Задача 3:
Создать тесты для каждого слоя архитектуры

---

# Что дальше?

## На следующей лекции:
- **Многопоточность**
- **Threads и Runnable**
- **Синхронизация**
- **Concurrent collections**

## Подготовка:
- Изучить главу 19-20 из учебника
- Выполнить домашнее задание
- Подготовить вопросы по текущей теме

---

# Вопросы?

## Контакты:
- **Email:** [ваш.email@university.edu]
- **Telegram:** [@username]
- **Офис:** [номер кабинета]

## Следующая лекция: **Многопоточность**
