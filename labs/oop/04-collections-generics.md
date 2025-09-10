---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Лабораторная работа 4: Коллекции и Generics

## Цель работы
Изучить работу с коллекциями Java и generics на примере разработки игровых систем. Научиться использовать ArrayList, LinkedList, HashMap, HashSet, а также создавать собственные generic классы.

## Теоретические основы

### Коллекции Java
- **ArrayList** - динамический массив с быстрым доступом по индексу
- **LinkedList** - связанный список с быстрыми операциями вставки/удаления
- **HashMap** - хеш-таблица для пар ключ-значение
- **HashSet** - множество уникальных элементов
- **TreeMap/TreeSet** - отсортированные коллекции

### Generics
- **Type Safety** - проверка типов на этапе компиляции
- **Type Parameters** - параметры типа в угловых скобках
- **Wildcards** - `?`, `? extends T`, `? super T`
- **Bounded Types** - ограничения на типы

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система коллекций

#### 1. Класс `GameWorld` с коллекциями
```java
public class GameWorld {
    private Map<Position, GameEntity> entityMap;
    private List<Unit> units;
    private List<Building> buildings;
    private Map<String, Resource> resources;
    private Set<Position> occupiedPositions;
    private Queue<GameEvent> eventQueue;
    
    public GameWorld() {
        this.entityMap = new HashMap<>();
        this.units = new ArrayList<>();
        this.buildings = new LinkedList<>();
        this.resources = new TreeMap<>();
        this.occupiedPositions = new HashSet<>();
        this.eventQueue = new LinkedList<>();
    }
    
    // Добавление сущностей
    public void addEntity(GameEntity entity) {
        Position pos = entity.getPosition();
        entityMap.put(pos, entity);
        occupiedPositions.add(pos);
        
        if (entity instanceof Unit) {
            units.add((Unit) entity);
        } else if (entity instanceof Building) {
            buildings.add((Building) entity);
        }
    }
    
    // Удаление сущностей
    public void removeEntity(Position position) {
        GameEntity entity = entityMap.remove(position);
        if (entity != null) {
            occupiedPositions.remove(position);
            units.remove(entity);
            buildings.remove(entity);
        }
    }
    
    // Поиск сущностей
    public List<Unit> findUnitsInRange(Position center, double range) {
        return units.stream()
                .filter(unit -> unit.getPosition().distanceTo(center) <= range)
                .collect(Collectors.toList());
    }
    
    public List<Building> findBuildingsByType(String type) {
        return buildings.stream()
                .filter(building -> building.getBuildingType().equals(type))
                .collect(Collectors.toList());
    }
    
    // Управление ресурсами
    public void addResource(String name, int amount) {
        resources.computeIfPresent(name, (k, v) -> {
            v.addAmount(amount);
            return v;
        });
    }
    
    public boolean consumeResource(String name, int amount) {
        Resource resource = resources.get(name);
        return resource != null && resource.consumeAmount(amount);
    }
    
    // События
    public void addEvent(GameEvent event) {
        eventQueue.offer(event);
    }
    
    public void processEvents() {
        while (!eventQueue.isEmpty()) {
            GameEvent event = eventQueue.poll();
            event.execute(this);
        }
    }
    
    // Геттеры
    public Map<Position, GameEntity> getEntityMap() { return entityMap; }
    public List<Unit> getUnits() { return units; }
    public List<Building> getBuildings() { return buildings; }
    public Map<String, Resource> getResources() { return resources; }
    public Set<Position> getOccupiedPositions() { return occupiedPositions; }
}
```

#### 2. Generic класс `Inventory<T>`
```java
public class Inventory<T> {
    private Map<String, T> items;
    private int maxCapacity;
    private int currentCapacity;
    
    public Inventory(int maxCapacity) {
        this.items = new HashMap<>();
        this.maxCapacity = maxCapacity;
        this.currentCapacity = 0;
    }
    
    public boolean addItem(String name, T item) {
        if (currentCapacity < maxCapacity) {
            items.put(name, item);
            currentCapacity++;
            return true;
        }
        return false;
    }
    
    public T getItem(String name) {
        return items.get(name);
    }
    
    public T removeItem(String name) {
        T item = items.remove(name);
        if (item != null) {
            currentCapacity--;
        }
        return item;
    }
    
    public boolean containsItem(String name) {
        return items.containsKey(name);
    }
    
    public List<T> getAllItems() {
        return new ArrayList<>(items.values());
    }
    
    public Set<String> getItemNames() {
        return new HashSet<>(items.keySet());
    }
    
    public int getCurrentCapacity() { return currentCapacity; }
    public int getMaxCapacity() { return maxCapacity; }
    public boolean isFull() { return currentCapacity >= maxCapacity; }
    public boolean isEmpty() { return currentCapacity == 0; }
}
```

#### 3. Generic класс `GameCollection<E extends GameEntity>`
```java
public class GameCollection<E extends GameEntity> {
    private List<E> entities;
    private Map<String, E> entityByName;
    private Set<Position> positions;
    
    public GameCollection() {
        this.entities = new ArrayList<>();
        this.entityByName = new HashMap<>();
        this.positions = new HashSet<>();
    }
    
    public void add(E entity) {
        entities.add(entity);
        entityByName.put(entity.getName(), entity);
        positions.add(entity.getPosition());
    }
    
    public void remove(E entity) {
        entities.remove(entity);
        entityByName.remove(entity.getName());
        positions.remove(entity.getPosition());
    }
    
    public E getByName(String name) {
        return entityByName.get(name);
    }
    
    public E getByPosition(Position position) {
        return entities.stream()
                .filter(e -> e.getPosition().equals(position))
                .findFirst()
                .orElse(null);
    }
    
    public List<E> getActiveEntities() {
        return entities.stream()
                .filter(GameEntity::isActive)
                .collect(Collectors.toList());
    }
    
    public List<E> getEntitiesInRange(Position center, double range) {
        return entities.stream()
                .filter(e -> e.getPosition().distanceTo(center) <= range)
                .collect(Collectors.toList());
    }
    
    public void sortByName() {
        entities.sort(Comparator.comparing(GameEntity::getName));
    }
    
    public void sortByPosition() {
        entities.sort(Comparator.comparing(e -> e.getPosition().getX() + e.getPosition().getY()));
    }
    
    public int size() { return entities.size(); }
    public boolean isEmpty() { return entities.isEmpty(); }
    public void clear() {
        entities.clear();
        entityByName.clear();
        positions.clear();
    }
}
```

#### 4. Generic класс `ResourceManager<T extends Resource>`
```java
public class ResourceManager<T extends Resource> {
    private Map<String, T> resources;
    private List<String> resourceTypes;
    private int totalValue;
    
    public ResourceManager() {
        this.resources = new TreeMap<>();
        this.resourceTypes = new ArrayList<>();
        this.totalValue = 0;
    }
    
    public void addResource(T resource) {
        resources.put(resource.getName(), resource);
        if (!resourceTypes.contains(resource.getName())) {
            resourceTypes.add(resource.getName());
        }
        updateTotalValue();
    }
    
    public T getResource(String name) {
        return resources.get(name);
    }
    
    public boolean consumeResource(String name, int amount) {
        T resource = resources.get(name);
        if (resource != null && resource.consumeAmount(amount)) {
            updateTotalValue();
            return true;
        }
        return false;
    }
    
    public List<T> getResourcesByType(String type) {
        return resources.values().stream()
                .filter(r -> r.getClass().getSimpleName().equals(type))
                .collect(Collectors.toList());
    }
    
    public List<T> getLowResources(int threshold) {
        return resources.values().stream()
                .filter(r -> r.getAmount() < threshold)
                .collect(Collectors.toList());
    }
    
    public Map<String, Integer> getResourceSummary() {
        return resources.values().stream()
                .collect(Collectors.groupingBy(
                    Resource::getName,
                    Collectors.summingInt(Resource::getAmount)
                ));
    }
    
    private void updateTotalValue() {
        totalValue = resources.values().stream()
                .mapToInt(Resource::getValue)
                .sum();
    }
    
    public int getTotalValue() { return totalValue; }
    public int getResourceCount() { return resources.size(); }
    public List<String> getResourceTypes() { return new ArrayList<>(resourceTypes); }
}
```

#### 5. Generic класс `EventQueue<E extends GameEvent>`
```java
public class EventQueue<E extends GameEvent> {
    private Queue<E> events;
    private Map<String, List<E>> eventsByType;
    private int maxSize;
    
    public EventQueue(int maxSize) {
        this.events = new LinkedList<>();
        this.eventsByType = new HashMap<>();
        this.maxSize = maxSize;
    }
    
    public void addEvent(E event) {
        if (events.size() < maxSize) {
            events.offer(event);
            eventsByType.computeIfAbsent(event.getType(), k -> new ArrayList<>())
                       .add(event);
        }
    }
    
    public E getNextEvent() {
        return events.poll();
    }
    
    public E peekNextEvent() {
        return events.peek();
    }
    
    public List<E> getEventsByType(String type) {
        return eventsByType.getOrDefault(type, new ArrayList<>());
    }
    
    public void processEvents(GameWorld world) {
        while (!events.isEmpty()) {
            E event = events.poll();
            event.execute(world);
        }
    }
    
    public boolean isEmpty() { return events.isEmpty(); }
    public int size() { return events.size(); }
    public void clear() {
        events.clear();
        eventsByType.clear();
    }
}
```

### Пример использования коллекций

#### 1. Главный класс игры
```java
public class Game {
    private GameWorld world;
    private Inventory<Item> playerInventory;
    private GameCollection<Unit> unitCollection;
    private ResourceManager<Resource> resourceManager;
    private EventQueue<GameEvent> eventQueue;
    
    public Game() {
        this.world = new GameWorld();
        this.playerInventory = new Inventory<>(100);
        this.unitCollection = new GameCollection<>();
        this.resourceManager = new ResourceManager<>();
        this.eventQueue = new EventQueue<>(1000);
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание юнитов
        Position pos1 = new Position(1, 1);
        Position pos2 = new Position(2, 1);
        
        Unit warrior = new Warrior("Александр", pos1);
        Unit archer = new Archer("Леголас", pos2);
        
        unitCollection.add(warrior);
        unitCollection.add(archer);
        world.addEntity(warrior);
        world.addEntity(archer);
        
        // Создание ресурсов
        Resource gold = new BasicResource("Золото", 1000, 10000, 10);
        Resource wood = new BasicResource("Дерево", 500, 2000, 5);
        
        resourceManager.addResource(gold);
        resourceManager.addResource(wood);
        
        // Создание событий
        GameEvent unitMoveEvent = new UnitMoveEvent(warrior, new Position(3, 3));
        eventQueue.addEvent(unitMoveEvent);
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Обработка событий
        eventQueue.processEvents(world);
        
        // Обновление сущностей
        unitCollection.getActiveEntities().forEach(Unit::update);
        
        // Поиск юнитов в радиусе
        Position center = new Position(2, 2);
        List<Unit> nearbyUnits = unitCollection.getEntitiesInRange(center, 3.0);
        System.out.println("Юниты в радиусе 3 от (2,2): " + nearbyUnits.size());
        
        // Управление ресурсами
        Map<String, Integer> summary = resourceManager.getResourceSummary();
        System.out.println("Сводка ресурсов: " + summary);
        
        // Сортировка юнитов
        unitCollection.sortByName();
        
        displayGameState();
    }
    
    private void displayGameState() {
        System.out.println("\nСостояние игры:");
        System.out.println("Юнитов: " + unitCollection.size());
        System.out.println("Ресурсов: " + resourceManager.getResourceCount());
        System.out.println("Событий в очереди: " + eventQueue.size());
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        
        for (int i = 0; i < 3; i++) {
            game.playTurn();
            System.out.println();
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему коллекций:

### 1. **Гонки на выживание**
- Коллекции: `Inventory<Vehicle>`, `GameCollection<Obstacle>`, `ResourceManager<PowerUp>`
- Структуры: HashMap для трасс, TreeSet для рекордов

### 2. **Космическая колонизация**
- Коллекции: `GameCollection<Planet>`, `ResourceManager<Technology>`, `Inventory<Spaceship>`
- Структуры: TreeMap для звездных систем, HashSet для колоний

### 3. **Подземелье и драконы**
- Коллекции: `GameCollection<Character>`, `Inventory<Item>`, `ResourceManager<Magic>`
- Структуры: HashMap для подземелий, LinkedList для квестов

### 4. **Город-государство**
- Коллекции: `GameCollection<District>`, `ResourceManager<Policy>`, `Inventory<Building>`
- Структуры: TreeMap для районов, HashSet для граждан

### 5. **Пиратская стратегия**
- Коллекции: `GameCollection<Ship>`, `Inventory<Treasure>`, `ResourceManager<Port>`
- Структуры: HashMap для островов, TreeSet для сокровищ

### 6. **Фермерское хозяйство**
- Коллекции: `GameCollection<Field>`, `ResourceManager<Crop>`, `Inventory<Animal>`
- Структуры: TreeMap для сезонов, HashSet для культур

### 7. **Киберпанк-тактика**
- Коллекции: `GameCollection<CyberUnit>`, `Inventory<Implant>`, `ResourceManager<Hack>`
- Структуры: HashMap для сетей, TreeSet для узлов

### 8. **Средневековая осада**
- Коллекции: `GameCollection<Castle>`, `Inventory<SiegeEngine>`, `ResourceManager<Defender>`
- Структуры: TreeMap для укреплений, HashSet для осадных орудий

### 9. **Зомби-выживание**
- Коллекции: `GameCollection<Survivor>`, `Inventory<Weapon>`, `ResourceManager<SafeZone>`
- Структуры: HashMap для зон, TreeSet для угроз

### 10. **Фэнтези-война**
- Коллекции: `GameCollection<Creature>`, `Inventory<Artifact>`, `ResourceManager<Magic>`
- Структуры: TreeMap для магических школ, HashSet для заклинаний

## Требования к реализации

### Обязательные требования:
1. **Создать минимум 3 generic класса** для выбранной игры
2. **Использовать различные типы коллекций** (List, Map, Set, Queue)
3. **Реализовать операции с коллекциями** (добавление, удаление, поиск, сортировка)
4. **Применить Stream API** для обработки данных
5. **Создать менеджер** для работы с коллекциями
6. **Демонстрировать работу** всех коллекций

### Дополнительные требования:
1. **Реализовать Comparable** для сортировки объектов
2. **Создать итераторы** для обхода коллекций
3. **Добавить валидацию** данных в коллекциях
4. **Реализовать паттерн "Observer"** для уведомлений об изменениях

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Generic классы** | 4 | Создание и использование generic классов |
| **Коллекции** | 4 | Применение различных типов коллекций |
| **Stream API** | 3 | Использование Stream API для обработки |
| **Операции** | 3 | Реализация операций с коллекциями |
| **Менеджер** | 2 | Создание менеджера для коллекций |
| **Демонстрация** | 2 | Работающий пример использования |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 19**

## Вопросы для самопроверки

1. В чем разница между ArrayList и LinkedList?
2. Когда использовать HashMap, а когда TreeMap?
3. Что такое generics и зачем они нужны?
4. Как работает Stream API в Java?
5. Что такое wildcards в generics?
6. Как правильно выбрать тип коллекции для задачи?
7. Как применить коллекции для игровых систем?

## Заключение

В данной лабораторной работе вы изучили работу с коллекциями Java и generics на примере создания игровых систем. Вы научились:

- Использовать различные типы коллекций
- Создавать generic классы для типобезопасности
- Применять Stream API для обработки данных
- Организовывать управление игровыми объектами через коллекции

Полученные знания позволят вам эффективно управлять большими наборами игровых объектов, используя современные возможности Java.
