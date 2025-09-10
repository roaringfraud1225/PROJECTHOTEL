# Лабораторная работа 1: Основы классов и объектов
они
## Цель работы
Изучить основы объектно-ориентированного программирования в Java на примере разработки игровых систем. Научиться создавать классы, объекты, конструкторы и методы для реализации базовой игровой логики.

## Теоретические основы

### Классы и объекты
- **Класс** - шаблон для создания объектов, определяющий их структуру и поведение
- **Объект** - экземпляр класса, содержащий данные и методы
- **Инкапсуляция** - объединение данных и методов в единое целое

### Конструкторы
- Специальные методы для инициализации объектов
- Имеют то же имя, что и класс
- Могут быть перегружены (несколько версий с разными параметрами)

### Модификаторы доступа
- `public` - доступ из любого места
- `private` - доступ только внутри класса
- `protected` - доступ внутри пакета и наследников
- `default` (без модификатора) - доступ только внутри пакета

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Основные игровые сущности

#### 1. Класс `Position` (Позиция на игровом поле)
```java
public class Position {
    private int x;
    private int y;
    
    // Конструктор
    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    // Геттеры
    public int getX() { return x; }
    public int getY() { return y; }
    
    // Сеттеры
    public void setX(int x) { this.x = x; }
    public void setY(int y) { this.y = y; }
    
    // Методы
    public double distanceTo(Position other) {
        int dx = this.x - other.x;
        int dy = this.y - other.y;
        return Math.sqrt(dx * dx + dy * dy);
    }
    
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Position position = (Position) obj;
        return x == position.x && y == position.y;
    }
    
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
```

#### 2. Класс `Resource` (Ресурс)
```java
public class Resource {
    private String name;
    private int amount;
    private int maxAmount;
    
    // Конструктор
    public Resource(String name, int initialAmount, int maxAmount) {
        this.name = name;
        this.amount = Math.min(initialAmount, maxAmount);
        this.maxAmount = maxAmount;
    }
    
    // Геттеры
    public String getName() { return name; }
    public int getAmount() { return amount; }
    public int getMaxAmount() { return maxAmount; }
    
    // Методы управления ресурсами
    public boolean addAmount(int value) {
        if (amount + value <= maxAmount) {
            amount += value;
            return true;
        }
        return false;
    }
    
    public boolean consumeAmount(int value) {
        if (amount >= value) {
            amount -= value;
            return true;
        }
        return false;
    }
    
    public boolean isFull() {
        return amount >= maxAmount;
    }
    
    public boolean isEmpty() {
        return amount <= 0;
    }
    
    public String toString() {
        return name + ": " + amount + "/" + maxAmount;
    }
}
```

#### 3. Класс `Building` (Здание)
```java
public class Building {
    private String name;
    private Position position;
    private int level;
    private int maxLevel;
    private boolean isConstructed;
    
    // Конструктор
    public Building(String name, Position position) {
        this.name = name;
        this.position = position;
        this.level = 0;
        this.maxLevel = 3;
        this.isConstructed = false;
    }
    
    // Геттеры
    public String getName() { return name; }
    public Position getPosition() { return position; }
    public int getLevel() { return level; }
    public int getMaxLevel() { return maxLevel; }
    public boolean isConstructed() { return isConstructed; }
    
    // Методы
    public boolean upgrade() {
        if (level < maxLevel && isConstructed) {
            level++;
            return true;
        }
        return false;
    }
    
    public boolean construct() {
        if (!isConstructed) {
            isConstructed = true;
            level = 1;
            return true;
        }
        return false;
    }
    
    public String getStatus() {
        if (!isConstructed) {
            return "В строительстве";
        }
        return "Уровень " + level;
    }
    
    public String toString() {
        return name + " " + getStatus() + " " + position;
    }
}
```

#### 4. Класс `Unit` (Юнит)
```java
public class Unit {
    private String name;
    private Position position;
    private int health;
    private int maxHealth;
    private int attack;
    private int defense;
    private int movementRange;
    
    // Конструктор
    public Unit(String name, Position position, int health, int attack, int defense, int movementRange) {
        this.name = name;
        this.position = position;
        this.maxHealth = health;
        this.health = health;
        this.attack = attack;
        this.defense = defense;
        this.movementRange = movementRange;
    }
    
    // Геттеры
    public String getName() { return name; }
    public Position getPosition() { return position; }
    public int getHealth() { return health; }
    public int getMaxHealth() { return maxHealth; }
    public int getAttack() { return attack; }
    public int getDefense() { return defense; }
    public int getMovementRange() { return movementRange; }
    
    // Методы
    public boolean moveTo(Position newPosition) {
        double distance = position.distanceTo(newPosition);
        if (distance <= movementRange) {
            position = newPosition;
            return true;
        }
        return false;
    }
    
    public void takeDamage(int damage) {
        int actualDamage = Math.max(1, damage - defense);
        health = Math.max(0, health - actualDamage);
    }
    
    public void heal(int amount) {
        health = Math.min(maxHealth, health + amount);
    }
    
    public boolean isAlive() {
        return health > 0;
    }
    
    public double getHealthPercentage() {
        return (double) health / maxHealth * 100;
    }
    
    public String toString() {
        return name + " [" + health + "/" + maxHealth + " HP] " + position;
    }
}
```

### Дополнительные классы для расширения

#### 5. Класс `GameMap` (Игровая карта)
```java
public class GameMap {
    private int width;
    private int height;
    private Building[][] buildings;
    private Unit[][] units;
    
    // Конструктор
    public GameMap(int width, int height) {
        this.width = width;
        this.height = height;
        this.buildings = new Building[width][height];
        this.units = new Unit[width][height];
    }
    
    // Методы
    public boolean isValidPosition(Position position) {
        return position.getX() >= 0 && position.getX() < width &&
               position.getY() >= 0 && position.getY() < height;
    }
    
    public boolean placeBuilding(Building building) {
        Position pos = building.getPosition();
        if (isValidPosition(pos) && buildings[pos.getX()][pos.getY()] == null) {
            buildings[pos.getX()][pos.getY()] = building;
            return true;
        }
        return false;
    }
    
    public boolean placeUnit(Unit unit) {
        Position pos = unit.getPosition();
        if (isValidPosition(pos) && units[pos.getX()][pos.getY()] == null) {
            units[pos.getX()][pos.getY()] = unit;
            return true;
        }
        return false;
    }
    
    public Building getBuildingAt(Position position) {
        if (isValidPosition(position)) {
            return buildings[position.getX()][position.getY()];
        }
        return null;
    }
    
    public Unit getUnitAt(Position position) {
        if (isValidPosition(position)) {
            return units[position.getX()][position.getY()];
        }
        return null;
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную структуру классов:

### 1. **Гонки на выживание**
- Классы: `Vehicle`, `Track`, `Obstacle`, `PowerUp`
- Механики: гонки, сбор ресурсов, улучшение транспорта

### 2. **Космическая колонизация**
- Классы: `Planet`, `Spaceship`, `Colony`, `Technology`
- Механики: освоение планет, строительство станций

### 3. **Подземелье и драконы**
- Классы: `Character`, `Dungeon`, `Monster`, `Item`
- Механики: RPG, пошаговые бои, исследование

### 4. **Город-государство**
- Классы: `City`, `District`, `Citizen`, `Policy`
- Механики: строительство города, экономика, дипломатия

### 5. **Пиратская стратегия**
- Классы: `Ship`, `Port`, `Treasure`, `Crew`
- Механики: морские сражения, поиск сокровищ

### 6. **Фермерское хозяйство**
- Классы: `Field`, `Crop`, `Animal`, `Market`
- Механики: выращивание, животноводство, торговля

### 7. **Киберпанк-тактика**
- Классы: `CyberUnit`, `Grid`, `Hack`, `Implant`
- Механики: тактические сражения, хакерские способности

### 8. **Средневековая осада**
- Классы: `Castle`, `SiegeEngine`, `Defender`, `Wall`
- Механики: оборона/осада, инженерные сооружения

### 9. **Зомби-выживание**
- Классы: `Survivor`, `SafeZone`, `Zombie`, `Weapon`
- Механики: построение базы, защита, крафтинг

### 10. **Фэнтези-война**
- Классы: `Creature`, `Magic`, `Artifact`, `Battlefield`
- Механики: магические существа, заклинания, сражения

## Требования к реализации

### Обязательные требования:
1. **Создать все базовые классы** для выбранной игры
2. **Реализовать конструкторы** с параметрами
3. **Добавить геттеры и сеттеры** для всех полей
4. **Создать методы** для основной логики игры
5. **Реализовать toString()** для всех классов
6. **Добавить валидацию** входных данных

### Дополнительные требования:
1. **Создать главный класс** `Game` для управления игрой
2. **Реализовать простую игровую логику** (инициализация, ход, проверка условий)
3. **Добавить обработку ошибок** (некорректные позиции, недостаточно ресурсов)
4. **Создать демонстрацию** работы всех классов

## Пример главного класса

```java
public class Game {
    private GameMap gameMap;
    private List<Resource> resources;
    private List<Building> buildings;
    private List<Unit> units;
    
    public Game() {
        this.gameMap = new GameMap(10, 10);
        this.resources = new ArrayList<>();
        this.buildings = new ArrayList<>();
        this.units = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание начальных ресурсов
        resources.add(new Resource("Золото", 1000, 10000));
        resources.add(new Resource("Дерево", 500, 2000));
        resources.add(new Resource("Камень", 300, 1500));
        
        // Создание начальных зданий
        Position castlePos = new Position(5, 5);
        Building castle = new Building("Замок", castlePos);
        buildings.add(castle);
        gameMap.placeBuilding(castle);
        
        // Создание начальных юнитов
        Position knightPos = new Position(4, 5);
        Unit knight = new Unit("Рыцарь", knightPos, 100, 15, 10, 2);
        units.add(knight);
        gameMap.placeUnit(knight);
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        displayGameState();
        
        // Простая логика хода
        for (Unit unit : units) {
            if (unit.isAlive()) {
                // Простое перемещение
                Position currentPos = unit.getPosition();
                Position newPos = new Position(currentPos.getX() + 1, currentPos.getY());
                if (gameMap.isValidPosition(newPos)) {
                    unit.moveTo(newPos);
                }
            }
        }
    }
    
    private void displayGameState() {
        System.out.println("Ресурсы:");
        for (Resource resource : resources) {
            System.out.println("  " + resource);
        }
        
        System.out.println("Здания:");
        for (Building building : buildings) {
            System.out.println("  " + building);
        }
        
        System.out.println("Юниты:");
        for (Unit unit : units) {
            System.out.println("  " + unit);
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

## Тестирование

### Создать тесты для проверки:
1. **Создание объектов** с корректными параметрами
2. **Валидация данных** (отрицательные значения, некорректные позиции)
3. **Работа методов** (перемещение, атака, строительство)
4. **Граничные случаи** (максимальные значения, пустые объекты)

### Пример теста:
```java
@Test
public void testUnitCreation() {
    Position pos = new Position(0, 0);
    Unit unit = new Unit("Тест", pos, 100, 10, 5, 2);
    
    assertEquals("Тест", unit.getName());
    assertEquals(100, unit.getMaxHealth());
    assertEquals(100, unit.getHealth());
    assertTrue(unit.isAlive());
}
```

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Создание классов** | 3 | Все необходимые классы созданы с правильной структурой |
| **Конструкторы** | 2 | Конструкторы корректно инициализируют объекты |
| **Методы** | 3 | Реализованы все необходимые методы с логикой |
| **Валидация** | 2 | Добавлена проверка входных данных |
| **Демонстрация** | 2 | Создан работающий пример игры |
| **Качество кода** | 2 | Читаемость, комментарии, именование |
| **Тестирование** | 1 | Созданы базовые тесты |

**Максимальный балл: 15**

## Вопросы для самопроверки

1. Что такое класс и чем он отличается от объекта?
2. Зачем нужны конструкторы и когда они вызываются?
3. Какие модификаторы доступа вы знаете и в чем их различие?
4. Почему важно использовать геттеры и сеттеры?
5. Как работает метод `toString()` и зачем он нужен?
6. Что такое инкапсуляция и как она реализуется в Java?
7. Как правильно организовать структуру классов для игры?

## Дополнительные материалы

- [Java Classes and Objects](https://docs.oracle.com/javase/tutorial/java/javaOO/)
- [Java Access Modifiers](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)
- [Java Constructors](https://docs.oracle.com/javase/tutorial/java/javaOO/constructors.html)
- [Game Development Patterns](https://gameprogrammingpatterns.com/)

## Заключение

В данной лабораторной работе вы изучили основы создания классов и объектов в Java на примере игровой системы "Королевство". Вы научились:

- Создавать классы с полями и методами
- Использовать конструкторы для инициализации
- Применять принципы инкапсуляции
- Организовывать взаимодействие между объектами

Полученные знания позволят вам создать собственную игру на основе одной из предложенных тем, применив изученные концепции ООП.
