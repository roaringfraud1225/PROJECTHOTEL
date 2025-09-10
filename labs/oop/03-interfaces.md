---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Лабораторная работа 3: Интерфейсы и абстрактные классы

## Цель работы
Изучить принципы работы с интерфейсами и абстрактными классами в Java на примере разработки игровых систем. Научиться создавать контракты для классов, реализовывать множественное наследование и использовать абстракцию для гибкого проектирования.

## Теоретические основы

### Интерфейсы
- **Интерфейс** - контракт, определяющий методы, которые должны реализовать классы
- **Множественное наследование** - класс может реализовывать несколько интерфейсов
- **Контракт** - гарантия того, что класс предоставляет определенную функциональность
- Ключевое слово `implements` для реализации интерфейсов

### Абстрактные классы
- **Абстрактный класс** - класс, который не может быть создан напрямую
- **Абстрактные методы** - методы без реализации, которые должны быть реализованы в дочерних классах
- **Частичная реализация** - абстрактный класс может содержать как абстрактные, так и обычные методы

### Сравнение интерфейсов и абстрактных классов
- **Интерфейсы** - только контракт, множественное наследование, нет состояния
- **Абстрактные классы** - могут содержать состояние, единичное наследование, частичная реализация

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система интерфейсов

#### 1. Интерфейс `Movable` (Перемещаемый)
```java
public interface Movable {
    /**
     * Перемещение объекта в новую позицию
     * @param newPosition новая позиция
     * @return true если перемещение успешно, false иначе
     */
    boolean moveTo(Position newPosition);
    
    /**
     * Проверка возможности перемещения в позицию
     * @param position позиция для проверки
     * @return true если перемещение возможно
     */
    boolean canMoveTo(Position position);
    
    /**
     * Получение дальности перемещения
     * @return максимальная дальность перемещения за один ход
     */
    int getMovementRange();
    
    /**
     * Получение текущей позиции
     * @return текущая позиция объекта
     */
    Position getCurrentPosition();
}
```

#### 2. Интерфейс `Combatable` (Боеспособный)
```java
public interface Combatable {
    /**
     * Атака цели
     * @param target цель атаки
     * @return урон, нанесенный цели
     */
    int attack(Combatable target);
    
    /**
     * Получение урона
     * @param damage количество урона
     */
    void takeDamage(int damage);
    
    /**
     * Проверка возможности атаки цели
     * @param target цель для проверки
     * @return true если атака возможна
     */
    boolean canAttack(Combatable target);
    
    /**
     * Получение атаки
     * @return значение атаки
     */
    int getAttack();
    
    /**
     * Получение защиты
     * @return значение защиты
     */
    int getDefense();
    
    /**
     * Проверка жизни
     * @return true если объект жив
     */
    boolean isAlive();
}
```

#### 3. Интерфейс `Producible` (Производимый)
```java
public interface Producible {
    /**
     * Производство ресурса или юнита
     * @return количество произведенных единиц
     */
    int produce();
    
    /**
     * Получение скорости производства
     * @return количество единиц за ход
     */
    int getProductionRate();
    
    /**
     * Проверка возможности производства
     * @return true если производство возможно
     */
    boolean canProduce();
    
    /**
     * Получение стоимости производства
     * @return стоимость в ресурсах
     */
    int getProductionCost();
    
    /**
     * Получение типа производимого объекта
     * @return название типа
     */
    String getProductType();
}
```

#### 4. Интерфейс `Upgradeable` (Улучшаемый)
```java
public interface Upgradeable {
    /**
     * Улучшение объекта
     * @return true если улучшение успешно
     */
    boolean upgrade();
    
    /**
     * Получение текущего уровня
     * @return текущий уровень
     */
    int getLevel();
    
    /**
     * Получение максимального уровня
     * @return максимальный уровень
     */
    int getMaxLevel();
    
    /**
     * Проверка возможности улучшения
     * @return true если улучшение возможно
     */
    boolean canUpgrade();
    
    /**
     * Получение стоимости улучшения
     * @return стоимость улучшения
     */
    int getUpgradeCost();
    
    /**
     * Получение бонусов от уровня
     * @return описание бонусов
     */
    String getLevelBonuses();
}
```

#### 5. Интерфейс `Tradeable` (Торгуемый)
```java
public interface Tradeable {
    /**
     * Получение стоимости объекта
     * @return стоимость в золоте
     */
    int getValue();
    
    /**
     * Проверка возможности продажи
     * @return true если объект можно продать
     */
    boolean canBeSold();
    
    /**
     * Проверка возможности покупки
     * @return true если объект можно купить
     */
    boolean canBeBought();
    
    /**
     * Получение типа торговли
     * @return тип (продажа/покупка/обмен)
     */
    String getTradeType();
    
    /**
     * Получение требований для торговли
     * @return список требований
     */
    List<String> getTradeRequirements();
}
```

### Реализация интерфейсов в классах

#### 1. Класс `Unit` реализует `Movable` и `Combatable`
```java
public abstract class Unit extends GameEntity implements Movable, Combatable {
    protected int health;
    protected int maxHealth;
    protected int attack;
    protected int defense;
    protected int movementRange;
    protected int experience;
    protected int level;
    
    // Конструктор
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
    
    // Реализация интерфейса Movable
    @Override
    public boolean moveTo(Position newPosition) {
        if (canMoveTo(newPosition)) {
            position = newPosition;
            return true;
        }
        return false;
    }
    
    @Override
    public boolean canMoveTo(Position position) {
        double distance = this.position.distanceTo(position);
        return distance <= movementRange && position.getX() >= 0 && position.getY() >= 0;
    }
    
    @Override
    public int getMovementRange() {
        return movementRange;
    }
    
    @Override
    public Position getCurrentPosition() {
        return position;
    }
    
    // Реализация интерфейса Combatable
    @Override
    public int attack(Combatable target) {
        if (canAttack(target)) {
            int damage = calculateDamage();
            target.takeDamage(damage);
            gainExperience(10);
            return damage;
        }
        return 0;
    }
    
    @Override
    public void takeDamage(int damage) {
        int actualDamage = Math.max(1, damage - defense);
        health = Math.max(0, health - actualDamage);
        if (health <= 0) {
            deactivate();
        }
    }
    
    @Override
    public boolean canAttack(Combatable target) {
        if (!isAlive()) return false;
        
        if (target instanceof Unit) {
            Unit targetUnit = (Unit) target;
            double distance = position.distanceTo(targetUnit.getCurrentPosition());
            return distance <= getAttackRange();
        }
        return false;
    }
    
    @Override
    public int getAttack() {
        return attack;
    }
    
    @Override
    public int getDefense() {
        return defense;
    }
    
    @Override
    public boolean isAlive() {
        return health > 0 && isActive;
    }
    
    // Абстрактные методы
    public abstract int getAttackRange();
    public abstract void specialAbility();
    public abstract int getExperienceReward();
    
    // Вспомогательные методы
    protected int calculateDamage() {
        return attack + (int)(Math.random() * 5);
    }
    
    protected void gainExperience(int exp) {
        experience += exp;
        checkLevelUp();
    }
    
    private void checkLevelUp() {
        int requiredExp = level * 100;
        if (experience >= requiredExp) {
            levelUp();
        }
    }
    
    protected void levelUp() {
        level++;
        maxHealth += 20;
        health = maxHealth;
        attack += 5;
        defense += 3;
        System.out.println(name + " достиг уровня " + level + "!");
    }
    
    public double getHealthPercentage() {
        return (double) health / maxHealth * 100;
    }
    
    @Override
    public void update() {
        if (health < maxHealth * 0.3) {
            heal(5);
        }
    }
    
    @Override
    public String getDescription() {
        return name + " [Ур." + level + "] [" + health + "/" + maxHealth + " HP] " +
               "Атака: " + attack + " Защита: " + defense;
    }
}
```

#### 2. Класс `Building` реализует `Producible` и `Upgradeable`
```java
public abstract class Building extends GameEntity implements Producible, Upgradeable {
    protected int level;
    protected int maxLevel;
    protected boolean isConstructed;
    protected int constructionProgress;
    protected int constructionCost;
    protected List<Resource> requiredResources;
    
    // Конструктор
    public Building(String name, Position position, int constructionCost) {
        super(name, position);
        this.level = 0;
        this.maxLevel = 3;
        this.isConstructed = false;
        this.constructionProgress = 0;
        this.constructionCost = constructionCost;
        this.requiredResources = new ArrayList<>();
    }
    
    // Реализация интерфейса Producible
    @Override
    public int produce() {
        if (!canProduce()) return 0;
        
        int production = getProductionRate();
        System.out.println(name + " производит " + production + " " + getProductType());
        return production;
    }
    
    @Override
    public int getProductionRate() {
        return getBaseProductionRate() * level;
    }
    
    @Override
    public boolean canProduce() {
        return isConstructed && isActive;
    }
    
    @Override
    public int getProductionCost() {
        return constructionCost;
    }
    
    @Override
    public String getProductType() {
        return getBuildingType();
    }
    
    // Реализация интерфейса Upgradeable
    @Override
    public boolean upgrade() {
        if (!canUpgrade()) return false;
        
        level++;
        System.out.println(name + " улучшен до уровня " + level + "!");
        return true;
    }
    
    @Override
    public int getLevel() {
        return level;
    }
    
    @Override
    public int getMaxLevel() {
        return maxLevel;
    }
    
    @Override
    public boolean canUpgrade() {
        return isConstructed && level < maxLevel && isActive;
    }
    
    @Override
    public int getUpgradeCost() {
        return constructionCost * (level + 1);
    }
    
    @Override
    public String getLevelBonuses() {
        return "Уровень " + level + ": +" + (level * 10) + "% к производству";
    }
    
    // Абстрактные методы
    protected abstract int getBaseProductionRate();
    protected abstract String getBuildingType();
    
    // Общие методы для всех зданий
    public boolean construct() {
        if (!isConstructed && constructionProgress >= constructionCost) {
            isConstructed = true;
            level = 1;
            return true;
        }
        return false;
    }
    
    public void addConstructionProgress(int progress) {
        if (!isConstructed) {
            constructionProgress += progress;
            if (constructionProgress >= constructionCost) {
                construct();
            }
        }
    }
    
    public double getConstructionPercentage() {
        if (isConstructed) return 100.0;
        return (double) constructionProgress / constructionCost * 100;
    }
    
    @Override
    public void update() {
        if (isConstructed) {
            produce();
        }
    }
    
    @Override
    public String getDescription() {
        if (!isConstructed) {
            return name + " [Строительство: " + String.format("%.1f", getConstructionPercentage()) + "%]";
        }
        return name + " [Ур." + level + "/" + maxLevel + "] " + getBuildingType();
    }
}
```

#### 3. Класс `Resource` реализует `Tradeable`
```java
public abstract class Resource implements Tradeable {
    protected String name;
    protected int amount;
    protected int maxAmount;
    protected boolean isRenewable;
    protected int baseValue;
    
    // Конструктор
    public Resource(String name, int initialAmount, int maxAmount, boolean isRenewable, int baseValue) {
        this.name = name;
        this.amount = Math.min(initialAmount, maxAmount);
        this.maxAmount = maxAmount;
        this.isRenewable = isRenewable;
        this.baseValue = baseValue;
    }
    
    // Реализация интерфейса Tradeable
    @Override
    public int getValue() {
        return amount * baseValue;
    }
    
    @Override
    public boolean canBeSold() {
        return amount > 0 && isTradeable();
    }
    
    @Override
    public boolean canBeBought() {
        return amount < maxAmount && isTradeable();
    }
    
    @Override
    public String getTradeType() {
        if (canBeSold() && canBeBought()) return "Продажа/Покупка";
        if (canBeSold()) return "Только продажа";
        if (canBeBought()) return "Только покупка";
        return "Не торгуется";
    }
    
    @Override
    public List<String> getTradeRequirements() {
        List<String> requirements = new ArrayList<>();
        if (amount > 0) requirements.add("Наличие ресурса");
        if (isTradeable()) requirements.add("Разрешение на торговлю");
        return requirements;
    }
    
    // Абстрактные методы
    protected abstract boolean isTradeable();
    public abstract void process();
    public abstract int getQuality();
    
    // Общие методы
    public String getName() { return name; }
    public int getAmount() { return amount; }
    public int getMaxAmount() { return maxAmount; }
    public boolean isRenewable() { return isRenewable; }
    
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
    
    public double getUsagePercentage() {
        return (double) amount / maxAmount * 100;
    }
    
    @Override
    public String toString() {
        return name + ": " + amount + "/" + maxAmount + 
               " [" + String.format("%.1f", getUsagePercentage()) + "%] " +
               "Ценность: " + getValue();
    }
}
```

### Абстрактные классы

#### 1. Абстрактный класс `GameEntity`
```java
public abstract class GameEntity {
    protected String name;
    protected Position position;
    protected boolean isActive;
    
    // Конструктор
    public GameEntity(String name, Position position) {
        this.name = name;
        this.position = position;
        this.isActive = true;
    }
    
    // Абстрактные методы
    public abstract void update();
    public abstract String getDescription();
    
    // Общие методы
    public String getName() { return name; }
    public Position getPosition() { return position; }
    public boolean isActive() { return isActive; }
    
    public void setPosition(Position position) {
        this.position = position;
    }
    
    public void deactivate() {
        this.isActive = false;
    }
    
    public void activate() {
        this.isActive = true;
    }
    
    public double distanceTo(GameEntity other) {
        return position.distanceTo(other.position);
    }
    
    @Override
    public String toString() {
        return name + " " + position + " [" + (isActive ? "Активен" : "Неактивен") + "]";
    }
}
```

#### 2. Абстрактный класс `CombatSystem`
```java
public abstract class CombatSystem {
    protected List<Combatable> participants;
    protected boolean isActive;
    
    public CombatSystem() {
        this.participants = new ArrayList<>();
        this.isActive = false;
    }
    
    // Абстрактные методы
    public abstract void startCombat();
    public abstract void processTurn();
    public abstract boolean isCombatFinished();
    public abstract List<Combatable> getWinners();
    
    // Общие методы
    public void addParticipant(Combatable participant) {
        if (participant.isAlive()) {
            participants.add(participant);
        }
    }
    
    public void removeParticipant(Combatable participant) {
        participants.remove(participant);
    }
    
    public List<Combatable> getAliveParticipants() {
        return participants.stream()
                .filter(Combatable::isAlive)
                .collect(Collectors.toList());
    }
    
    public boolean isActive() {
        return isActive;
    }
    
    public void setActive(boolean active) {
        this.isActive = active;
    }
    
    public int getParticipantCount() {
        return participants.size();
    }
}
```

### Пример использования интерфейсов

#### 1. Класс `GameManager` с полиморфизмом
```java
public class GameManager {
    private List<Movable> movableEntities;
    private List<Combatable> combatEntities;
    private List<Producible> productionEntities;
    private List<Upgradeable> upgradeableEntities;
    private List<Tradeable> tradeableEntities;
    
    public GameManager() {
        this.movableEntities = new ArrayList<>();
        this.combatEntities = new ArrayList<>();
        this.productionEntities = new ArrayList<>();
        this.upgradeableEntities = new ArrayList<>();
        this.tradeableEntities = new ArrayList<>();
    }
    
    // Добавление сущностей по интерфейсам
    public void addMovable(Movable movable) {
        movableEntities.add(movable);
    }
    
    public void addCombatable(Combatable combatable) {
        combatEntities.add(combatable);
    }
    
    public void addProducible(Producible producible) {
        productionEntities.add(producible);
    }
    
    public void addUpgradeable(Upgradeable upgradeable) {
        upgradeableEntities.add(upgradeable);
    }
    
    public void addTradeable(Tradeable tradeable) {
        tradeableEntities.add(tradeable);
    }
    
    // Полиморфные операции
    public void moveAllEntities(Position targetPosition) {
        for (Movable movable : movableEntities) {
            if (movable.canMoveTo(targetPosition)) {
                movable.moveTo(targetPosition);
            }
        }
    }
    
    public void processAllProduction() {
        for (Producible producible : productionEntities) {
            if (producible.canProduce()) {
                producible.produce();
            }
        }
    }
    
    public void upgradeAllPossible() {
        for (Upgradeable upgradeable : upgradeableEntities) {
            if (upgradeable.canUpgrade()) {
                upgradeable.upgrade();
            }
        }
    }
    
    public int getTotalTradeValue() {
        return tradeableEntities.stream()
                .mapToInt(Tradeable::getValue)
                .sum();
    }
    
    public void displayCombatStatus() {
        System.out.println("Боевые сущности:");
        for (Combatable combatable : combatEntities) {
            String status = combatable.isAlive() ? "Жив" : "Мертв";
            System.out.println("  " + status + " - Атака: " + combatable.getAttack() + 
                             ", Защита: " + combatable.getDefense());
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему интерфейсов:

### 1. **Гонки на выживание**
- Интерфейсы: `Movable`, `Upgradeable`, `Tradeable`
- Абстрактные классы: `Vehicle`, `TrackElement`
- Реализация: `Car`, `Motorcycle`, `Obstacle`, `PowerUp`

### 2. **Космическая колонизация**
- Интерфейсы: `Producible`, `Upgradeable`, `Tradeable`
- Абстрактные классы: `SpaceObject`, `Colonizable`
- Реализация: `Planet`, `Spaceship`, `Colony`, `Technology`

### 3. **Подземелье и драконы**
- Интерфейсы: `Movable`, `Combatable`, `Tradeable`
- Абстрактные классы: `Character`, `DungeonElement`
- Реализация: `Warrior`, `Mage`, `Monster`, `Treasure`

### 4. **Город-государство**
- Интерфейсы: `Producible`, `Upgradeable`, `Tradeable`
- Абстрактные классы: `CityElement`, `Infrastructure`
- Реализация: `Residential`, `Commercial`, `Road`, `Policy`

### 5. **Пиратская стратегия**
- Интерфейсы: `Movable`, `Combatable`, `Tradeable`
- Абстрактные классы: `MaritimeObject`, `CrewMember`
- Реализация: `Ship`, `Port`, `Captain`, `Treasure`

### 6. **Фермерское хозяйство**
- Интерфейсы: `Producible`, `Upgradeable`, `Tradeable`
- Абстрактные классы: `FarmElement`, `LivingThing`
- Реализация: `Field`, `Crop`, `Animal`, `Market`

### 7. **Киберпанк-тактика**
- Интерфейсы: `Movable`, `Combatable`, `Upgradeable`
- Абстрактные классы: `CyberEntity`, `GridElement`
- Реализация: `CyberUnit`, `Hacker`, `Node`, `Virus`

### 8. **Средневековая осада**
- Интерфейсы: `Combatable`, `Upgradeable`, `Producible`
- Абстрактные классы: `CastleElement`, `SiegeEquipment`
- Реализация: `Tower`, `Wall`, `Catapult`, `Defender`

### 9. **Зомби-выживание**
- Интерфейсы: `Movable`, `Combatable`, `Producible`
- Абстрактные классы: `SurvivalElement`, `Threat`
- Реализация: `Survivor`, `SafeZone`, `Zombie`, `Weapon`

### 10. **Фэнтези-война**
- Интерфейсы: `Movable`, `Combatable`, `Upgradeable`
- Абстрактные классы: `FantasyEntity`, `MagicElement`
- Реализация: `Creature`, `Hero`, `Spell`, `Artifact`

## Требования к реализации

### Обязательные требования:
1. **Создать минимум 5 интерфейсов** для выбранной игры
2. **Реализовать интерфейсы** в соответствующих классах
3. **Создать абстрактные классы** с абстрактными методами
4. **Использовать множественное наследование** интерфейсов
5. **Применить полиморфизм** через интерфейсы
6. **Создать менеджер** для работы с сущностями по интерфейсам

### Дополнительные требования:
1. **Реализовать интерфейс `Comparable`** для сортировки
2. **Создать интерфейс `Cloneable`** для копирования объектов
3. **Добавить интерфейс `Serializable`** для сохранения
4. **Реализовать паттерн "Стратегия"** через интерфейсы

## Пример главного класса

```java
public class Game {
    private GameManager gameManager;
    private List<Unit> units;
    private List<Building> buildings;
    private List<Resource> resources;
    
    public Game() {
        this.gameManager = new GameManager();
        this.units = new ArrayList<>();
        this.buildings = new ArrayList<>();
        this.resources = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание юнитов (реализуют Movable и Combatable)
        Position pos1 = new Position(1, 1);
        Position pos2 = new Position(2, 1);
        
        Unit warrior = new Warrior("Александр", pos1);
        Unit archer = new Archer("Леголас", pos2);
        
        units.add(warrior);
        units.add(archer);
        
        // Добавление в менеджер по интерфейсам
        gameManager.addMovable(warrior);
        gameManager.addMovable(archer);
        gameManager.addCombatable(warrior);
        gameManager.addCombatable(archer);
        
        // Создание зданий (реализуют Producible и Upgradeable)
        Position buildPos1 = new Position(5, 5);
        Building mine = new ResourceBuilding("Шахта", buildPos1, "Железо", 50);
        
        buildings.add(mine);
        gameManager.addProducible(mine);
        gameManager.addUpgradeable(mine);
        
        // Создание ресурсов (реализуют Tradeable)
        Resource gold = new BasicResource("Золото", 1000, 10000, 10);
        resources.add(gold);
        gameManager.addTradeable(gold);
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Полиморфные операции через интерфейсы
        Position targetPos = new Position(3, 3);
        gameManager.moveAllEntities(targetPos);
        
        gameManager.processAllProduction();
        gameManager.upgradeAllPossible();
        
        gameManager.displayCombatStatus();
        
        System.out.println("Общая торговая ценность: " + gameManager.getTotalTradeValue());
        
        displayGameState();
    }
    
    private void displayGameState() {
        System.out.println("\nСостояние игры:");
        
        System.out.println("Юниты:");
        for (Unit unit : units) {
            System.out.println("  " + unit.getDescription());
        }
        
        System.out.println("Здания:");
        for (Building building : buildings) {
            System.out.println("  " + building.getDescription());
        }
        
        System.out.println("Ресурсы:");
        for (Resource resource : resources) {
            System.out.println("  " + resource);
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
1. **Реализации интерфейсов** - корректность реализации всех методов
2. **Полиморфизма** - работа через ссылки на интерфейсы
3. **Абстрактных классов** - создание объектов дочерних классов
4. **Множественного наследования** - корректность работы нескольких интерфейсов

### Пример теста:
```java
@Test
public void testInterfaceImplementation() {
    Unit warrior = new Warrior("Тест", new Position(0, 0));
    
    // Тест интерфейса Movable
    assertTrue(warrior instanceof Movable);
    assertTrue(warrior.canMoveTo(new Position(1, 1)));
    
    // Тест интерфейса Combatable
    assertTrue(warrior instanceof Combatable);
    assertTrue(warrior.isAlive());
    assertTrue(warrior.getAttack() > 0);
}
```

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Интерфейсы** | 4 | Создание и реализация интерфейсов |
| **Абстрактные классы** | 3 | Использование абстрактных классов и методов |
| **Множественное наследование** | 3 | Реализация нескольких интерфейсов |
| **Полиморфизм** | 3 | Применение полиморфизма через интерфейсы |
| **Менеджер** | 2 | Создание менеджера для работы с интерфейсами |
| **Демонстрация** | 2 | Работающий пример использования интерфейсов |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Что такое интерфейс и чем он отличается от абстрактного класса?
2. Как реализуется множественное наследование в Java?
3. Зачем нужны интерфейсы и как они обеспечивают гибкость?
4. Что такое контракт и как он связан с интерфейсами?
5. Как работает полиморфизм через интерфейсы?
6. Когда использовать интерфейсы, а когда абстрактные классы?
7. Как применить интерфейсы для создания игровых систем?

## Дополнительные материалы

- [Java Interfaces](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)
- [Java Abstract Classes](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)
- [Java Multiple Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/multipleinheritance.html)
- [Interface Design Principles](https://www.baeldung.com/java-interface-design)

## Заключение

В данной лабораторной работе вы изучили принципы работы с интерфейсами и абстрактными классами в Java на примере создания игровых систем. Вы научились:

- Создавать интерфейсы для определения контрактов
- Реализовывать множественное наследование через интерфейсы
- Использовать абстрактные классы для частичной реализации
- Применять полиморфизм через интерфейсы
- Организовывать гибкую архитектуру игровых систем

Полученные знания позволят вам создавать расширяемые и гибкие игровые системы, используя принципы интерфейсного программирования.
