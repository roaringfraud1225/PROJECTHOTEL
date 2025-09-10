# Лабораторная работа 2: Наследование и полиморфизм

## Цель работы
Изучить принципы наследования и полиморфизма в Java на примере разработки иерархии игровых сущностей. Научиться создавать базовые и производные классы, использовать переопределение методов и абстрактные классы.

## Теоретические основы

### Наследование
- **Наследование** - механизм создания нового класса на основе существующего
- **Базовый класс** (родительский) - класс, от которого наследуются другие
- **Производный класс** (дочерний) - класс, который наследует от базового
- Ключевое слово `extends` для наследования

### Полиморфизм
- **Полиморфизм** - способность объектов с одинаковым интерфейсом иметь различное поведение
- **Переопределение методов** - замена реализации метода в дочернем классе
- **Абстрактные классы** - классы, которые не могут быть созданы напрямую

### Модификаторы доступа при наследовании
- `private` - недоступен в дочерних классах
- `protected` - доступен в дочерних классах
- `public` - доступен везде
- `final` - запрещает наследование или переопределение

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Иерархия классов

#### 1. Базовый класс `GameEntity` (Игровая сущность)
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

#### 2. Иерархия юнитов

##### Базовый класс `Unit` (наследует от `GameEntity`)
```java
public abstract class Unit extends GameEntity {
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
    
    // Абстрактные методы
    public abstract void specialAbility();
    public abstract int getExperienceReward();
    
    // Общие методы для всех юнитов
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
        if (health <= 0) {
            deactivate();
        }
    }
    
    public void heal(int amount) {
        health = Math.min(maxHealth, health + amount);
    }
    
    public boolean isAlive() {
        return health > 0 && isActive;
    }
    
    public void gainExperience(int exp) {
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
        // Базовая логика обновления юнита
        if (health < maxHealth * 0.3) {
            // Автоматическое лечение при низком здоровье
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

##### Производные классы юнитов

###### Класс `Warrior` (Воин)
```java
public class Warrior extends Unit {
    private boolean isCharged;
    private int chargeCooldown;
    
    public Warrior(String name, Position position) {
        super(name, position, 120, 18, 12, 2);
        this.isCharged = false;
        this.chargeCooldown = 0;
    }
    
    @Override
    public void specialAbility() {
        if (chargeCooldown <= 0) {
            isCharged = true;
            chargeCooldown = 3;
            attack += 10;
            System.out.println(name + " готов к мощной атаке!");
        }
    }
    
    @Override
    public int getExperienceReward() {
        return 50;
    }
    
    @Override
    public void update() {
        super.update();
        
        if (chargeCooldown > 0) {
            chargeCooldown--;
            if (chargeCooldown == 0) {
                isCharged = false;
                attack -= 10;
                System.out.println(name + " больше не заряжен.");
            }
        }
    }
    
    @Override
    public String getDescription() {
        String chargeStatus = isCharged ? " [ЗАРЯЖЕН]" : "";
        return super.getDescription() + chargeStatus;
    }
}
```

###### Класс `Archer` (Лучник)
```java
public class Archer extends Unit {
    private int arrows;
    private int maxArrows;
    private boolean isAiming;
    
    public Archer(String name, Position position) {
        super(name, position, 80, 25, 8, 3);
        this.maxArrows = 20;
        this.arrows = maxArrows;
        this.isAiming = false;
    }
    
    @Override
    public void specialAbility() {
        if (arrows > 0) {
            isAiming = true;
            attack += 15;
            System.out.println(name + " прицелился для точного выстрела!");
        }
    }
    
    @Override
    public int getExperienceReward() {
        return 40;
    }
    
    public boolean shoot() {
        if (arrows > 0 && isAiming) {
            arrows--;
            isAiming = false;
            attack -= 15;
            return true;
        }
        return false;
    }
    
    public void reloadArrows(int amount) {
        arrows = Math.min(maxArrows, arrows + amount);
    }
    
    @Override
    public void update() {
        super.update();
        
        if (arrows < maxArrows * 0.2) {
            // Автоматическая перезарядка при нехватке стрел
            reloadArrows(5);
        }
    }
    
    @Override
    public String getDescription() {
        String aimStatus = isAiming ? " [ПРИЦЕЛИЛСЯ]" : "";
        return super.getDescription() + " Стрелы: " + arrows + "/" + maxArrows + aimStatus;
    }
}
```

###### Класс `Mage` (Маг)
```java
public class Mage extends Unit {
    private int mana;
    private int maxMana;
    private List<String> spells;
    
    public Mage(String name, Position position) {
        super(name, position, 60, 30, 5, 2);
        this.maxMana = 100;
        this.mana = maxMana;
        this.spells = new ArrayList<>();
        spells.add("Огненный шар");
        spells.add("Ледяная стрела");
        spells.add("Молния");
    }
    
    @Override
    public void specialAbility() {
        if (mana >= 30) {
            mana -= 30;
            attack += 20;
            System.out.println(name + " усилил магию!");
        }
    }
    
    @Override
    public int getExperienceReward() {
        return 60;
    }
    
    public boolean castSpell(String spellName) {
        if (spells.contains(spellName) && mana >= 20) {
            mana -= 20;
            System.out.println(name + " произносит заклинание: " + spellName);
            return true;
        }
        return false;
    }
    
    public void meditate() {
        mana = Math.min(maxMana, mana + 25);
        System.out.println(name + " медитирует для восстановления маны.");
    }
    
    @Override
    public void update() {
        super.update();
        
        if (mana < maxMana * 0.3) {
            // Автоматическая медитация при нехватке маны
            meditate();
        }
    }
    
    @Override
    public String getDescription() {
        return super.getDescription() + " Мана: " + mana + "/" + maxMana + 
               " Заклинания: " + spells.size();
    }
}
```

#### 3. Иерархия зданий

##### Базовый класс `Building` (наследует от `GameEntity`)
```java
public abstract class Building extends GameEntity {
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
    
    // Абстрактные методы
    public abstract void produce();
    public abstract int getProductionRate();
    public abstract String getBuildingType();
    
    // Общие методы для всех зданий
    public boolean construct() {
        if (!isConstructed && constructionProgress >= constructionCost) {
            isConstructed = true;
            level = 1;
            return true;
        }
        return false;
    }
    
    public boolean upgrade() {
        if (isConstructed && level < maxLevel) {
            level++;
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

##### Производные классы зданий

###### Класс `ResourceBuilding` (Ресурсное здание)
```java
public class ResourceBuilding extends Building {
    private String resourceType;
    private int productionPerTurn;
    private int storageCapacity;
    
    public ResourceBuilding(String name, Position position, String resourceType, int productionPerTurn) {
        super(name, position, 100);
        this.resourceType = resourceType;
        this.productionPerTurn = productionPerTurn;
        this.storageCapacity = 1000;
    }
    
    @Override
    public void produce() {
        // Производство ресурсов каждый ход
        System.out.println(name + " производит " + productionPerTurn * level + " " + resourceType);
    }
    
    @Override
    public int getProductionRate() {
        return productionPerTurn * level;
    }
    
    @Override
    public String getBuildingType() {
        return "Ресурсное производство";
    }
    
    public String getResourceType() {
        return resourceType;
    }
    
    public int getStorageCapacity() {
        return storageCapacity;
    }
}
```

###### Класс `MilitaryBuilding` (Военное здание)
```java
public class MilitaryBuilding extends Building {
    private String unitType;
    private int trainingSpeed;
    private List<Unit> trainingQueue;
    
    public MilitaryBuilding(String name, Position position, String unitType) {
        super(name, position, 150);
        this.unitType = unitType;
        this.trainingSpeed = 1;
        this.trainingQueue = new ArrayList<>();
    }
    
    @Override
    public void produce() {
        // Обучение юнитов
        if (!trainingQueue.isEmpty()) {
            Unit unit = trainingQueue.remove(0);
            System.out.println(name + " завершил обучение " + unit.getName());
        }
    }
    
    @Override
    public int getProductionRate() {
        return trainingSpeed * level;
    }
    
    @Override
    public String getBuildingType() {
        return "Военное здание";
    }
    
    public void addToTrainingQueue(Unit unit) {
        trainingQueue.add(unit);
        System.out.println(unit.getName() + " добавлен в очередь обучения в " + name);
    }
    
    public String getUnitType() {
        return unitType;
    }
    
    public int getQueueSize() {
        return trainingQueue.size();
    }
}
```

#### 4. Иерархия ресурсов

##### Базовый класс `Resource`
```java
public abstract class Resource {
    protected String name;
    protected int amount;
    protected int maxAmount;
    protected boolean isRenewable;
    
    // Конструктор
    public Resource(String name, int initialAmount, int maxAmount, boolean isRenewable) {
        this.name = name;
        this.amount = Math.min(initialAmount, maxAmount);
        this.maxAmount = maxAmount;
        this.isRenewable = isRenewable;
    }
    
    // Абстрактные методы
    public abstract void process();
    public abstract int getValue();
    
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
               " [" + String.format("%.1f", getUsagePercentage()) + "%]";
    }
}
```

##### Производные классы ресурсов

###### Класс `BasicResource` (Базовый ресурс)
```java
public class BasicResource extends Resource {
    private int quality;
    
    public BasicResource(String name, int initialAmount, int maxAmount) {
        super(name, initialAmount, maxAmount, true);
        this.quality = 1;
    }
    
    @Override
    public void process() {
        // Базовые ресурсы могут восстанавливаться
        if (isRenewable && amount < maxAmount) {
            amount = Math.min(maxAmount, amount + 10);
        }
    }
    
    @Override
    public int getValue() {
        return amount * quality;
    }
    
    public int getQuality() {
        return quality;
    }
    
    public void improveQuality() {
        if (quality < 5) {
            quality++;
        }
    }
}
```

###### Класс `AdvancedResource` (Продвинутый ресурс)
```java
public class AdvancedResource extends Resource {
    private int complexity;
    private boolean isProcessed;
    
    public AdvancedResource(String name, int initialAmount, int maxAmount, int complexity) {
        super(name, initialAmount, maxAmount, false);
        this.complexity = complexity;
        this.isProcessed = false;
    }
    
    @Override
    public void process() {
        if (!isProcessed && amount > 0) {
            isProcessed = true;
            amount = amount / 2; // Обработка уменьшает количество, но увеличивает ценность
        }
    }
    
    @Override
    public int getValue() {
        int baseValue = amount * 10;
        if (isProcessed) baseValue *= 2;
        return baseValue * complexity;
    }
    
    public int getComplexity() {
        return complexity;
    }
    
    public boolean isProcessed() {
        return isProcessed;
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную иерархию классов:

### 1. **Гонки на выживание**
- Базовые классы: `Vehicle`, `TrackElement`, `GameObject`
- Производные: `Car`, `Motorcycle`, `Truck` (от `Vehicle`)
- Производные: `Obstacle`, `PowerUp`, `Checkpoint` (от `TrackElement`)

### 2. **Космическая колонизация**
- Базовые классы: `SpaceObject`, `Colonizable`, `Technology`
- Производные: `Planet`, `Asteroid`, `SpaceStation` (от `SpaceObject`)
- Производные: `Habitable`, `Mining`, `Research` (от `Colonizable`)

### 3. **Подземелье и драконы**
- Базовые классы: `Character`, `DungeonElement`, `Item`
- Производные: `Warrior`, `Mage`, `Rogue` (от `Character`)
- Производные: `Monster`, `Trap`, `Treasure` (от `DungeonElement`)

### 4. **Город-государство**
- Базовые классы: `CityElement`, `Infrastructure`, `Policy`
- Производные: `Residential`, `Commercial`, `Industrial` (от `CityElement`)
- Производные: `Road`, `Bridge`, `Tunnel` (от `Infrastructure`)

### 5. **Пиратская стратегия**
- Базовые классы: `MaritimeObject`, `CrewMember`, `Treasure`
- Производные: `Ship`, `Port`, `Island` (от `MaritimeObject`)
- Производные: `Captain`, `Navigator`, `Gunner` (от `CrewMember`)

### 6. **Фермерское хозяйство**
- Базовые классы: `FarmElement`, `LivingThing`, `Product`
- Производные: `Field`, `Barn`, `Greenhouse` (от `FarmElement`)
- Производные: `Crop`, `Animal`, `Tree` (от `LivingThing`)

### 7. **Киберпанк-тактика**
- Базовые классы: `CyberEntity`, `GridElement`, `Technology`
- Производные: `CyberUnit`, `Hacker`, `Netrunner` (от `CyberEntity`)
- Производные: `Node`, `Firewall`, `Virus` (от `GridElement`)

### 8. **Средневековая осада**
- Базовые классы: `CastleElement`, `SiegeEquipment`, `Defender`
- Производные: `Tower`, `Wall`, `Gate` (от `CastleElement`)
- Производные: `Catapult`, `BatteringRam`, `Trebuchet` (от `SiegeEquipment`)

### 9. **Зомби-выживание**
- Базовые классы: `SurvivalElement`, `Threat`, `Resource`
- Производные: `SafeZone`, `Shelter`, `Workshop` (от `SurvivalElement`)
- Производные: `Zombie`, `Infected`, `Mutant` (от `Threat`)

### 10. **Фэнтези-война**
- Базовые классы: `FantasyEntity`, `MagicElement`, `Artifact`
- Производные: `Creature`, `Hero`, `Villain` (от `FantasyEntity`)
- Производные: `Spell`, `Enchantment`, `Ritual` (от `MagicElement`)

## Требования к реализации

### Обязательные требования:
1. **Создать базовые абстрактные классы** для выбранной игры
2. **Реализовать иерархию наследования** минимум 3 уровня
3. **Использовать абстрактные методы** в базовых классах
4. **Переопределить методы** в производных классах
5. **Применить полиморфизм** в игровой логике
6. **Создать конструкторы** с вызовом `super()`

### Дополнительные требования:
1. **Реализовать интерфейсы** для общих поведений
2. **Добавить final методы** и классы где уместно
3. **Создать фабрики** для создания объектов
4. **Реализовать паттерн "Шаблонный метод"**

## Пример главного класса с полиморфизмом

```java
public class Game {
    private List<GameEntity> entities;
    private List<Unit> units;
    private List<Building> buildings;
    
    public Game() {
        this.entities = new ArrayList<>();
        this.units = new ArrayList<>();
        this.buildings = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание различных типов юнитов
        Position pos1 = new Position(1, 1);
        Position pos2 = new Position(2, 1);
        Position pos3 = new Position(3, 1);
        
        Unit warrior = new Warrior("Александр", pos1);
        Unit archer = new Archer("Леголас", pos2);
        Unit mage = new Mage("Гэндальф", pos3);
        
        units.add(warrior);
        units.add(archer);
        units.add(mage);
        
        entities.addAll(units);
        
        // Создание различных типов зданий
        Position buildPos1 = new Position(5, 5);
        Position buildPos2 = new Position(6, 5);
        
        Building mine = new ResourceBuilding("Шахта", buildPos1, "Железо", 50);
        Building barracks = new MilitaryBuilding("Казармы", buildPos2, "Воин");
        
        buildings.add(mine);
        buildings.add(barracks);
        
        entities.addAll(buildings);
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Полиморфное обновление всех сущностей
        for (GameEntity entity : entities) {
            entity.update();
        }
        
        // Полиморфное использование специальных способностей
        for (Unit unit : units) {
            if (unit.isAlive()) {
                unit.specialAbility();
            }
        }
        
        // Полиморфное производство в зданиях
        for (Building building : buildings) {
            if (building.isConstructed()) {
                building.produce();
            }
        }
        
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
1. **Наследования** - корректность создания объектов дочерних классов
2. **Полиморфизма** - вызов методов через ссылки на базовый класс
3. **Абстрактных методов** - корректность реализации в дочерних классах
4. **Переопределения** - правильность работы переопределенных методов

### Пример теста:
```java
@Test
public void testPolymorphism() {
    List<Unit> units = new ArrayList<>();
    units.add(new Warrior("Тест", new Position(0, 0)));
    units.add(new Archer("Тест", new Position(0, 0)));
    units.add(new Mage("Тест", new Position(0, 0)));
    
    for (Unit unit : units) {
        // Полиморфный вызов
        assertNotNull(unit.getDescription());
        unit.specialAbility();
        assertTrue(unit.getExperienceReward() > 0);
    }
}
```

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Наследование** | 4 | Корректная иерархия классов с наследованием |
| **Абстрактные классы** | 3 | Использование абстрактных классов и методов |
| **Полиморфизм** | 3 | Применение полиморфизма в игровой логике |
| **Переопределение** | 2 | Корректное переопределение методов |
| **Конструкторы** | 2 | Правильное использование super() |
| **Демонстрация** | 2 | Работающий пример с полиморфизмом |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 17**

## Вопросы для самопроверки

1. Что такое наследование и как оно реализуется в Java?
2. В чем разница между `extends` и `implements`?
3. Что такое полиморфизм и как он связан с наследованием?
4. Зачем нужны абстрактные классы и методы?
5. Как работает вызов `super()` в конструкторах?
6. Что такое переопределение методов и когда оно используется?
7. Как применить наследование для создания игровых сущностей?

## Дополнительные материалы

- [Java Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
- [Java Polymorphism](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
- [Java Abstract Classes](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)
- [Game Design Patterns](https://gameprogrammingpatterns.com/)

## Заключение

В данной лабораторной работе вы изучили принципы наследования и полиморфизма в Java на примере создания иерархии игровых сущностей. Вы научились:

- Создавать базовые и производные классы
- Использовать абстрактные классы и методы
- Применять полиморфизм в игровой логике
- Организовывать иерархию классов для игр

Полученные знания позволят вам создать сложную иерархию классов для собственной игры, используя принципы ООП для организации кода.
