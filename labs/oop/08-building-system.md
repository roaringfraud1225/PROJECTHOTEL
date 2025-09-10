# Лабораторная работа 8: Система зданий

## Цель работы
Изучить принципы создания системы зданий и строительства в Java на примере разработки игровых систем. Научиться управлять строительством, создавать производственные здания и реализовывать систему улучшений.

## Теоретические основы

### Система зданий
- **Типы зданий** - жилые, производственные, военные, специальные
- **Строительство** - требования, время, стоимость
- **Улучшения** - уровни, модификации, специализация
- **Взаимодействие** - связи между зданиями, бонусы

### Строительная система
- **Планирование** - размещение, ориентация, связи
- **Ресурсы** - материалы, рабочая сила, время
- **Эффективность** - производительность, бонусы, штрафы
- **Разрушение** - ремонт, снос, восстановление

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система зданий

#### 1. Базовый класс `Building`
```java
public abstract class Building extends GameEntity {
    protected String name;
    protected BuildingType type;
    protected int level;
    protected int maxLevel;
    protected Position position;
    protected int constructionTime;
    protected int constructionProgress;
    protected boolean isConstructed;
    protected boolean isOperational;
    protected Map<String, Integer> maintenanceCost;
    protected List<BuildingEffect> effects;
    
    public Building(String name, BuildingType type, Position position, int constructionTime) {
        this.name = name;
        this.type = type;
        this.position = position;
        this.constructionTime = constructionTime;
        this.level = 1;
        this.maxLevel = 5;
        this.constructionProgress = 0;
        this.isConstructed = false;
        this.isOperational = false;
        this.maintenanceCost = new HashMap<>();
        this.effects = new ArrayList<>();
    }
    
    public void updateConstruction() {
        if (!isConstructed) {
            constructionProgress++;
            if (constructionProgress >= constructionTime) {
                isConstructed = true;
                isOperational = true;
                onConstructionComplete();
            }
        }
    }
    
    public boolean upgrade() {
        if (level < maxLevel && isConstructed) {
            level++;
            onUpgrade();
            return true;
        }
        return false;
    }
    
    public void repair() {
        if (isConstructed && !isOperational) {
            isOperational = true;
            onRepair();
        }
    }
    
    public void demolish() {
        if (isConstructed) {
            onDemolish();
            isConstructed = false;
            isOperational = false;
        }
    }
    
    protected abstract void onConstructionComplete();
    protected abstract void onUpgrade();
    protected abstract void onRepair();
    protected abstract void onDemolish();
    
    public abstract void produce();
    public abstract boolean canProduce();
    
    public String getName() { return name; }
    public BuildingType getType() { return type; }
    public int getLevel() { return level; }
    public int getMaxLevel() { return maxLevel; }
    public Position getPosition() { return position; }
    public int getConstructionProgress() { return constructionProgress; }
    public int getConstructionTime() { return constructionTime; }
    public boolean isConstructed() { return isConstructed; }
    public boolean isOperational() { return isOperational; }
    public Map<String, Integer> getMaintenanceCost() { return maintenanceCost; }
    public List<BuildingEffect> getEffects() { return effects; }
    
    @Override
    public String toString() {
        String status = isConstructed ? 
            (isOperational ? "Работает" : "Требует ремонта") : 
            "Строится (" + constructionProgress + "/" + constructionTime + ")";
        return String.format("%s [Уровень %d] - %s", name, level, status);
    }
}
```

#### 2. Перечисление `BuildingType`
```java
public enum BuildingType {
    RESIDENTIAL("Жилое", 1.0),
    PRODUCTION("Производственное", 1.5),
    MILITARY("Военное", 2.0),
    SPECIAL("Специальное", 2.5),
    INFRASTRUCTURE("Инфраструктура", 1.2);
    
    private final String name;
    private final double costMultiplier;
    
    BuildingType(String name, double costMultiplier) {
        this.name = name;
        this.costMultiplier = costMultiplier;
    }
    
    public String getName() { return name; }
    public double getCostMultiplier() { return costMultiplier; }
}
```

#### 3. Класс `BuildingEffect` (Эффект здания)
```java
public class BuildingEffect {
    private String name;
    private EffectType type;
    private double value;
    private int radius;
    private boolean isActive;
    
    public BuildingEffect(String name, EffectType type, double value, int radius) {
        this.name = name;
        this.type = type;
        this.value = value;
        this.radius = radius;
        this.isActive = true;
    }
    
    public enum EffectType {
        PRODUCTION_BONUS("Бонус к производству"),
        COST_REDUCTION("Снижение стоимости"),
        QUALITY_IMPROVEMENT("Улучшение качества"),
        SPEED_BONUS("Бонус к скорости"),
        DEFENSE_BONUS("Бонус к защите");
        
        private final String description;
        
        EffectType(String description) {
            this.description = description;
        }
        
        public String getDescription() { return description; }
    }
    
    public String getName() { return name; }
    public EffectType getType() { return type; }
    public double getValue() { return value; }
    public int getRadius() { return radius; }
    public boolean isActive() { return isActive; }
    
    public void setActive(boolean active) { isActive = active; }
    
    @Override
    public String toString() {
        return String.format("%s: %s %.1f (радиус: %d)", 
                           name, type.getDescription(), value, radius);
    }
}
```

#### 4. Класс `ResidentialBuilding` (Жилое здание)
```java
public class ResidentialBuilding extends Building {
    private int populationCapacity;
    private int currentPopulation;
    private double happiness;
    private List<String> amenities;
    
    public ResidentialBuilding(String name, Position position, int capacity) {
        super(name, BuildingType.RESIDENTIAL, position, 5);
        this.populationCapacity = capacity;
        this.currentPopulation = 0;
        this.happiness = 1.0;
        this.amenities = new ArrayList<>();
        
        // Базовые расходы на содержание
        maintenanceCost.put("Золото", 10);
        maintenanceCost.put("Еда", 20);
    }
    
    @Override
    protected void onConstructionComplete() {
        System.out.println(name + " построено! Может разместить " + populationCapacity + " жителей");
    }
    
    @Override
    protected void onUpgrade() {
        populationCapacity += 10;
        happiness += 0.1;
        System.out.println(name + " улучшено до уровня " + level + 
                         ". Вместимость: " + populationCapacity + ", Счастье: " + happiness);
    }
    
    @Override
    protected void onRepair() {
        System.out.println(name + " отремонтировано");
    }
    
    @Override
    protected void onDemolish() {
        System.out.println(name + " снесено");
    }
    
    @Override
    public void produce() {
        if (canProduce()) {
            // Жилые здания производят счастье и налоги
            double taxIncome = currentPopulation * happiness * 0.1;
            // Здесь можно добавить логику получения налогов
        }
    }
    
    @Override
    public boolean canProduce() {
        return isOperational && currentPopulation > 0;
    }
    
    public boolean addResident() {
        if (currentPopulation < populationCapacity) {
            currentPopulation++;
            return true;
        }
        return false;
    }
    
    public boolean removeResident() {
        if (currentPopulation > 0) {
            currentPopulation--;
            return true;
        }
        return false;
    }
    
    public void addAmenity(String amenity) {
        if (!amenities.contains(amenity)) {
            amenities.add(amenity);
            happiness += 0.05;
        }
    }
    
    public int getPopulationCapacity() { return populationCapacity; }
    public int getCurrentPopulation() { return currentPopulation; }
    public double getHappiness() { return happiness; }
    public List<String> getAmenities() { return amenities; }
}
```

#### 5. Класс `ProductionBuilding` (Производственное здание)
```java
public class ProductionBuilding extends Building {
    private String productType;
    private int productionRate;
    private int currentProduction;
    private Map<String, Integer> inputResources;
    private Map<String, Integer> outputResources;
    private double efficiency;
    private List<BuildingEffect> productionEffects;
    
    public ProductionBuilding(String name, Position position, String productType, int productionRate) {
        super(name, BuildingType.PRODUCTION, position, 8);
        this.productType = productType;
        this.productionRate = productionRate;
        this.currentProduction = 0;
        this.inputResources = new HashMap<>();
        this.outputResources = new HashMap<>();
        this.efficiency = 1.0;
        this.productionEffects = new ArrayList<>();
        
        // Базовые расходы на содержание
        maintenanceCost.put("Золото", 15);
        maintenanceCost.put("Еда", 10);
    }
    
    @Override
    protected void onConstructionComplete() {
        System.out.println(name + " построено! Может производить " + productType);
    }
    
    @Override
    protected void onUpgrade() {
        productionRate += 2;
        efficiency += 0.1;
        System.out.println(name + " улучшено до уровня " + level + 
                         ". Производство: " + productionRate + ", Эффективность: " + efficiency);
    }
    
    @Override
    protected void onRepair() {
        System.out.println(name + " отремонтировано");
    }
    
    @Override
    protected void onDemolish() {
        System.out.println(name + " снесено");
    }
    
    @Override
    public void produce() {
        if (canProduce()) {
            currentProduction += productionRate * efficiency;
            
            // Применяем эффекты от соседних зданий
            for (BuildingEffect effect : productionEffects) {
                if (effect.isActive() && effect.getType() == BuildingEffect.EffectType.PRODUCTION_BONUS) {
                    currentProduction += effect.getValue();
                }
            }
            
            System.out.println(name + " произвел " + currentProduction + " " + productType);
        }
    }
    
    @Override
    public boolean canProduce() {
        return isOperational && hasRequiredResources();
    }
    
    private boolean hasRequiredResources() {
        // Проверяем наличие входных ресурсов
        for (Map.Entry<String, Integer> entry : inputResources.entrySet()) {
            // Здесь должна быть проверка через ResourceManager
            if (entry.getValue() > 0) return false;
        }
        return true;
    }
    
    public void addInputResource(String resourceName, int amount) {
        inputResources.put(resourceName, amount);
    }
    
    public void addOutputResource(String resourceName, int amount) {
        outputResources.put(resourceName, amount);
    }
    
    public void addProductionEffect(BuildingEffect effect) {
        productionEffects.add(effect);
    }
    
    public String getProductType() { return productType; }
    public int getProductionRate() { return productionRate; }
    public int getCurrentProduction() { return currentProduction; }
    public double getEfficiency() { return efficiency; }
    public Map<String, Integer> getInputResources() { return inputResources; }
    public Map<String, Integer> getOutputResources() { return outputResources; }
}
```

#### 6. Класс `MilitaryBuilding` (Военное здание)
```java
public class MilitaryBuilding extends Building {
    private int defenseValue;
    private int attackValue;
    private int garrisonCapacity;
    private List<Unit> garrison;
    private boolean isUnderSiege;
    private double morale;
    
    public MilitaryBuilding(String name, Position position, int defenseValue, int attackValue) {
        super(name, BuildingType.MILITARY, position, 10);
        this.defenseValue = defenseValue;
        this.attackValue = attackValue;
        this.garrisonCapacity = 10;
        this.garrison = new ArrayList<>();
        this.isUnderSiege = false;
        this.morale = 1.0;
        
        // Базовые расходы на содержание
        maintenanceCost.put("Золото", 25);
        maintenanceCost.put("Еда", 15);
    }
    
    @Override
    protected void onConstructionComplete() {
        System.out.println(name + " построено! Защита: " + defenseValue + ", Атака: " + attackValue);
    }
    
    @Override
    protected void onUpgrade() {
        defenseValue += 5;
        attackValue += 3;
        garrisonCapacity += 2;
        System.out.println(name + " улучшено до уровня " + level + 
                         ". Защита: " + defenseValue + ", Атака: " + attackValue);
    }
    
    @Override
    protected void onRepair() {
        System.out.println(name + " отремонтировано");
    }
    
    @Override
    protected void onDemolish() {
        System.out.println(name + " снесено");
    }
    
    @Override
    public void produce() {
        if (canProduce()) {
            // Военные здания производят опыт и улучшают мораль
            morale = Math.min(1.5, morale + 0.01);
        }
    }
    
    @Override
    public boolean canProduce() {
        return isOperational && !isUnderSiege;
    }
    
    public boolean addUnitToGarrison(Unit unit) {
        if (garrison.size() < garrisonCapacity) {
            garrison.add(unit);
            return true;
        }
        return false;
    }
    
    public boolean removeUnitFromGarrison(Unit unit) {
        return garrison.remove(unit);
    }
    
    public void startSiege() {
        isUnderSiege = true;
        morale -= 0.1;
        System.out.println(name + " находится в осаде!");
    }
    
    public void endSiege() {
        isUnderSiege = false;
        System.out.println(name + " осада снята!");
    }
    
    public int getDefenseValue() { return defenseValue; }
    public int getAttackValue() { return attackValue; }
    public int getGarrisonCapacity() { return garrisonCapacity; }
    public List<Unit> getGarrison() { return garrison; }
    public boolean isUnderSiege() { return isUnderSiege; }
    public double getMorale() { return morale; }
}
```

#### 7. Класс `BuildingManager` (Менеджер зданий)
```java
public class BuildingManager {
    private List<Building> buildings;
    private Map<Position, Building> buildingPositions;
    private List<BuildingEffect> globalEffects;
    private ResourceManager resourceManager;
    
    public BuildingManager(ResourceManager resourceManager) {
        this.buildings = new ArrayList<>();
        this.buildingPositions = new HashMap<>();
        this.globalEffects = new ArrayList<>();
        this.resourceManager = resourceManager;
    }
    
    public boolean canBuildAt(Position position) {
        return !buildingPositions.containsKey(position);
    }
    
    public boolean build(Building building) {
        if (canBuildAt(building.getPosition())) {
            buildings.add(building);
            buildingPositions.put(building.getPosition(), building);
            
            // Применяем эффекты здания к соседним зданиям
            applyBuildingEffects(building);
            
            return true;
        }
        return false;
    }
    
    public boolean demolish(Position position) {
        Building building = buildingPositions.get(position);
        if (building != null) {
            // Убираем эффекты здания
            removeBuildingEffects(building);
            
            buildings.remove(building);
            buildingPositions.remove(position);
            building.demolish();
            return true;
        }
        return false;
    }
    
    public void updateAllBuildings() {
        for (Building building : buildings) {
            building.updateConstruction();
            if (building.isOperational()) {
                building.produce();
            }
        }
        
        // Обновляем глобальные эффекты
        updateGlobalEffects();
    }
    
    private void applyBuildingEffects(Building building) {
        for (BuildingEffect effect : building.getEffects()) {
            if (effect.getRadius() > 0) {
                // Применяем эффект к зданиям в радиусе
                for (Building nearbyBuilding : buildings) {
                    if (nearbyBuilding != building && 
                        isInRadius(building.getPosition(), nearbyBuilding.getPosition(), effect.getRadius())) {
                        
                        if (nearbyBuilding instanceof ProductionBuilding) {
                            ((ProductionBuilding) nearbyBuilding).addProductionEffect(effect);
                        }
                    }
                }
            }
        }
    }
    
    private void removeBuildingEffects(Building building) {
        // Убираем эффекты здания при сносе
        for (Building nearbyBuilding : buildings) {
            if (nearbyBuilding instanceof ProductionBuilding) {
                ProductionBuilding prodBuilding = (ProductionBuilding) nearbyBuilding;
                prodBuilding.getProductionEffects().removeIf(effect -> 
                    building.getEffects().contains(effect));
            }
        }
    }
    
    private boolean isInRadius(Position pos1, Position pos2, int radius) {
        int dx = Math.abs(pos1.getX() - pos2.getX());
        int dy = Math.abs(pos1.getY() - pos2.getY());
        return dx <= radius && dy <= radius;
    }
    
    private void updateGlobalEffects() {
        // Обновляем глобальные эффекты от всех зданий
        globalEffects.clear();
        for (Building building : buildings) {
            if (building.isOperational()) {
                globalEffects.addAll(building.getEffects());
            }
        }
    }
    
    public List<Building> getBuildings() { return buildings; }
    public Building getBuildingAt(Position position) { return buildingPositions.get(position); }
    public List<BuildingEffect> getGlobalEffects() { return globalEffects; }
    public boolean hasBuildingAt(Position position) { return buildingPositions.containsKey(position); }
}
```

#### 8. Класс `ConstructionSite` (Строительная площадка)
```java
public class ConstructionSite {
    private Building building;
    private List<Worker> workers;
    private int requiredWorkers;
    private double constructionSpeed;
    private boolean isActive;
    
    public ConstructionSite(Building building, int requiredWorkers) {
        this.building = building;
        this.requiredWorkers = requiredWorkers;
        this.workers = new ArrayList<>();
        this.constructionSpeed = 1.0;
        this.isActive = true;
    }
    
    public boolean addWorker(Worker worker) {
        if (workers.size() < requiredWorkers) {
            workers.add(worker);
            updateConstructionSpeed();
            return true;
        }
        return false;
    }
    
    public boolean removeWorker(Worker worker) {
        if (workers.remove(worker)) {
            updateConstructionSpeed();
            return true;
        }
        return false;
    }
    
    private void updateConstructionSpeed() {
        if (workers.size() >= requiredWorkers) {
            constructionSpeed = 1.0;
        } else {
            constructionSpeed = (double) workers.size() / requiredWorkers;
        }
    }
    
    public void update() {
        if (isActive && workers.size() > 0) {
            int progress = (int)(workers.size() * constructionSpeed);
            // Здесь должна быть логика обновления прогресса строительства
        }
    }
    
    public void pause() { isActive = false; }
    public void resume() { isActive = true; }
    public void cancel() { isActive = false; }
    
    public Building getBuilding() { return building; }
    public List<Worker> getWorkers() { return workers; }
    public int getRequiredWorkers() { return requiredWorkers; }
    public double getConstructionSpeed() { return constructionSpeed; }
    public boolean isActive() { return isActive; }
}
```

#### 9. Главный класс с демонстрацией системы зданий
```java
public class Game {
    private BuildingManager buildingManager;
    private ResourceManager resourceManager;
    private List<Building> gameBuildings;
    
    public Game() {
        this.resourceManager = new ResourceManager();
        this.buildingManager = new BuildingManager(resourceManager);
        this.gameBuildings = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация системы зданий...");
        
        // Создание зданий
        Position townCenter = new Position(5, 5);
        Position farmPosition = new Position(6, 5);
        Position barracksPosition = new Position(4, 5);
        Position housePosition = new Position(5, 6);
        
        // Жилое здание
        ResidentialBuilding house = new ResidentialBuilding("Дом", housePosition, 20);
        house.addAmenity("Сад");
        house.addAmenity("Колодец");
        
        // Производственное здание
        ProductionBuilding farm = new ProductionBuilding("Ферма", farmPosition, "Еда", 10);
        farm.addInputResource("Семена", 5);
        farm.addOutputResource("Еда", 15);
        
        // Военное здание
        MilitaryBuilding barracks = new MilitaryBuilding("Казармы", barracksPosition, 15, 8);
        
        // Добавление зданий в игру
        gameBuildings.add(house);
        gameBuildings.add(farm);
        gameBuildings.add(barracks);
        
        // Строительство зданий
        for (Building building : gameBuildings) {
            if (buildingManager.build(building)) {
                System.out.println("Начато строительство: " + building.getName());
            }
        }
        
        System.out.println("Система зданий инициализирована");
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Обновление всех зданий
        buildingManager.updateAllBuildings();
        
        // Демонстрация работы зданий
        demonstrateBuildingOperations();
        
        // Демонстрация улучшений
        demonstrateUpgrades();
        
        displayGameState();
    }
    
    private void demonstrateBuildingOperations() {
        System.out.println("Демонстрация работы зданий...");
        
        for (Building building : gameBuildings) {
            if (building.isOperational()) {
                System.out.println(building.getName() + " работает");
                if (building instanceof ResidentialBuilding) {
                    ResidentialBuilding house = (ResidentialBuilding) building;
                    house.addResident();
                    System.out.println("Добавлен житель в " + house.getName() + 
                                     ". Население: " + house.getCurrentPopulation());
                }
            } else if (building.isConstructed()) {
                System.out.println(building.getName() + " требует ремонта");
                building.repair();
            } else {
                System.out.println(building.getName() + " строится...");
            }
        }
    }
    
    private void demonstrateUpgrades() {
        System.out.println("Демонстрация улучшений...");
        
        for (Building building : gameBuildings) {
            if (building.isOperational() && building.getLevel() < building.getMaxLevel()) {
                System.out.println("Улучшение " + building.getName() + "...");
                if (building.upgrade()) {
                    System.out.println(building.getName() + " улучшено до уровня " + building.getLevel());
                }
            }
        }
    }
    
    private void displayGameState() {
        System.out.println("\n=== Состояние игры ===");
        
        System.out.println("Здания:");
        for (Building building : gameBuildings) {
            System.out.println("  " + building);
        }
        
        System.out.println("Глобальные эффекты:");
        List<BuildingEffect> globalEffects = buildingManager.getGlobalEffects();
        if (globalEffects.isEmpty()) {
            System.out.println("  Нет активных эффектов");
        } else {
            globalEffects.forEach(effect -> System.out.println("  " + effect));
        }
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        
        // Играем несколько ходов
        for (int i = 0; i < 5; i++) {
            game.playTurn();
            System.out.println();
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему зданий:

### 1. **Гонки на выживание**
- Здания: гаражи, мастерские, заправки, отели
- Строительство: ремонтные базы, трассы, тоннели
- Взаимодействие: логистические цепочки

### 2. **Космическая колонизация**
- Здания: жилые модули, фабрики, лаборатории, доки
- Строительство: космические станции, колонии
- Взаимодействие: системы жизнеобеспечения

### 3. **Подземелье и драконы**
- Здания: таверны, кузницы, храмы, библиотеки
- Строительство: подземные комплексы, башни
- Взаимодействие: магические связи

### 4. **Город-государство**
- Здания: ратуши, рынки, школы, больницы
- Строительство: городские стены, мосты
- Взаимодействие: городская инфраструктура

### 5. **Пиратская стратегия**
- Здания: верфи, склады, таверны, укрепления
- Строительство: порты, форты, маяки
- Взаимодействие: морские пути

### 6. **Фермерское хозяйство**
- Здания: амбары, курятники, теплицы, мельницы
- Строительство: ирригационные системы, дороги
- Взаимодействие: сельскохозяйственные циклы

### 7. **Киберпанк-тактика**
- Здания: серверные, лаборатории, жилые блоки, рынки
- Строительство: небоскребы, подземные комплексы
- Взаимодействие: цифровые сети

### 8. **Средневековая осада**
- Здания: башни, стены, казармы, склады
- Строительство: осадные орудия, укрепления
- Взаимодействие: оборонительные линии

### 9. **Зомби-выживание**
- Здания: убежища, мастерские, оружейные, больницы
- Строительство: баррикады, ловушки, укрытия
- Взаимодействие: системы безопасности

### 10. **Фэнтези-война**
- Здания: замки, башни магов, кузницы, храмы
- Строительство: магические порталы, укрепления
- Взаимодействие: магические связи

## Требования к реализации

### Обязательные требования:
1. **Создать иерархию зданий** с различными типами
2. **Реализовать систему строительства** с прогрессом
3. **Создать менеджер зданий** для управления
4. **Реализовать систему улучшений** зданий
5. **Демонстрировать работу** всех систем
6. **Создать различные типы зданий** с уникальными свойствами

### Дополнительные требования:
1. **Реализовать эффекты зданий** на соседние постройки
2. **Добавить систему строительных площадок** с рабочими
3. **Создать планировщик строительства** с очередями
4. **Реализовать разрушение и восстановление** зданий

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Иерархия зданий** | 4 | Создание различных типов зданий |
| **Система строительства** | 3 | Прогресс строительства и требования |
| **Менеджер зданий** | 3 | Управление зданиями и их взаимодействием |
| **Система улучшений** | 3 | Улучшение зданий и их свойств |
| **Типы зданий** | 2 | Уникальные свойства различных зданий |
| **Демонстрация** | 2 | Работающий пример системы зданий |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Как организовать иерархию зданий?
2. Зачем нужна система строительства?
3. Как реализовать эффекты между зданиями?
4. Что такое строительные площадки?
5. Как управлять улучшениями зданий?
6. Как создать планировщик строительства?
7. Как применить систему зданий для игр?

## Заключение

В данной лабораторной работе вы изучили принципы создания системы зданий и строительства в Java на примере создания игровых систем. Вы научились:

- Создавать различные типы зданий
- Управлять строительством и улучшениями
- Реализовывать взаимодействие между зданиями
- Организовывать строительную систему

Полученные знания позволят вам создавать сложные системы строительства для игр.
