# Лабораторная работа 7: Система ресурсов

## Цель работы
Изучить принципы создания системы ресурсов и экономики в Java на примере разработки игровых систем. Научиться управлять ресурсами, создавать производственные цепочки и реализовывать торговые системы.

## Теоретические основы

### Система ресурсов
- **Типы ресурсов** - базовые, продвинутые, специальные
- **Производство** - создание ресурсов из других ресурсов
- **Потребление** - использование ресурсов для различных целей
- **Хранение** - управление емкостью и переполнением

### Экономика игры
- **Стоимость** - динамическое ценообразование
- **Торговля** - обмен ресурсов между игроками
- **Спрос и предложение** - рыночные механизмы
- **Инфляция** - изменение цен со временем

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Система ресурсов

#### 1. Базовый класс `Resource`
```java
public abstract class Resource {
    protected String name;
    protected int amount;
    protected int maxAmount;
    protected boolean isRenewable;
    protected double baseValue;
    protected ResourceType type;
    
    public Resource(String name, int initialAmount, int maxAmount, boolean isRenewable, double baseValue, ResourceType type) {
        this.name = name;
        this.amount = Math.min(initialAmount, maxAmount);
        this.maxAmount = maxAmount;
        this.isRenewable = isRenewable;
        this.baseValue = baseValue;
        this.type = type;
    }
    
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
    
    public double getCurrentValue() {
        return amount * baseValue;
    }
    
    public double getUsagePercentage() {
        return (double) amount / maxAmount * 100;
    }
    
    public boolean isFull() { return amount >= maxAmount; }
    public boolean isEmpty() { return amount <= 0; }
    public String getName() { return name; }
    public int getAmount() { return amount; }
    public int getMaxAmount() { return maxAmount; }
    public boolean isRenewable() { return isRenewable; }
    public double getBaseValue() { return baseValue; }
    public ResourceType getType() { return type; }
    
    @Override
    public String toString() {
        return String.format("%s: %d/%d (%.1f%%) [Ценность: %.1f]", 
                           name, amount, maxAmount, getUsagePercentage(), getCurrentValue());
    }
}
```

#### 2. Перечисление `ResourceType`
```java
public enum ResourceType {
    BASIC("Базовый", 1.0),
    ADVANCED("Продвинутый", 2.0),
    LUXURY("Роскошь", 5.0),
    STRATEGIC("Стратегический", 3.0),
    MAGICAL("Магический", 4.0);
    
    private final String name;
    private final double valueMultiplier;
    
    ResourceType(String name, double valueMultiplier) {
        this.name = name;
        this.valueMultiplier = valueMultiplier;
    }
    
    public String getName() { return name; }
    public double getValueMultiplier() { return valueMultiplier; }
}
```

#### 3. Конкретные классы ресурсов

##### `BasicResource` (Базовый ресурс)
```java
public class BasicResource extends Resource {
    private int quality;
    private boolean isProcessed;
    
    public BasicResource(String name, int initialAmount, int maxAmount) {
        super(name, initialAmount, maxAmount, true, 1.0, ResourceType.BASIC);
        this.quality = 1;
        this.isProcessed = false;
    }
    
    public void improveQuality() {
        if (quality < 5) {
            quality++;
            baseValue *= 1.2;
        }
    }
    
    public void process() {
        if (!isProcessed && amount > 0) {
            isProcessed = true;
            baseValue *= 1.5;
        }
    }
    
    @Override
    public double getCurrentValue() {
        return amount * baseValue * quality;
    }
    
    public int getQuality() { return quality; }
    public boolean isProcessed() { return isProcessed; }
}
```

##### `AdvancedResource` (Продвинутый ресурс)
```java
public class AdvancedResource extends Resource {
    private List<Resource> components;
    private int complexity;
    
    public AdvancedResource(String name, int initialAmount, int maxAmount, int complexity) {
        super(name, initialAmount, maxAmount, false, 5.0, ResourceType.ADVANCED);
        this.components = new ArrayList<>();
        this.complexity = complexity;
    }
    
    public void addComponent(Resource component) {
        components.add(component);
        recalculateValue();
    }
    
    private void recalculateValue() {
        double componentValue = components.stream()
                .mapToDouble(Resource::getCurrentValue)
                .sum();
        baseValue = componentValue * 0.8 + complexity * 2.0;
    }
    
    public List<Resource> getComponents() { return components; }
    public int getComplexity() { return complexity; }
}
```

#### 4. Класс `ResourceManager` (Менеджер ресурсов)
```java
public class ResourceManager {
    private Map<String, Resource> resources;
    private List<ResourceTransaction> transactionHistory;
    private Map<String, Double> marketPrices;
    
    public ResourceManager() {
        this.resources = new HashMap<>();
        this.transactionHistory = new ArrayList<>();
        this.marketPrices = new HashMap<>();
    }
    
    public void addResource(Resource resource) {
        resources.put(resource.getName(), resource);
        marketPrices.put(resource.getName(), resource.getBaseValue());
    }
    
    public boolean consumeResource(String name, int amount) {
        Resource resource = resources.get(name);
        if (resource != null && resource.consumeAmount(amount)) {
            recordTransaction(name, -amount, "Потребление");
            updateMarketPrice(name);
            return true;
        }
        return false;
    }
    
    public boolean produceResource(String name, int amount) {
        Resource resource = resources.get(name);
        if (resource != null && resource.addAmount(amount)) {
            recordTransaction(name, amount, "Производство");
            updateMarketPrice(name);
            return true;
        }
        return false;
    }
    
    public boolean tradeResource(String fromName, String toName, int amount) {
        if (consumeResource(fromName, amount)) {
            produceResource(toName, amount);
            recordTransaction(fromName, -amount, "Торговля");
            recordTransaction(toName, amount, "Торговля");
            return true;
        }
        return false;
    }
    
    private void recordTransaction(String resourceName, int amount, String type) {
        transactionHistory.add(new ResourceTransaction(resourceName, amount, type));
    }
    
    private void updateMarketPrice(String resourceName) {
        Resource resource = resources.get(resourceName);
        if (resource != null) {
            double currentPrice = marketPrices.get(resourceName);
            double newPrice = currentPrice * (1 + (Math.random() - 0.5) * 0.1);
            marketPrices.put(resourceName, Math.max(0.1, newPrice));
        }
    }
    
    public double getResourceValue(String name) {
        Resource resource = resources.get(name);
        return resource != null ? resource.getCurrentValue() : 0;
    }
    
    public double getMarketPrice(String name) {
        return marketPrices.getOrDefault(name, 0.0);
    }
    
    public List<ResourceTransaction> getTransactionHistory() {
        return new ArrayList<>(transactionHistory);
    }
    
    public Map<String, Resource> getResources() { return resources; }
    public Map<String, Double> getMarketPrices() { return marketPrices; }
}
```

#### 5. Класс `ResourceTransaction` (Транзакция ресурсов)
```java
public class ResourceTransaction {
    private final String resourceName;
    private final int amount;
    private final String type;
    private final LocalDateTime timestamp;
    
    public ResourceTransaction(String resourceName, int amount, String type) {
        this.resourceName = resourceName;
        this.amount = amount;
        this.type = type;
        this.timestamp = LocalDateTime.now();
    }
    
    public String getResourceName() { return resourceName; }
    public int getAmount() { return amount; }
    public String getType() { return type; }
    public LocalDateTime getTimestamp() { return timestamp; }
    
    @Override
    public String toString() {
        return String.format("[%s] %s: %s %d %s", 
                           timestamp.format(DateTimeFormatter.ofPattern("HH:mm:ss")),
                           type, resourceName, amount, amount > 0 ? "получено" : "потрачено");
    }
}
```

#### 6. Класс `ProductionChain` (Производственная цепочка)
```java
public class ProductionChain {
    private String name;
    private Map<String, Integer> inputResources;
    private Map<String, Integer> outputResources;
    private int productionTime;
    private double efficiency;
    
    public ProductionChain(String name, int productionTime) {
        this.name = name;
        this.inputResources = new HashMap<>();
        this.outputResources = new HashMap<>();
        this.productionTime = productionTime;
        this.efficiency = 1.0;
    }
    
    public void addInputResource(String resourceName, int amount) {
        inputResources.put(resourceName, amount);
    }
    
    public void addOutputResource(String resourceName, int amount) {
        outputResources.put(resourceName, amount);
    }
    
    public boolean canProduce(ResourceManager resourceManager) {
        for (Map.Entry<String, Integer> entry : inputResources.entrySet()) {
            Resource resource = resourceManager.getResources().get(entry.getKey());
            if (resource == null || resource.getAmount() < entry.getValue()) {
                return false;
            }
        }
        return true;
    }
    
    public boolean produce(ResourceManager resourceManager) {
        if (!canProduce(resourceManager)) return false;
        
        // Потребляем входные ресурсы
        for (Map.Entry<String, Integer> entry : inputResources.entrySet()) {
            resourceManager.consumeResource(entry.getKey(), entry.getValue());
        }
        
        // Производим выходные ресурсы
        for (Map.Entry<String, Integer> entry : outputResources.entrySet()) {
            int amount = (int)(entry.getValue() * efficiency);
            resourceManager.produceResource(entry.getKey(), amount);
        }
        
        return true;
    }
    
    public void improveEfficiency() {
        efficiency = Math.min(2.0, efficiency + 0.1);
    }
    
    public String getName() { return name; }
    public int getProductionTime() { return productionTime; }
    public double getEfficiency() { return efficiency; }
    public Map<String, Integer> getInputResources() { return inputResources; }
    public Map<String, Integer> getOutputResources() { return outputResources; }
}
```

#### 7. Класс `TradeSystem` (Торговая система)
```java
public class TradeSystem {
    private Map<String, Double> exchangeRates;
    private List<TradeOffer> offers;
    private ResourceManager resourceManager;
    
    public TradeSystem(ResourceManager resourceManager) {
        this.exchangeRates = new HashMap<>();
        this.offers = new ArrayList<>();
        this.resourceManager = resourceManager;
    }
    
    public void setExchangeRate(String fromResource, String toResource, double rate) {
        String key = fromResource + "_" + toResource;
        exchangeRates.put(key, rate);
    }
    
    public double getExchangeRate(String fromResource, String toResource) {
        String key = fromResource + "_" + toResource;
        return exchangeRates.getOrDefault(key, 1.0);
    }
    
    public boolean exchange(String fromResource, String toResource, int amount) {
        double rate = getExchangeRate(fromResource, toResource);
        int convertedAmount = (int)(amount * rate);
        
        if (resourceManager.consumeResource(fromResource, amount)) {
            resourceManager.produceResource(toResource, convertedAmount);
            return true;
        }
        return false;
    }
    
    public void addTradeOffer(String resourceName, int amount, double price, String type) {
        offers.add(new TradeOffer(resourceName, amount, price, type));
    }
    
    public List<TradeOffer> getOffers(String resourceName) {
        return offers.stream()
                .filter(offer -> offer.getResourceName().equals(resourceName))
                .collect(Collectors.toList());
    }
    
    public boolean acceptTradeOffer(TradeOffer offer) {
        if (offer.getType().equals("Покупка")) {
            return resourceManager.consumeResource("Золото", (int)offer.getPrice()) &&
                   resourceManager.produceResource(offer.getResourceName(), offer.getAmount());
        } else {
            return resourceManager.consumeResource(offer.getResourceName(), offer.getAmount()) &&
                   resourceManager.produceResource("Золото", (int)offer.getPrice());
        }
    }
    
    public Map<String, Double> getExchangeRates() { return exchangeRates; }
    public List<TradeOffer> getAllOffers() { return offers; }
}
```

#### 8. Класс `TradeOffer` (Торговое предложение)
```java
public class TradeOffer {
    private final String resourceName;
    private final int amount;
    private final double price;
    private final String type; // "Покупка" или "Продажа"
    private final LocalDateTime timestamp;
    
    public TradeOffer(String resourceName, int amount, double price, String type) {
        this.resourceName = resourceName;
        this.amount = amount;
        this.price = price;
        this.type = type;
        this.timestamp = LocalDateTime.now();
    }
    
    public String getResourceName() { return resourceName; }
    public int getAmount() { return amount; }
    public double getPrice() { return price; }
    public String getType() { return type; }
    public LocalDateTime getTimestamp() { return timestamp; }
    
    @Override
    public String toString() {
        return String.format("%s %d %s за %.1f золота [%s]", 
                           type, amount, resourceName, price, 
                           timestamp.format(DateTimeFormatter.ofPattern("HH:mm:ss")));
    }
}
```

#### 9. Главный класс с демонстрацией системы ресурсов
```java
public class Game {
    private ResourceManager resourceManager;
    private ProductionChain productionChain;
    private TradeSystem tradeSystem;
    private List<Resource> gameResources;
    
    public Game() {
        this.resourceManager = new ResourceManager();
        this.tradeSystem = new TradeSystem(resourceManager);
        this.gameResources = new ArrayList<>();
        
        initializeGame();
    }
    
    private void initializeGame() {
        // Создание ресурсов
        BasicResource wood = new BasicResource("Дерево", 100, 1000);
        BasicResource stone = new BasicResource("Камень", 50, 500);
        BasicResource iron = new BasicResource("Железо", 25, 250);
        BasicResource gold = new BasicResource("Золото", 1000, 10000);
        
        gameResources.add(wood);
        gameResources.add(stone);
        gameResources.add(iron);
        gameResources.add(gold);
        
        // Добавление в менеджер
        for (Resource resource : gameResources) {
            resourceManager.addResource(resource);
        }
        
        // Создание производственной цепочки
        productionChain = new ProductionChain("Строительство", 3);
        productionChain.addInputResource("Дерево", 10);
        productionChain.addInputResource("Камень", 5);
        productionChain.addOutputResource("Строительные материалы", 1);
        
        // Настройка торговых курсов
        tradeSystem.setExchangeRate("Дерево", "Золото", 0.1);
        tradeSystem.setExchangeRate("Камень", "Золото", 0.2);
        tradeSystem.setExchangeRate("Железо", "Золото", 0.5);
        
        System.out.println("Игра инициализирована");
    }
    
    public void playTurn() {
        System.out.println("=== Ход игры ===");
        
        // Демонстрация производства
        demonstrateProduction();
        
        // Демонстрация торговли
        demonstrateTrading();
        
        // Демонстрация управления ресурсами
        demonstrateResourceManagement();
        
        displayGameState();
    }
    
    private void demonstrateProduction() {
        System.out.println("Попытка производства строительных материалов...");
        
        if (productionChain.canProduce(resourceManager)) {
            productionChain.produce(resourceManager);
            System.out.println("Производство успешно!");
        } else {
            System.out.println("Недостаточно ресурсов для производства");
        }
    }
    
    private void demonstrateTrading() {
        System.out.println("Демонстрация торговли...");
        
        // Обмен дерева на золото
        int woodAmount = 20;
        double exchangeRate = tradeSystem.getExchangeRate("Дерево", "Золото");
        int goldAmount = (int)(woodAmount * exchangeRate);
        
        System.out.printf("Обмен %d дерева на %d золота (курс: %.2f)%n", 
                         woodAmount, goldAmount, exchangeRate);
        
        if (tradeSystem.exchange("Дерево", "Золото", woodAmount)) {
            System.out.println("Обмен успешен!");
        } else {
            System.out.println("Обмен не удался");
        }
    }
    
    private void demonstrateResourceManagement() {
        System.out.println("Демонстрация управления ресурсами...");
        
        // Улучшение качества дерева
        BasicResource wood = (BasicResource) gameResources.get(0);
        System.out.println("Дерево до улучшения: " + wood);
        wood.improveQuality();
        System.out.println("Дерево после улучшения: " + wood);
        
        // Обработка камня
        BasicResource stone = (BasicResource) gameResources.get(1);
        System.out.println("Камень до обработки: " + stone);
        stone.process();
        System.out.println("Камень после обработки: " + stone);
    }
    
    private void displayGameState() {
        System.out.println("\n=== Состояние игры ===");
        
        System.out.println("Ресурсы:");
        for (Resource resource : gameResources) {
            System.out.println("  " + resource);
        }
        
        System.out.println("Рыночные цены:");
        Map<String, Double> prices = resourceManager.getMarketPrices();
        prices.forEach((name, price) -> 
            System.out.println("  " + name + ": " + String.format("%.2f", price)));
        
        System.out.println("Последние транзакции:");
        List<ResourceTransaction> transactions = resourceManager.getTransactionHistory();
        transactions.stream()
                .limit(5)
                .forEach(t -> System.out.println("  " + t));
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

Студенты должны выбрать одну из следующих игр и реализовать аналогичную систему ресурсов:

### 1. **Гонки на выживание**
- Ресурсы: топливо, запчасти, очки выживания
- Производство: ремонт транспорта, улучшения
- Торговля: обмен ресурсов на трассах

### 2. **Космическая колонизация**
- Ресурсы: минералы, энергия, технологии
- Производство: космические корабли, колонии
- Торговля: межпланетная торговля

### 3. **Подземелье и драконы**
- Ресурсы: золото, опыт, магические предметы
- Производство: оружие, зелья, заклинания
- Торговля: торговля с NPC, аукционы

### 4. **Город-государство**
- Ресурсы: еда, материалы, население
- Производство: здания, инфраструктура
- Торговля: международная торговля

### 5. **Пиратская стратегия**
- Ресурсы: сокровища, припасы, команда
- Производство: корабли, оружие, укрепления
- Торговля: пиратские рынки, контрабанда

### 6. **Фермерское хозяйство**
- Ресурсы: семена, урожай, животные
- Производство: продукты питания, товары
- Торговля: сельскохозяйственные рынки

### 7. **Киберпанк-тактика**
- Ресурсы: данные, криптовалюта, импланты
- Производство: программы, хакерские инструменты
- Торговля: черный рынок, цифровые сделки

### 8. **Средневековая осада**
- Ресурсы: оружие, продовольствие, материалы
- Производство: осадные орудия, укрепления
- Торговля: военные поставки, выкуп

### 9. **Зомби-выживание**
- Ресурсы: еда, оружие, медикаменты
- Производство: убежища, ловушки, инструменты
- Торговля: бартер между выжившими

### 10. **Фэнтези-война**
- Ресурсы: мана, артефакты, души
- Производство: магические предметы, заклинания
- Торговля: магические рынки, алхимия

## Требования к реализации

### Обязательные требования:
1. **Создать иерархию ресурсов** с различными типами
2. **Реализовать менеджер ресурсов** с транзакциями
3. **Создать производственные цепочки** для создания ресурсов
4. **Реализовать торговую систему** с обменными курсами
5. **Демонстрировать работу** всех систем
6. **Создать различные типы ресурсов** с уникальными свойствами

### Дополнительные требования:
1. **Реализовать рыночные механизмы** спроса и предложения
2. **Добавить качество ресурсов** и их обработку
3. **Создать систему аукционов** для торговли
4. **Реализовать экономические циклы** и инфляцию

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Иерархия ресурсов** | 4 | Создание различных типов ресурсов |
| **Менеджер ресурсов** | 3 | Управление ресурсами и транзакциями |
| **Производственные цепочки** | 3 | Создание ресурсов из других ресурсов |
| **Торговая система** | 3 | Обмен ресурсов и торговля |
| **Типы ресурсов** | 2 | Уникальные свойства ресурсов |
| **Демонстрация** | 2 | Работающий пример системы ресурсов |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Как организовать иерархию ресурсов?
2. Зачем нужны производственные цепочки?
3. Как реализовать динамическое ценообразование?
4. Что такое рыночные механизмы в играх?
5. Как управлять качеством ресурсов?
6. Как создать справедливую торговую систему?
7. Как применить экономику для игровых систем?

## Заключение

В данной лабораторной работе вы изучили принципы создания системы ресурсов и экономики в Java на примере создания игровых систем. Вы научились:

- Создавать различные типы ресурсов
- Управлять производственными цепочками
- Реализовывать торговые системы
- Организовывать экономику игры

Полученные знания позволят вам создавать сложные экономические системы для игр.
