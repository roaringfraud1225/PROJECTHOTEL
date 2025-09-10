# Лабораторная работа 13: ИИ противник

## Цель работы
Изучить принципы создания искусственного интеллекта для игр в Java на примере разработки игровых систем. Научиться реализовывать различные алгоритмы ИИ, создавать поведенческие деревья и оптимизировать принятие решений.

## Теоретические основы

### Искусственный интеллект в играх
- **Поведенческие деревья** - структурированное принятие решений
- **Алгоритмы поиска** - навигация и планирование
- **Машинное обучение** - адаптация к игроку
- **Оптимизация** - баланс между качеством и производительностью

### Типы ИИ
- **Реактивный ИИ** - простые реакции на события
- **Планирующий ИИ** - долгосрочное планирование
- **Адаптивный ИИ** - обучение на основе опыта
- **Гибридный ИИ** - комбинация различных подходов

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### ИИ противника

#### 1. Базовый класс `AIEntity`
```java
public abstract class AIEntity extends GameEntity {
    protected AIController aiController;
    protected AIState currentState;
    protected AIStrategy currentStrategy;
    protected List<AIGoal> goals;
    protected Map<String, Object> memory;
    protected int decisionCooldown;
    protected long lastDecisionTime;
    
    public AIEntity(Position position, String name) {
        super(position, name);
        this.aiController = new AIController();
        this.currentState = AIState.IDLE;
        this.currentStrategy = AIStrategy.BALANCED;
        this.goals = new ArrayList<>();
        this.memory = new HashMap<>();
        this.decisionCooldown = 1000; // 1 секунда
        this.lastDecisionTime = 0;
        
        initializeGoals();
    }
    
    protected abstract void initializeGoals();
    protected abstract void updateBehavior();
    
    public void update() {
        long currentTime = System.currentTimeMillis();
        
        // Проверяем, можно ли принимать решение
        if (currentTime - lastDecisionTime >= decisionCooldown) {
            makeDecision();
            lastDecisionTime = currentTime;
        }
        
        // Обновляем поведение
        updateBehavior();
        
        // Обновляем цели
        updateGoals();
    }
    
    private void makeDecision() {
        // Анализируем текущую ситуацию
        GameSituation situation = analyzeSituation();
        
        // Выбираем стратегию
        AIStrategy newStrategy = selectStrategy(situation);
        if (newStrategy != currentStrategy) {
            changeStrategy(newStrategy);
        }
        
        // Планируем действия
        planActions(situation);
    }
    
    private GameSituation analyzeSituation() {
        GameSituation situation = new GameSituation();
        
        // Анализируем ресурсы
        situation.setResourceLevel(analyzeResources());
        
        // Анализируем военную силу
        situation.setMilitaryStrength(analyzeMilitaryStrength());
        
        // Анализируем угрозы
        situation.setThreatLevel(analyzeThreats());
        
        // Анализируем возможности
        situation.setOpportunityLevel(analyzeOpportunities());
        
        return situation;
    }
    
    private AIStrategy selectStrategy(GameSituation situation) {
        // Логика выбора стратегии на основе ситуации
        if (situation.getThreatLevel() > 0.7) {
            return AIStrategy.DEFENSIVE;
        } else if (situation.getMilitaryStrength() > 0.8 && situation.getOpportunityLevel() > 0.6) {
            return AIStrategy.AGGRESSIVE;
        } else if (situation.getResourceLevel() < 0.3) {
            return AIStrategy.ECONOMIC;
        } else {
            return AIStrategy.BALANCED;
        }
    }
    
    private void changeStrategy(AIStrategy newStrategy) {
        System.out.println(name + " меняет стратегию с " + currentStrategy + " на " + newStrategy);
        currentStrategy = newStrategy;
        
        // Обновляем цели в соответствии с новой стратегией
        updateGoalsForStrategy(newStrategy);
    }
    
    private void planActions(GameSituation situation) {
        // Очищаем текущие цели
        goals.clear();
        
        // Добавляем цели в зависимости от стратегии
        switch (currentStrategy) {
            case AGGRESSIVE:
                addAggressiveGoals(situation);
                break;
            case DEFENSIVE:
                addDefensiveGoals(situation);
                break;
            case ECONOMIC:
                addEconomicGoals(situation);
                break;
            case BALANCED:
                addBalancedGoals(situation);
                break;
        }
        
        // Сортируем цели по приоритету
        goals.sort(Comparator.comparingInt(AIGoal::getPriority).reversed());
    }
    
    private void addAggressiveGoals(GameSituation situation) {
        goals.add(new AIGoal("Атаковать врага", 10, () -> executeAttack()));
        goals.add(new AIGoal("Укреплять армию", 8, () -> strengthenArmy()));
        goals.add(new AIGoal("Разведывать территорию", 6, () -> scoutTerritory()));
    }
    
    private void addDefensiveGoals(GameSituation situation) {
        goals.add(new AIGoal("Строить укрепления", 10, () -> buildDefenses()));
        goals.add(new AIGoal("Укреплять армию", 8, () -> strengthenArmy()));
        goals.add(new AIGoal("Собирать ресурсы", 6, () -> gatherResources()));
    }
    
    private void addEconomicGoals(GameSituation situation) {
        goals.add(new AIGoal("Собирать ресурсы", 10, () -> gatherResources()));
        goals.add(new AIGoal("Строить экономические здания", 8, () -> buildEconomy()));
        goals.add(new AIGoal("Исследовать технологии", 6, () -> researchTechnologies()));
    }
    
    private void addBalancedGoals(GameSituation situation) {
        goals.add(new AIGoal("Собирать ресурсы", 8, () -> gatherResources()));
        goals.add(new AIGoal("Укреплять армию", 7, () -> strengthenArmy()));
        goals.add(new AIGoal("Строить здания", 6, () -> buildStructures()));
        goals.add(new AIGoal("Разведывать территорию", 5, () -> scoutTerritory()));
    }
    
    private void updateGoals() {
        // Выполняем цели с наивысшим приоритетом
        for (AIGoal goal : goals) {
            if (goal.canExecute()) {
                goal.execute();
                break; // Выполняем только одну цель за раз
            }
        }
    }
    
    // Абстрактные методы для анализа
    protected abstract double analyzeResources();
    protected abstract double analyzeMilitaryStrength();
    protected abstract double analyzeThreats();
    protected abstract double analyzeOpportunities();
    
    // Абстрактные методы для выполнения действий
    protected abstract void executeAttack();
    protected abstract void strengthenArmy();
    protected abstract void buildDefenses();
    protected abstract void gatherResources();
    protected abstract void buildEconomy();
    protected abstract void buildStructures();
    protected abstract void scoutTerritory();
    protected abstract void researchTechnologies();
    
    public AIState getCurrentState() { return currentState; }
    public AIStrategy getCurrentStrategy() { return currentStrategy; }
    public List<AIGoal> getGoals() { return goals; }
    public Map<String, Object> getMemory() { return memory; }
    
    public enum AIState {
        IDLE, MOVING, ATTACKING, DEFENDING, BUILDING, GATHERING
    }
    
    public enum AIStrategy {
        AGGRESSIVE, DEFENSIVE, ECONOMIC, BALANCED
    }
}
```

#### 2. Класс `AIGoal` (Цель ИИ)
```java
public class AIGoal {
    private String name;
    private int priority;
    private Runnable action;
    private boolean completed;
    private boolean failed;
    private long startTime;
    private long timeout;
    
    public AIGoal(String name, int priority, Runnable action) {
        this.name = name;
        this.priority = priority;
        this.action = action;
        this.completed = false;
        this.failed = false;
        this.startTime = System.currentTimeMillis();
        this.timeout = 30000; // 30 секунд по умолчанию
    }
    
    public boolean canExecute() {
        return !completed && !failed && !isTimedOut();
    }
    
    public void execute() {
        if (!canExecute()) {
            return;
        }
        
        try {
            System.out.println("ИИ выполняет цель: " + name);
            action.run();
            complete();
        } catch (Exception e) {
            System.err.println("Ошибка выполнения цели " + name + ": " + e.getMessage());
            fail();
        }
    }
    
    public void complete() {
        this.completed = true;
        System.out.println("Цель " + name + " выполнена");
    }
    
    public void fail() {
        this.failed = true;
        System.out.println("Цель " + name + " провалена");
    }
    
    public boolean isTimedOut() {
        return System.currentTimeMillis() - startTime > timeout;
    }
    
    public void setTimeout(long timeout) {
        this.timeout = timeout;
    }
    
    public String getName() { return name; }
    public int getPriority() { return priority; }
    public boolean isCompleted() { return completed; }
    public boolean isFailed() { return failed; }
    public long getStartTime() { return startTime; }
    public long getTimeout() { return timeout; }
}
```

#### 3. Класс `AIController` (Контроллер ИИ)
```java
public class AIController {
    private List<AIBehavior> behaviors;
    private AIBehavior currentBehavior;
    private Map<String, Object> blackboard;
    private DecisionTree decisionTree;
    
    public AIController() {
        this.behaviors = new ArrayList<>();
        this.blackboard = new HashMap<>();
        this.decisionTree = new DecisionTree();
        
        initializeBehaviors();
    }
    
    private void initializeBehaviors() {
        behaviors.add(new IdleBehavior());
        behaviors.add(new CombatBehavior());
        behaviors.add(new EconomicBehavior());
        behaviors.add(new ExplorationBehavior());
        
        currentBehavior = behaviors.get(0); // Начинаем с idle
    }
    
    public void update(AIEntity entity, GameWorld world) {
        // Обновляем черную доску
        updateBlackboard(entity, world);
        
        // Принимаем решение о смене поведения
        AIBehavior newBehavior = selectBehavior(entity, world);
        if (newBehavior != currentBehavior) {
            changeBehavior(newBehavior, entity);
        }
        
        // Обновляем текущее поведение
        currentBehavior.update(entity, world);
    }
    
    private void updateBlackboard(AIEntity entity, GameWorld world) {
        // Обновляем информацию о текущем состоянии
        blackboard.put("entity_health", entity.getCurrentHealth());
        blackboard.put("entity_position", entity.getPosition());
        blackboard.put("nearby_enemies", findNearbyEnemies(entity, world));
        blackboard.put("nearby_allies", findNearbyAllies(entity, world));
        blackboard.put("resource_level", calculateResourceLevel(entity));
        blackboard.put("threat_level", calculateThreatLevel(entity, world));
    }
    
    private AIBehavior selectBehavior(AIEntity entity, GameWorld world) {
        // Используем дерево решений для выбора поведения
        return decisionTree.selectBehavior(blackboard);
    }
    
    private void changeBehavior(AIBehavior newBehavior, AIEntity entity) {
        if (currentBehavior != null) {
            currentBehavior.onExit(entity);
        }
        
        currentBehavior = newBehavior;
        currentBehavior.onEnter(entity);
        
        System.out.println(entity.getName() + " меняет поведение на: " + newBehavior.getClass().getSimpleName());
    }
    
    private List<GameEntity> findNearbyEnemies(AIEntity entity, GameWorld world) {
        return world.getEntities().stream()
                .filter(e -> e != entity && e.getTeam() != entity.getTeam())
                .filter(e -> e.getPosition().distanceTo(entity.getPosition()) <= 5)
                .collect(Collectors.toList());
    }
    
    private List<GameEntity> findNearbyAllies(AIEntity entity, GameWorld world) {
        return world.getEntities().stream()
                .filter(e -> e != entity && e.getTeam() == entity.getTeam())
                .filter(e -> e.getPosition().distanceTo(entity.getPosition()) <= 5)
                .collect(Collectors.toList());
    }
    
    private double calculateResourceLevel(AIEntity entity) {
        // Упрощенный расчет уровня ресурсов
        return Math.random(); // Заглушка
    }
    
    private double calculateThreatLevel(AIEntity entity, GameWorld world) {
        List<GameEntity> enemies = findNearbyEnemies(entity, world);
        if (enemies.isEmpty()) {
            return 0.0;
        }
        
        // Рассчитываем уровень угрозы на основе количества и силы врагов
        double totalThreat = enemies.stream()
                .mapToDouble(e -> calculateEntityThreat(e))
                .sum();
        
        return Math.min(1.0, totalThreat / enemies.size());
    }
    
    private double calculateEntityThreat(GameEntity entity) {
        // Упрощенный расчет угрозы от сущности
        if (entity instanceof Unit) {
            Unit unit = (Unit) entity;
            return unit.getAttackPower() * 0.1 + unit.getCurrentHealth() * 0.01;
        }
        return 0.5; // Базовая угроза
    }
    
    public AIBehavior getCurrentBehavior() { return currentBehavior; }
    public Map<String, Object> getBlackboard() { return blackboard; }
}
```

#### 4. Интерфейс `AIBehavior` и его реализации
```java
public interface AIBehavior {
    void onEnter(AIEntity entity);
    void update(AIEntity entity, GameWorld world);
    void onExit(AIEntity entity);
    String getName();
}

public class IdleBehavior implements AIBehavior {
    @Override
    public void onEnter(AIEntity entity) {
        System.out.println(entity.getName() + " переходит в режим ожидания");
    }
    
    @Override
    public void update(AIEntity entity, GameWorld world) {
        // В режиме ожидания ничего не делаем
        // Можно добавить патрулирование или разведку
    }
    
    @Override
    public void onExit(AIEntity entity) {
        System.out.println(entity.getName() + " выходит из режима ожидания");
    }
    
    @Override
    public String getName() {
        return "Idle";
    }
}

public class CombatBehavior implements AIBehavior {
    @Override
    public void onEnter(AIEntity entity) {
        System.out.println(entity.getName() + " переходит в боевой режим");
    }
    
    @Override
    public void update(AIEntity entity, GameWorld world) {
        // Ищем ближайшего врага
        GameEntity target = findNearestEnemy(entity, world);
        
        if (target != null) {
            // Атакуем врага
            if (canAttack(entity, target)) {
                attack(entity, target);
            } else {
                // Двигаемся к врагу
                moveTowards(entity, target);
            }
        } else {
            // Нет врагов - возвращаемся к базе
            returnToBase(entity, world);
        }
    }
    
    @Override
    public void onExit(AIEntity entity) {
        System.out.println(entity.getName() + " выходит из боевого режима");
    }
    
    @Override
    public String getName() {
        return "Combat";
    }
    
    private GameEntity findNearestEnemy(AIEntity entity, GameWorld world) {
        return world.getEntities().stream()
                .filter(e -> e != entity && e.getTeam() != entity.getTeam())
                .min(Comparator.comparingDouble(e -> e.getPosition().distanceTo(entity.getPosition())))
                .orElse(null);
    }
    
    private boolean canAttack(AIEntity entity, GameEntity target) {
        // Проверяем, может ли атаковать
        double distance = entity.getPosition().distanceTo(target.getPosition());
        return distance <= 2; // Дистанция атаки
    }
    
    private void attack(AIEntity entity, GameEntity target) {
        System.out.println(entity.getName() + " атакует " + target.getName());
        // Логика атаки
    }
    
    private void moveTowards(AIEntity entity, GameEntity target) {
        System.out.println(entity.getName() + " движется к " + target.getName());
        // Логика движения
    }
    
    private void returnToBase(AIEntity entity, GameWorld world) {
        System.out.println(entity.getName() + " возвращается на базу");
        // Логика возвращения
    }
}

public class EconomicBehavior implements AIBehavior {
    @Override
    public void onEnter(AIEntity entity) {
        System.out.println(entity.getName() + " переходит в экономический режим");
    }
    
    @Override
    public void update(AIEntity entity, GameWorld world) {
        // Собираем ресурсы
        if (shouldGatherResources(entity, world)) {
            gatherResources(entity, world);
        }
        
        // Строим здания
        if (shouldBuild(entity, world)) {
            buildStructure(entity, world);
        }
        
        // Исследуем технологии
        if (shouldResearch(entity, world)) {
            researchTechnology(entity, world);
        }
    }
    
    @Override
    public void onExit(AIEntity entity) {
        System.out.println(entity.getName() + " выходит из экономического режима");
    }
    
    @Override
    public String getName() {
        return "Economic";
    }
    
    private boolean shouldGatherResources(AIEntity entity, GameWorld world) {
        // Логика принятия решения о сборе ресурсов
        return Math.random() > 0.5;
    }
    
    private boolean shouldBuild(AIEntity entity, GameWorld world) {
        // Логика принятия решения о строительстве
        return Math.random() > 0.7;
    }
    
    private boolean shouldResearch(AIEntity entity, GameWorld world) {
        // Логика принятия решения об исследованиях
        return Math.random() > 0.8;
    }
    
    private void gatherResources(AIEntity entity, GameWorld world) {
        System.out.println(entity.getName() + " собирает ресурсы");
        // Логика сбора ресурсов
    }
    
    private void buildStructure(AIEntity entity, GameWorld world) {
        System.out.println(entity.getName() + " строит здание");
        // Логика строительства
    }
    
    private void researchTechnology(AIEntity entity, GameWorld world) {
        System.out.println(entity.getName() + " исследует технологию");
        // Логика исследований
    }
}

public class ExplorationBehavior implements AIBehavior {
    @Override
    public void onEnter(AIEntity entity) {
        System.out.println(entity.getName() + " переходит в режим разведки");
    }
    
    @Override
    public void update(AIEntity entity, GameWorld world) {
        // Исследуем неизвестную территорию
        Position unexploredPosition = findUnexploredPosition(entity, world);
        
        if (unexploredPosition != null) {
            moveTo(entity, unexploredPosition);
        } else {
            // Вся территория исследована
            System.out.println(entity.getName() + " завершил разведку");
        }
    }
    
    @Override
    public void onExit(AIEntity entity) {
        System.out.println(entity.getName() + " выходит из режима разведки");
    }
    
    @Override
    public String getName() {
        return "Exploration";
    }
    
    private Position findUnexploredPosition(AIEntity entity, GameWorld world) {
        // Упрощенная логика поиска неизвестной позиции
        int x = (int)(Math.random() * 20);
        int y = (int)(Math.random() * 20);
        return new Position(x, y);
    }
    
    private void moveTo(AIEntity entity, Position position) {
        System.out.println(entity.getName() + " движется к " + position);
        // Логика движения
    }
}
```

#### 5. Дерево решений - `DecisionTree`
```java
public class DecisionTree {
    private DecisionNode root;
    
    public DecisionTree() {
        buildDecisionTree();
    }
    
    private void buildDecisionTree() {
        // Создаем дерево решений
        root = new DecisionNode("threat_level > 0.7", 
            new DecisionNode("combat", null, null),
            new DecisionNode("resource_level < 0.3", 
                new DecisionNode("economic", null, null),
                new DecisionNode("military_strength > 0.8", 
                    new DecisionNode("combat", null, null),
                    new DecisionNode("exploration", null, null)
                )
            )
        );
    }
    
    public AIBehavior selectBehavior(Map<String, Object> blackboard) {
        DecisionNode result = traverseTree(root, blackboard);
        return createBehavior(result.getBehaviorName());
    }
    
    private DecisionNode traverseTree(DecisionNode node, Map<String, Object> blackboard) {
        if (node.isLeaf()) {
            return node;
        }
        
        boolean condition = evaluateCondition(node.getCondition(), blackboard);
        
        if (condition) {
            return traverseTree(node.getTrueNode(), blackboard);
        } else {
            return traverseTree(node.getFalseNode(), blackboard);
        }
    }
    
    private boolean evaluateCondition(String condition, Map<String, Object> blackboard) {
        // Упрощенная оценка условий
        if (condition.contains("threat_level > 0.7")) {
            Double threatLevel = (Double) blackboard.get("threat_level");
            return threatLevel != null && threatLevel > 0.7;
        } else if (condition.contains("resource_level < 0.3")) {
            Double resourceLevel = (Double) blackboard.get("resource_level");
            return resourceLevel != null && resourceLevel < 0.3;
        } else if (condition.contains("military_strength > 0.8")) {
            // Упрощенная оценка военной силы
            return Math.random() > 0.2;
        }
        
        return false;
    }
    
    private AIBehavior createBehavior(String behaviorName) {
        switch (behaviorName) {
            case "combat":
                return new CombatBehavior();
            case "economic":
                return new EconomicBehavior();
            case "exploration":
                return new ExplorationBehavior();
            default:
                return new IdleBehavior();
        }
    }
    
    private static class DecisionNode {
        private String condition;
        private String behaviorName;
        private DecisionNode trueNode;
        private DecisionNode falseNode;
        
        public DecisionNode(String condition, DecisionNode trueNode, DecisionNode falseNode) {
            this.condition = condition;
            this.trueNode = trueNode;
            this.falseNode = falseNode;
        }
        
        public DecisionNode(String behaviorName, DecisionNode trueNode, DecisionNode falseNode) {
            this.behaviorName = behaviorName;
            this.trueNode = trueNode;
            this.falseNode = falseNode;
        }
        
        public boolean isLeaf() {
            return behaviorName != null;
        }
        
        public String getCondition() { return condition; }
        public String getBehaviorName() { return behaviorName; }
        public DecisionNode getTrueNode() { return trueNode; }
        public DecisionNode getFalseNode() { return falseNode; }
    }
}
```

#### 6. Конкретные ИИ сущности
```java
public class AIWarrior extends AIEntity {
    private int attackRange;
    private int visionRange;
    
    public AIWarrior(Position position, String name) {
        super(position, name);
        this.attackRange = 2;
        this.visionRange = 5;
    }
    
    @Override
    protected void initializeGoals() {
        // Воин фокусируется на боевых целях
        goals.add(new AIGoal("Защищать территорию", 9, this::defendTerritory));
        goals.add(new AIGoal("Атаковать врагов", 8, this::attackEnemies));
        goals.add(new AIGoal("Патрулировать", 6, this::patrol));
    }
    
    @Override
    protected void updateBehavior() {
        // Обновляем поведение воина
        if (isUnderAttack()) {
            currentState = AIState.DEFENDING;
        } else if (hasEnemyInRange()) {
            currentState = AIState.ATTACKING;
        } else {
            currentState = AIState.MOVING;
        }
    }
    
    @Override
    protected double analyzeResources() {
        // Воин не сильно зависит от ресурсов
        return 0.8;
    }
    
    @Override
    protected double analyzeMilitaryStrength() {
        // Воин имеет высокую военную силу
        return 0.9;
    }
    
    @Override
    protected double analyzeThreats() {
        // Воин хорошо оценивает угрозы
        return calculateThreatLevel();
    }
    
    @Override
    protected double analyzeOpportunities() {
        // Воин ищет возможности для атаки
        return calculateOpportunityLevel();
    }
    
    @Override
    protected void executeAttack() {
        System.out.println(name + " выполняет атаку");
        // Логика атаки
    }
    
    @Override
    protected void strengthenArmy() {
        System.out.println(name + " укрепляет армию");
        // Логика укрепления
    }
    
    @Override
    protected void buildDefenses() {
        System.out.println(name + " строит оборону");
        // Логика строительства обороны
    }
    
    @Override
    protected void gatherResources() {
        System.out.println(name + " собирает ресурсы");
        // Логика сбора ресурсов
    }
    
    @Override
    protected void buildEconomy() {
        System.out.println(name + " строит экономику");
        // Логика строительства экономики
    }
    
    @Override
    protected void buildStructures() {
        System.out.println(name + " строит здания");
        // Логика строительства
    }
    
    @Override
    protected void scoutTerritory() {
        System.out.println(name + " разведывает территорию");
        // Логика разведки
    }
    
    @Override
    protected void researchTechnologies() {
        System.out.println(name + " исследует технологии");
        // Логика исследований
    }
    
    private boolean isUnderAttack() {
        // Проверка, атакуют ли воина
        return Math.random() > 0.8;
    }
    
    private boolean hasEnemyInRange() {
        // Проверка наличия врагов в зоне атаки
        return Math.random() > 0.6;
    }
    
    private double calculateThreatLevel() {
        // Расчет уровня угрозы
        return Math.random();
    }
    
    private double calculateOpportunityLevel() {
        // Расчет уровня возможностей
        return Math.random();
    }
    
    private void defendTerritory() {
        System.out.println(name + " защищает территорию");
    }
    
    private void attackEnemies() {
        System.out.println(name + " атакует врагов");
    }
    
    private void patrol() {
        System.out.println(name + " патрулирует");
    }
}

public class AIArcher extends AIEntity {
    private int attackRange;
    private int visionRange;
    
    public AIArcher(Position position, String name) {
        super(position, name);
        this.attackRange = 4;
        this.visionRange = 6;
    }
    
    @Override
    protected void initializeGoals() {
        // Лучник фокусируется на дальних атаках
        goals.add(new AIGoal("Атаковать с дистанции", 9, this::attackFromDistance));
        goals.add(new AIGoal("Поддерживать союзников", 7, this::supportAllies));
        goals.add(new AIGoal("Избегать ближнего боя", 8, this::avoidCloseCombat));
    }
    
    @Override
    protected void updateBehavior() {
        // Обновляем поведение лучника
        if (isEnemyTooClose()) {
            currentState = AIState.MOVING; // Отступаем
        } else if (hasEnemyInRange()) {
            currentState = AIState.ATTACKING;
        } else {
            currentState = AIState.IDLE;
        }
    }
    
    @Override
    protected double analyzeResources() {
        return 0.7;
    }
    
    @Override
    protected double analyzeMilitaryStrength() {
        return 0.8;
    }
    
    @Override
    protected double analyzeThreats() {
        return calculateThreatLevel();
    }
    
    @Override
    protected double analyzeOpportunities() {
        return calculateOpportunityLevel();
    }
    
    // Реализация абстрактных методов...
    @Override
    protected void executeAttack() { /* ... */ }
    @Override
    protected void strengthenArmy() { /* ... */ }
    @Override
    protected void buildDefenses() { /* ... */ }
    @Override
    protected void gatherResources() { /* ... */ }
    @Override
    protected void buildEconomy() { /* ... */ }
    @Override
    protected void buildStructures() { /* ... */ }
    @Override
    protected void scoutTerritory() { /* ... */ }
    @Override
    protected void researchTechnologies() { /* ... */ }
    
    private boolean isEnemyTooClose() {
        return Math.random() > 0.7;
    }
    
    private boolean hasEnemyInRange() {
        return Math.random() > 0.5;
    }
    
    private double calculateThreatLevel() {
        return Math.random();
    }
    
    private double calculateOpportunityLevel() {
        return Math.random();
    }
    
    private void attackFromDistance() {
        System.out.println(name + " атакует с дистанции");
    }
    
    private void supportAllies() {
        System.out.println(name + " поддерживает союзников");
    }
    
    private void avoidCloseCombat() {
        System.out.println(name + " избегает ближнего боя");
    }
}

public class AIMage extends AIEntity {
    private int magicPower;
    private int manaPool;
    
    public AIMage(Position position, String name) {
        super(position, name);
        this.magicPower = 15;
        this.manaPool = 100;
    }
    
    @Override
    protected void initializeGoals() {
        // Маг фокусируется на магических атаках
        goals.add(new AIGoal("Использовать магию", 10, this::useMagic));
        goals.add(new AIGoal("Восстанавливать ману", 8, this::restoreMana));
        goals.add(new AIGoal("Поддерживать союзников", 7, this::supportAllies));
    }
    
    @Override
    protected void updateBehavior() {
        // Обновляем поведение мага
        if (manaPool < 20) {
            currentState = AIState.IDLE; // Восстанавливаем ману
        } else if (hasEnemyInRange()) {
            currentState = AIState.ATTACKING;
        } else {
            currentState = AIState.IDLE;
        }
    }
    
    @Override
    protected double analyzeResources() {
        return 0.6;
    }
    
    @Override
    protected double analyzeMilitaryStrength() {
        return 0.9; // Маг очень силен в бою
    }
    
    @Override
    protected double analyzeThreats() {
        return calculateThreatLevel();
    }
    
    @Override
    protected double analyzeOpportunities() {
        return calculateOpportunityLevel();
    }
    
    // Реализация абстрактных методов...
    @Override
    protected void executeAttack() { /* ... */ }
    @Override
    protected void strengthenArmy() { /* ... */ }
    @Override
    protected void buildDefenses() { /* ... */ }
    @Override
    protected void gatherResources() { /* ... */ }
    @Override
    protected void buildEconomy() { /* ... */ }
    @Override
    protected void buildStructures() { /* ... */ }
    @Override
    protected void scoutTerritory() { /* ... */ }
    @Override
    protected void researchTechnologies() { /* ... */ }
    
    private boolean hasEnemyInRange() {
        return Math.random() > 0.4;
    }
    
    private double calculateThreatLevel() {
        return Math.random();
    }
    
    private double calculateOpportunityLevel() {
        return Math.random();
    }
    
    private void useMagic() {
        System.out.println(name + " использует магию");
        manaPool -= 20;
    }
    
    private void restoreMana() {
        System.out.println(name + " восстанавливает ману");
        manaPool = Math.min(100, manaPool + 10);
    }
    
    private void supportAllies() {
        System.out.println(name + " поддерживает союзников");
    }
}
```

#### 7. Главный класс с демонстрацией ИИ
```java
public class Game {
    private GameWorld gameWorld;
    private List<AIEntity> aiEntities;
    private AIController aiController;
    
    public Game() {
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация игры с ИИ...");
        
        this.gameWorld = new GameWorld();
        this.aiEntities = new ArrayList<>();
        this.aiController = new AIController();
        
        // Создаем ИИ сущностей
        createAIEntities();
        
        System.out.println("Игра с ИИ инициализирована");
    }
    
    private void createAIEntities() {
        // Создаем различных ИИ противников
        aiEntities.add(new AIWarrior(new Position(5, 5), "Враг-Воин"));
        aiEntities.add(new AIArcher(new Position(6, 6), "Враг-Лучник"));
        aiEntities.add(new AIMage(new Position(7, 7), "Враг-Маг"));
        
        // Добавляем их в игровой мир
        for (AIEntity entity : aiEntities) {
            gameWorld.addEntity(entity);
        }
    }
    
    public void start() {
        System.out.println("Запуск игры с ИИ...");
        
        // Основной игровой цикл
        for (int turn = 1; turn <= 10; turn++) {
            System.out.println("\n=== Ход " + turn + " ===");
            
            // Обновляем всех ИИ сущностей
            updateAIEntities();
            
            // Показываем состояние
            displayGameState();
            
            // Небольшая пауза между ходами
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
        
        System.out.println("Игра завершена");
    }
    
    private void updateAIEntities() {
        for (AIEntity entity : aiEntities) {
            if (entity.isAlive()) {
                // Обновляем ИИ
                entity.update();
                
                // Обновляем через контроллер
                aiController.update(entity, gameWorld);
                
                System.out.println(entity.getName() + " - Стратегия: " + 
                                 entity.getCurrentStrategy() + ", Состояние: " + 
                                 entity.getCurrentState());
            }
        }
    }
    
    private void displayGameState() {
        System.out.println("Состояние игры:");
        System.out.println("ИИ сущностей: " + aiEntities.size());
        
        for (AIEntity entity : aiEntities) {
            System.out.println("  " + entity.getName() + 
                             " - Позиция: " + entity.getPosition() +
                             ", Цели: " + entity.getGoals().size());
        }
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        game.start();
    }
}

// Вспомогательные классы
class GameSituation {
    private double resourceLevel;
    private double militaryStrength;
    private double threatLevel;
    private double opportunityLevel;
    
    public GameSituation() {
        this.resourceLevel = 0.5;
        this.militaryStrength = 0.5;
        this.threatLevel = 0.5;
        this.opportunityLevel = 0.5;
    }
    
    public double getResourceLevel() { return resourceLevel; }
    public void setResourceLevel(double resourceLevel) { this.resourceLevel = resourceLevel; }
    
    public double getMilitaryStrength() { return militaryStrength; }
    public void setMilitaryStrength(double militaryStrength) { this.militaryStrength = militaryStrength; }
    
    public double getThreatLevel() { return threatLevel; }
    public void setThreatLevel(double threatLevel) { this.threatLevel = threatLevel; }
    
    public double getOpportunityLevel() { return opportunityLevel; }
    public void setOpportunityLevel(double opportunityLevel) { this.opportunityLevel = opportunityLevel; }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичный ИИ:

### 1. **Гонки на выживание**
- ИИ: AI гонщиков, AI препятствий, AI трасс
- Поведения: агрессивное вождение, оборонительное, экономичное
- Алгоритмы: поиск пути, предсказание движения, тактика обгона

### 2. **Космическая колонизация**
- ИИ: AI колонистов, AI исследователей, AI торговцев
- Поведения: исследование, колонизация, торговля, дипломатия
- Алгоритмы: планирование ресурсов, поиск планет, торговые маршруты

### 3. **Подземелье и драконы**
- ИИ: AI монстров, AI NPC, AI боссов
- Поведения: патрулирование, охота, защита территории, групповые атаки
- Алгоритмы: поиск игрока, тактика боя, групповое поведение

### 4. **Город-государство**
- ИИ: AI горожан, AI торговцев, AI дипломатов
- Поведения: строительство, торговля, дипломатия, развитие
- Алгоритмы: планирование города, экономические решения, политика

### 5. **Пиратская стратегия**
- ИИ: AI пиратов, AI торговцев, AI военных
- Поведения: пиратство, торговля, военные действия, навигация
- Алгоритмы: поиск сокровищ, морские сражения, торговые маршруты

### 6. **Фермерское хозяйство**
- ИИ: AI фермеров, AI животных, AI погоды
- Поведения: сельское хозяйство, животноводство, торговля
- Алгоритмы: планирование урожая, управление ресурсами, рыночные решения

### 7. **Киберпанк-тактика**
- ИИ: AI хакеров, AI корпораций, AI сетей
- Поведения: взлом, защита, цифровая торговля, исследования
- Алгоритмы: сетевые атаки, защита систем, цифровая экономика

### 8. **Средневековая осада**
- ИИ: AI защитников, AI осаждающих, AI инженеров
- Поведения: оборона, осада, строительство, тактика
- Алгоритмы: военное планирование, тактика осады, инженерные решения

### 9. **Зомби-выживание**
- ИИ: AI зомби, AI выживших, AI групп
- Поведения: охота, выживание, групповые действия, строительство
- Алгоритмы: поиск добычи, групповое поведение, планирование убежищ

### 10. **Фэнтези-война**
- ИИ: AI магов, AI воинов, AI существ
- Поведения: магические атаки, тактика боя, групповые заклинания
- Алгоритмы: магические решения, тактическое планирование, групповые действия

## Требования к реализации

### Обязательные требования:
1. **Создать базовый класс ИИ** с системой целей и стратегий
2. **Реализовать поведенческие деревья** для принятия решений
3. **Создать различные типы ИИ** с уникальным поведением
4. **Реализовать систему целей** с приоритетами
5. **Демонстрировать работу** всех типов ИИ
6. **Создать контроллер ИИ** для управления поведением

### Дополнительные требования:
1. **Добавить машинное обучение** для адаптации
2. **Реализовать оптимизацию** производительности
3. **Создать конфигурацию ИИ** через файлы
4. **Добавить профилирование** поведения ИИ

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Базовый класс ИИ** | 4 | Система целей, стратегий и состояний |
| **Поведенческие деревья** | 3 | Принятие решений и выбор поведения |
| **Типы ИИ** | 3 | Различные сущности с уникальным поведением |
| **Система целей** | 3 | Приоритизация и выполнение задач |
| **Контроллер ИИ** | 2 | Управление поведением и состояниями |
| **Демонстрация** | 2 | Работающий пример всех типов ИИ |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужен ИИ в играх?
2. Как работают поведенческие деревья?
3. Как система целей влияет на поведение ИИ?
4. Как выбрать подходящую стратегию для ИИ?
5. Как оптимизировать производительность ИИ?
6. Как создать адаптивный ИИ?
7. Как тестировать поведение ИИ?

## Заключение

В данной лабораторной работе вы изучили принципы создания искусственного интеллекта для игр в Java на примере создания игровых систем. Вы научились:

- Создавать базовые классы ИИ с системой целей
- Реализовывать поведенческие деревья для принятия решений
- Создавать различные типы ИИ с уникальным поведением
- Управлять поведением через контроллеры
- Оптимизировать принятие решений

Полученные знания позволят вам создавать умных и интересных противников для игр.
