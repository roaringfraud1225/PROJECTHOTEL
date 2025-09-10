# Лабораторная работа 11: Многопоточность

## Цель работы
Изучить принципы многопоточного программирования в Java на примере разработки игровых систем. Научиться создавать потоки, синхронизировать данные и управлять параллельным выполнением задач.

## Теоретические основы

### Многопоточность
- **Потоки** - легковесные единицы выполнения
- **Синхронизация** - координация между потоками
- **Блокировки** - защита общих ресурсов
- **Пул потоков** - управление множественными потоками

### Применение в играх
- **ИИ противников** - параллельные вычисления
- **Физика и анимация** - обновление в фоне
- **Сетевое взаимодействие** - асинхронная связь
- **Загрузка ресурсов** - параллельная загрузка

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Многопоточность в игре

#### 1. Базовый класс `GameThread`
```java
public abstract class GameThread extends Thread {
    protected volatile boolean running;
    protected volatile boolean paused;
    protected final Object pauseLock;
    protected long updateInterval;
    protected String threadName;
    
    public GameThread(String name, long updateInterval) {
        this.threadName = name;
        this.updateInterval = updateInterval;
        this.running = false;
        this.paused = false;
        this.pauseLock = new Object();
        setName(name);
    }
    
    @Override
    public void run() {
        running = true;
        System.out.println(threadName + " запущен");
        
        while (running) {
            try {
                // Проверяем паузу
                checkPause();
                
                // Выполняем основную логику
                performUpdate();
                
                // Ждем до следующего обновления
                Thread.sleep(updateInterval);
                
            } catch (InterruptedException e) {
                System.out.println(threadName + " прерван");
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                System.err.println("Ошибка в " + threadName + ": " + e.getMessage());
                e.printStackTrace();
            }
        }
        
        System.out.println(threadName + " завершен");
    }
    
    protected abstract void performUpdate();
    
    protected void checkPause() throws InterruptedException {
        synchronized (pauseLock) {
            while (paused && running) {
                pauseLock.wait();
            }
        }
    }
    
    public void pause() {
        synchronized (pauseLock) {
            paused = true;
            System.out.println(threadName + " приостановлен");
        }
    }
    
    public void resume() {
        synchronized (pauseLock) {
            paused = false;
            pauseLock.notifyAll();
            System.out.println(threadName + " возобновлен");
        }
    }
    
    public void stopThread() {
        running = false;
        resume(); // Снимаем паузу, если поток был приостановлен
    }
    
    public boolean isRunning() { return running; }
    public boolean isPaused() { return paused; }
    public String getThreadName() { return threadName; }
}
```

#### 2. Поток ИИ противника - `AIThread`
```java
public class AIThread extends GameThread {
    private GameWorld gameWorld;
    private List<AIEntity> aiEntities;
    private AIController aiController;
    private final Object worldLock;
    
    public AIThread(GameWorld gameWorld, long updateInterval) {
        super("AI Thread", updateInterval);
        this.gameWorld = gameWorld;
        this.aiEntities = new ArrayList<>();
        this.aiController = new AIController();
        this.worldLock = new Object();
        
        initializeAIEntities();
    }
    
    private void initializeAIEntities() {
        // Создаем ИИ противников
        aiEntities.add(new AIWarrior(new Position(8, 8), "Враг-1"));
        aiEntities.add(new AIArcher(new Position(9, 9), "Враг-2"));
        aiEntities.add(new AIMage(new Position(7, 7), "Враг-3"));
    }
    
    @Override
    protected void performUpdate() {
        synchronized (worldLock) {
            // Обновляем каждую ИИ сущность
            for (AIEntity entity : aiEntities) {
                if (entity.isAlive()) {
                    aiController.updateAI(entity, gameWorld);
                }
            }
            
            // Проверяем боевые действия
            checkCombatActions();
            
            // Обновляем стратегию
            updateAIStrategy();
        }
    }
    
    private void checkCombatActions() {
        for (AIEntity entity : aiEntities) {
            if (entity instanceof CombatAI) {
                CombatAI combatAI = (CombatAI) entity;
                List<Unit> nearbyEnemies = findNearbyEnemies(entity, gameWorld);
                
                if (!nearbyEnemies.isEmpty()) {
                    Unit target = selectBestTarget(nearbyEnemies);
                    combatAI.attack(target);
                }
            }
        }
    }
    
    private void updateAIStrategy() {
        // Адаптируем стратегию на основе ситуации
        int playerStrength = calculatePlayerStrength();
        int aiStrength = calculateAIStrength();
        
        if (playerStrength > aiStrength * 1.5) {
            aiController.setStrategy(AIStrategy.DEFENSIVE);
        } else if (aiStrength > playerStrength * 1.2) {
            aiController.setStrategy(AIStrategy.AGGRESSIVE);
        } else {
            aiController.setStrategy(AIStrategy.BALANCED);
        }
    }
    
    private List<Unit> findNearbyEnemies(AIEntity entity, GameWorld world) {
        // Упрощенная реализация поиска врагов
        return world.getUnits().stream()
                .filter(unit -> unit.getPosition().distanceTo(entity.getPosition()) <= 3)
                .collect(Collectors.toList());
    }
    
    private Unit selectBestTarget(List<Unit> enemies) {
        // Выбираем самого слабого врага
        return enemies.stream()
                .min(Comparator.comparingInt(Unit::getCurrentHealth))
                .orElse(null);
    }
    
    private int calculatePlayerStrength() {
        // Упрощенный расчет силы игрока
        return 100; // Заглушка
    }
    
    private int calculateAIStrength() {
        // Упрощенный расчет силы ИИ
        return aiEntities.stream()
                .mapToInt(AIEntity::getCombatPower)
                .sum();
    }
    
    public void addAIEntity(AIEntity entity) {
        synchronized (worldLock) {
            aiEntities.add(entity);
        }
    }
    
    public void removeAIEntity(AIEntity entity) {
        synchronized (worldLock) {
            aiEntities.remove(entity);
        }
    }
    
    public List<AIEntity> getAIEntities() {
        synchronized (worldLock) {
            return new ArrayList<>(aiEntities);
        }
    }
}
```

#### 3. Поток физики - `PhysicsThread`
```java
public class PhysicsThread extends GameThread {
    private GameWorld gameWorld;
    private PhysicsEngine physicsEngine;
    private List<PhysicsObject> physicsObjects;
    private final Object physicsLock;
    
    public PhysicsThread(GameWorld gameWorld, long updateInterval) {
        super("Physics Thread", updateInterval);
        this.gameWorld = gameWorld;
        this.physicsEngine = new PhysicsEngine();
        this.physicsObjects = new ArrayList<>();
        this.physicsLock = new Object();
        
        initializePhysicsObjects();
    }
    
    private void initializePhysicsObjects() {
        // Создаем физические объекты
        physicsObjects.add(new Projectile(new Position(0, 0), new Vector2D(1, 1)));
        physicsObjects.add(new FallingObject(new Position(5, 10)));
        physicsObjects.add(new MovingPlatform(new Position(3, 3)));
    }
    
    @Override
    protected void performUpdate() {
        synchronized (physicsLock) {
            // Обновляем физику всех объектов
            for (PhysicsObject obj : physicsObjects) {
                physicsEngine.updatePhysics(obj, updateInterval);
            }
            
            // Проверяем коллизии
            checkCollisions();
            
            // Применяем гравитацию
            applyGravity();
            
            // Обновляем позиции в игровом мире
            updateWorldPositions();
        }
    }
    
    private void checkCollisions() {
        for (int i = 0; i < physicsObjects.size(); i++) {
            for (int j = i + 1; j < physicsObjects.size(); j++) {
                PhysicsObject obj1 = physicsObjects.get(i);
                PhysicsObject obj2 = physicsObjects.get(j);
                
                if (physicsEngine.checkCollision(obj1, obj2)) {
                    handleCollision(obj1, obj2);
                }
            }
        }
    }
    
    private void handleCollision(PhysicsObject obj1, PhysicsObject obj2) {
        // Обрабатываем коллизию
        CollisionResponse response = physicsEngine.calculateCollisionResponse(obj1, obj2);
        
        // Применяем ответ на коллизию
        obj1.applyCollisionResponse(response);
        obj2.applyCollisionResponse(response);
        
        // Уведомляем игровой мир о коллизии
        gameWorld.onCollisionDetected(obj1, obj2);
    }
    
    private void applyGravity() {
        for (PhysicsObject obj : physicsObjects) {
            if (obj.isAffectedByGravity()) {
                physicsEngine.applyGravity(obj, updateInterval);
            }
        }
    }
    
    private void updateWorldPositions() {
        for (PhysicsObject obj : physicsObjects) {
            if (obj.hasPositionChanged()) {
                gameWorld.updateObjectPosition(obj);
                obj.markPositionUpdated();
            }
        }
    }
    
    public void addPhysicsObject(PhysicsObject object) {
        synchronized (physicsLock) {
            physicsObjects.add(object);
        }
    }
    
    public void removePhysicsObject(PhysicsObject object) {
        synchronized (physicsLock) {
            physicsObjects.remove(object);
        }
    }
    
    public List<PhysicsObject> getPhysicsObjects() {
        synchronized (physicsLock) {
            return new ArrayList<>(physicsObjects);
        }
    }
}
```

#### 4. Поток загрузки ресурсов - `ResourceLoaderThread`
```java
public class ResourceLoaderThread extends GameThread {
    private ResourceManager resourceManager;
    private Queue<LoadingTask> loadingQueue;
    private Map<String, LoadingStatus> loadingStatuses;
    private final Object queueLock;
    private final Object statusLock;
    
    public ResourceLoaderThread(ResourceManager resourceManager, long updateInterval) {
        super("Resource Loader Thread", updateInterval);
        this.resourceManager = resourceManager;
        this.loadingQueue = new LinkedList<>();
        this.loadingStatuses = new HashMap<>();
        this.queueLock = new Object();
        this.statusLock = new Object();
    }
    
    @Override
    protected void performUpdate() {
        synchronized (queueLock) {
            if (!loadingQueue.isEmpty()) {
                LoadingTask task = loadingQueue.poll();
                processLoadingTask(task);
            }
        }
    }
    
    private void processLoadingTask(LoadingTask task) {
        String resourceId = task.getResourceId();
        
        // Обновляем статус
        updateLoadingStatus(resourceId, LoadingStatus.LOADING);
        
        try {
            // Имитируем загрузку ресурса
            Thread.sleep(task.getLoadingTime());
            
            // Загружаем ресурс
            Resource resource = loadResource(task);
            
            // Добавляем в менеджер ресурсов
            resourceManager.addResource(resource);
            
            // Обновляем статус
            updateLoadingStatus(resourceId, LoadingStatus.COMPLETED);
            
            System.out.println("Ресурс " + resourceId + " загружен");
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            updateLoadingStatus(resourceId, LoadingStatus.FAILED);
        } catch (Exception e) {
            System.err.println("Ошибка загрузки ресурса " + resourceId + ": " + e.getMessage());
            updateLoadingStatus(resourceId, LoadingStatus.FAILED);
        }
    }
    
    private Resource loadResource(LoadingTask task) {
        // Упрощенная загрузка ресурса
        switch (task.getResourceType()) {
            case TEXTURE:
                return new TextureResource(task.getResourceId(), "texture_data");
            case SOUND:
                return new SoundResource(task.getResourceId(), "sound_data");
            case MODEL:
                return new ModelResource(task.getResourceId(), "model_data");
            default:
                return new GenericResource(task.getResourceId(), "generic_data");
        }
    }
    
    private void updateLoadingStatus(String resourceId, LoadingStatus status) {
        synchronized (statusLock) {
            loadingStatuses.put(resourceId, status);
        }
    }
    
    public void addLoadingTask(LoadingTask task) {
        synchronized (queueLock) {
            loadingQueue.offer(task);
            updateLoadingStatus(task.getResourceId(), LoadingStatus.QUEUED);
        }
    }
    
    public LoadingStatus getLoadingStatus(String resourceId) {
        synchronized (statusLock) {
            return loadingStatuses.getOrDefault(resourceId, LoadingStatus.UNKNOWN);
        }
    }
    
    public boolean isResourceLoaded(String resourceId) {
        return getLoadingStatus(resourceId) == LoadingStatus.COMPLETED;
    }
    
    public int getQueueSize() {
        synchronized (queueLock) {
            return loadingQueue.size();
        }
    }
    
    public enum LoadingStatus {
        UNKNOWN, QUEUED, LOADING, COMPLETED, FAILED
    }
}
```

#### 5. Пул потоков - `GameThreadPool`
```java
public class GameThreadPool {
    private ExecutorService executorService;
    private Map<String, Future<?>> runningTasks;
    private final Object tasksLock;
    
    public GameThreadPool(int corePoolSize, int maxPoolSize) {
        this.executorService = new ThreadPoolExecutor(
            corePoolSize,
            maxPoolSize,
            60L,
            TimeUnit.SECONDS,
            new LinkedBlockingQueue<>(),
            new GameThreadFactory()
        );
        this.runningTasks = new HashMap<>();
        this.tasksLock = new Object();
    }
    
    public Future<?> submitTask(String taskName, Runnable task) {
        synchronized (tasksLock) {
            Future<?> future = executorService.submit(() -> {
                try {
                    System.out.println("Задача " + taskName + " запущена");
                    task.run();
                    System.out.println("Задача " + taskName + " завершена");
                } catch (Exception e) {
                    System.err.println("Ошибка в задаче " + taskName + ": " + e.getMessage());
                }
            });
            
            runningTasks.put(taskName, future);
            return future;
        }
    }
    
    public Future<?> submitTask(String taskName, Callable<?> task) {
        synchronized (tasksLock) {
            Future<?> future = executorService.submit(() -> {
                try {
                    System.out.println("Задача " + taskName + " запущена");
                    Object result = task.call();
                    System.out.println("Задача " + taskName + " завершена с результатом: " + result);
                    return result;
                } catch (Exception e) {
                    System.err.println("Ошибка в задаче " + taskName + ": " + e.getMessage());
                    throw new RuntimeException(e);
                }
            });
            
            runningTasks.put(taskName, future);
            return future;
        }
    }
    
    public boolean cancelTask(String taskName) {
        synchronized (tasksLock) {
            Future<?> future = runningTasks.get(taskName);
            if (future != null && !future.isDone()) {
                boolean cancelled = future.cancel(true);
                if (cancelled) {
                    runningTasks.remove(taskName);
                }
                return cancelled;
            }
            return false;
        }
    }
    
    public boolean isTaskRunning(String taskName) {
        synchronized (tasksLock) {
            Future<?> future = runningTasks.get(taskName);
            return future != null && !future.isDone();
        }
    }
    
    public void shutdown() {
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(60, TimeUnit.SECONDS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
    
    public List<String> getRunningTaskNames() {
        synchronized (tasksLock) {
            return new ArrayList<>(runningTasks.keySet());
        }
    }
    
    private static class GameThreadFactory implements ThreadFactory {
        private final AtomicInteger threadNumber = new AtomicInteger(1);
        private final String namePrefix = "GameThread-";
        
        @Override
        public Thread newThread(Runnable r) {
            Thread thread = new Thread(r, namePrefix + threadNumber.getAndIncrement());
            thread.setDaemon(true);
            thread.setPriority(Thread.NORM_PRIORITY);
            return thread;
        }
    }
}
```

#### 6. Синхронизированные коллекции - `ThreadSafeGameWorld`
```java
public class ThreadSafeGameWorld {
    private final ConcurrentHashMap<Position, Cell> gameBoard;
    private final CopyOnWriteArrayList<Unit> units;
    private final CopyOnWriteArrayList<Building> buildings;
    private final ConcurrentHashMap<String, Resource> resources;
    private final ReadWriteLock worldLock;
    
    public ThreadSafeGameWorld() {
        this.gameBoard = new ConcurrentHashMap<>();
        this.units = new CopyOnWriteArrayList<>();
        this.buildings = new CopyOnWriteArrayList<>();
        this.resources = new ConcurrentHashMap<>();
        this.worldLock = new ReentrantReadWriteLock();
        
        initializeWorld();
    }
    
    private void initializeWorld() {
        // Инициализируем игровой мир
        for (int x = 0; x < 10; x++) {
            for (int y = 0; y < 10; y++) {
                Position pos = new Position(x, y);
                gameBoard.put(pos, new Cell(pos));
            }
        }
    }
    
    public boolean addUnit(Unit unit) {
        worldLock.writeLock().lock();
        try {
            if (isValidPosition(unit.getPosition()) && !isPositionOccupied(unit.getPosition())) {
                units.add(unit);
                gameBoard.get(unit.getPosition()).setUnit(unit);
                return true;
            }
            return false;
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public boolean removeUnit(Unit unit) {
        worldLock.writeLock().lock();
        try {
            boolean removed = units.remove(unit);
            if (removed) {
                gameBoard.get(unit.getPosition()).setUnit(null);
            }
            return removed;
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public boolean moveUnit(Unit unit, Position newPosition) {
        worldLock.writeLock().lock();
        try {
            if (isValidPosition(newPosition) && !isPositionOccupied(newPosition)) {
                Position oldPosition = unit.getPosition();
                unit.setPosition(newPosition);
                
                gameBoard.get(oldPosition).setUnit(null);
                gameBoard.get(newPosition).setUnit(unit);
                
                return true;
            }
            return false;
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public boolean addBuilding(Building building) {
        worldLock.writeLock().lock();
        try {
            if (isValidPosition(building.getPosition()) && !isPositionOccupied(building.getPosition())) {
                buildings.add(building);
                gameBoard.get(building.getPosition()).setBuilding(building);
                return true;
            }
            return false;
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public boolean removeBuilding(Building building) {
        worldLock.writeLock().lock();
        try {
            boolean removed = buildings.remove(building);
            if (removed) {
                gameBoard.get(building.getPosition()).setBuilding(null);
            }
            return removed;
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public void updateResource(String resourceName, int amount) {
        resources.compute(resourceName, (key, oldValue) -> {
            if (oldValue == null) {
                return new Resource(key, amount);
            } else {
                oldValue.setAmount(oldValue.getAmount() + amount);
                return oldValue;
            }
        });
    }
    
    public List<Unit> getUnitsInRadius(Position center, int radius) {
        worldLock.readLock().lock();
        try {
            return units.stream()
                    .filter(unit -> unit.getPosition().distanceTo(center) <= radius)
                    .collect(Collectors.toList());
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    public List<Building> getBuildingsInRadius(Position center, int radius) {
        worldLock.readLock().lock();
        try {
            return buildings.stream()
                    .filter(building -> building.getPosition().distanceTo(center) <= radius)
                    .collect(Collectors.toList());
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    private boolean isValidPosition(Position position) {
        return position.getX() >= 0 && position.getX() < 10 &&
               position.getY() >= 0 && position.getY() < 10;
    }
    
    private boolean isPositionOccupied(Position position) {
        Cell cell = gameBoard.get(position);
        return cell != null && (cell.hasUnit() || cell.hasBuilding());
    }
    
    public ConcurrentHashMap<Position, Cell> getGameBoard() { return gameBoard; }
    public CopyOnWriteArrayList<Unit> getUnits() { return units; }
    public CopyOnWriteArrayList<Building> getBuildings() { return buildings; }
    public ConcurrentHashMap<String, Resource> getResources() { return resources; }
}
```

#### 7. Главный класс с демонстрацией многопоточности
```java
public class Game {
    private GameThreadPool threadPool;
    private AIThread aiThread;
    private PhysicsThread physicsThread;
    private ResourceLoaderThread resourceLoaderThread;
    private ThreadSafeGameWorld gameWorld;
    private volatile boolean gameRunning;
    
    public Game() {
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация многопоточной игры...");
        
        // Создаем пул потоков
        this.threadPool = new GameThreadPool(4, 8);
        
        // Создаем игровой мир
        this.gameWorld = new ThreadSafeGameWorld();
        
        // Создаем специализированные потоки
        this.aiThread = new AIThread(gameWorld, 100);
        this.physicsThread = new PhysicsThread(gameWorld, 50);
        this.resourceLoaderThread = new ResourceLoaderThread(new ResourceManager(), 200);
        
        // Загружаем ресурсы
        loadInitialResources();
        
        System.out.println("Многопоточная игра инициализирована");
    }
    
    private void loadInitialResources() {
        // Добавляем задачи загрузки ресурсов
        resourceLoaderThread.addLoadingTask(new LoadingTask("player_texture", ResourceType.TEXTURE, 1000));
        resourceLoaderThread.addLoadingTask(new LoadingTask("background_music", ResourceType.SOUND, 2000));
        resourceLoaderThread.addLoadingTask(new LoadingTask("castle_model", ResourceType.MODEL, 1500));
    }
    
    public void start() {
        System.out.println("Запуск многопоточной игры...");
        gameRunning = true;
        
        // Запускаем специализированные потоки
        aiThread.start();
        physicsThread.start();
        resourceLoaderThread.start();
        
        // Запускаем дополнительные задачи в пуле потоков
        submitGameTasks();
        
        // Основной игровой цикл
        runGameLoop();
    }
    
    private void submitGameTasks() {
        // Задача обновления статистики
        threadPool.submitTask("Statistics Update", () -> {
            while (gameRunning) {
                updateGameStatistics();
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
        
        // Задача автоматического сохранения
        threadPool.submitTask("Auto Save", () -> {
            while (gameRunning) {
                autoSaveGame();
                try {
                    Thread.sleep(30000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
        
        // Задача очистки памяти
        threadPool.submitTask("Memory Cleanup", () -> {
            while (gameRunning) {
                cleanupMemory();
                try {
                    Thread.sleep(10000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
    }
    
    private void runGameLoop() {
        try {
            while (gameRunning) {
                // Основная игровая логика
                updateGameState();
                
                // Проверяем состояние потоков
                checkThreadStatus();
                
                // Небольшая задержка
                Thread.sleep(100);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            shutdownGame();
        }
    }
    
    private void updateGameState() {
        // Обновляем состояние игры
        // В реальной игре здесь была бы основная логика
    }
    
    private void checkThreadStatus() {
        // Проверяем состояние всех потоков
        if (!aiThread.isRunning()) {
            System.err.println("AI поток остановлен!");
        }
        if (!physicsThread.isRunning()) {
            System.err.println("Physics поток остановлен!");
        }
        if (!resourceLoaderThread.isRunning()) {
            System.err.println("Resource Loader поток остановлен!");
        }
    }
    
    private void updateGameStatistics() {
        System.out.println("Обновление статистики игры...");
        System.out.println("Активных юнитов: " + gameWorld.getUnits().size());
        System.out.println("Зданий: " + gameWorld.getBuildings().size());
        System.out.println("Задач в пуле: " + threadPool.getRunningTaskNames().size());
    }
    
    private void autoSaveGame() {
        System.out.println("Автоматическое сохранение игры...");
        // Логика сохранения
    }
    
    private void cleanupMemory() {
        System.out.println("Очистка памяти...");
        // Логика очистки
    }
    
    public void pauseGame() {
        System.out.println("Игра приостановлена");
        aiThread.pause();
        physicsThread.pause();
        resourceLoaderThread.pause();
    }
    
    public void resumeGame() {
        System.out.println("Игра возобновлена");
        aiThread.resume();
        physicsThread.resume();
        resourceLoaderThread.resume();
    }
    
    public void stopGame() {
        System.out.println("Остановка игры...");
        gameRunning = false;
        
        // Останавливаем потоки
        aiThread.stopThread();
        physicsThread.stopThread();
        resourceLoaderThread.stopThread();
        
        // Завершаем пул потоков
        threadPool.shutdown();
    }
    
    private void shutdownGame() {
        System.out.println("Завершение игры...");
        stopGame();
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        
        // Запускаем игру
        game.start();
        
        // Даем игре поработать некоторое время
        try {
            Thread.sleep(10000);
            
            // Приостанавливаем игру
            game.pauseGame();
            Thread.sleep(2000);
            
            // Возобновляем игру
            game.resumeGame();
            Thread.sleep(5000);
            
            // Останавливаем игру
            game.stopGame();
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            game.stopGame();
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную многопоточность:

### 1. **Гонки на выживание**
- Потоки: AI гонщиков, физика автомобилей, загрузка трасс
- Синхронизация: позиции машин, результаты гонок
- Пул потоков: обработка событий, обновление статистики

### 2. **Космическая колонизация**
- Потоки: ИИ колонистов, физика космоса, загрузка планет
- Синхронизация: ресурсы колоний, космические корабли
- Пул потоков: исследования, торговля, дипломатия

### 3. **Подземелье и драконы**
- Потоки: ИИ монстров, физика заклинаний, загрузка уровней
- Синхронизация: состояние персонажей, инвентарь
- Пул потоков: генерация подземелий, квесты

### 4. **Город-государство**
- Потоки: ИИ горожан, экономика, загрузка зданий
- Синхронизация: городские ресурсы, население
- Пул потоков: торговля, дипломатия, строительство

### 5. **Пиратская стратегия**
- Потоки: ИИ пиратов, физика кораблей, загрузка островов
- Синхронизация: сокровища, флоты
- Пул потоков: навигация, бои, торговля

### 6. **Фермерское хозяйство**
- Потоки: ИИ животных, погода, загрузка урожая
- Синхронизация: сельскохозяйственные ресурсы, животные
- Пул потоков: рыночные цены, сезоны, болезни

### 7. **Киберпанк-тактика**
- Потоки: ИИ хакеров, сетевые атаки, загрузка данных
- Синхронизация: цифровые ресурсы, импланты
- Пул потоков: взломы, торговля, исследования

### 8. **Средневековая осада**
- Потоки: ИИ защитников, физика осадных орудий, загрузка замков
- Синхронизация: военные ресурсы, укрепления
- Пул потоков: осады, дипломатия, строительство

### 9. **Зомби-выживание**
- Потоки: ИИ зомби, физика оружия, загрузка убежищ
- Синхронизация: ресурсы выживания, зомби
- Пул потоков: генерация событий, торговля

### 10. **Фэнтези-война**
- Потоки: ИИ магов, физика заклинаний, загрузка артефактов
- Синхронизация: магические ресурсы, армии
- Пул потоков: магические исследования, битвы

## Требования к реализации

### Обязательные требования:
1. **Создать специализированные потоки** для различных игровых систем
2. **Реализовать пул потоков** для управления задачами
3. **Синхронизировать данные** между потоками
4. **Создать потокобезопасные коллекции** для игровых объектов
5. **Демонстрировать работу** всех потоков
6. **Реализовать управление потоками** (пауза, возобновление, остановка)

### Дополнительные требования:
1. **Добавить мониторинг потоков** и их состояния
2. **Реализовать обработку ошибок** в потоках
3. **Создать конфигурацию потоков** через файлы
4. **Добавить профилирование** производительности

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Специализированные потоки** | 4 | Потоки для ИИ, физики, загрузки |
| **Пул потоков** | 3 | Управление множественными задачами |
| **Синхронизация данных** | 3 | Безопасный доступ к общим данным |
| **Потокобезопасные коллекции** | 3 | Защищенные структуры данных |
| **Управление потоками** | 2 | Пауза, возобновление, остановка |
| **Демонстрация** | 2 | Работающий пример многопоточности |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужна многопоточность в играх?
2. Как синхронизировать данные между потоками?
3. Что такое пул потоков?
4. Как создать потокобезопасные коллекции?
5. Как управлять жизненным циклом потоков?
6. Как обрабатывать ошибки в потоках?
7. Как оптимизировать многопоточность?

## Заключение

В данной лабораторной работе вы изучили принципы многопоточного программирования в Java на примере создания игровых систем. Вы научились:

- Создавать специализированные потоки для игровых систем
- Управлять множественными потоками через пул
- Синхронизировать данные между потоками
- Использовать потокобезопасные коллекции
- Управлять жизненным циклом потоков

Полученные знания позволят вам создавать высокопроизводительные многопоточные приложения.
