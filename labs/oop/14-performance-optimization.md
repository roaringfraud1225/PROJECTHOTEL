# Лабораторная работа 14: Оптимизация производительности

## Цель работы
Изучить методы оптимизации производительности в Java на примере разработки игровых систем. Научиться профилировать код, оптимизировать алгоритмы, управлять памятью и создавать высокопроизводительные игровые приложения.

## Теоретические основы

### Оптимизация производительности
- **Профилирование** - анализ узких мест в коде
- **Алгоритмическая оптимизация** - улучшение сложности алгоритмов
- **Управление памятью** - минимизация выделений и сборок мусора
- **Кэширование** - хранение часто используемых данных
- **Многопоточность** - параллельная обработка данных

### Методы оптимизации
- **Ленивые вычисления** - отложенная инициализация
- **Пул объектов** - переиспользование объектов
- **Сжатие данных** - уменьшение размера данных
- **Пространственная оптимизация** - эффективное использование памяти
- **Временная оптимизация** - минимизация времени выполнения

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Оптимизация производительности

#### 1. Профилировщик производительности
```java
public class PerformanceProfiler {
    private Map<String, Long> methodTimings;
    private Map<String, Integer> methodCalls;
    private Map<String, Long> memoryUsage;
    private long startTime;
    private Runtime runtime;
    
    public PerformanceProfiler() {
        this.methodTimings = new HashMap<>();
        this.methodCalls = new HashMap<>();
        this.memoryUsage = new HashMap<>();
        this.startTime = System.currentTimeMillis();
        this.runtime = Runtime.getRuntime();
    }
    
    public void startMethod(String methodName) {
        long startTime = System.nanoTime();
        long memoryBefore = runtime.totalMemory() - runtime.freeMemory();
        
        // Сохраняем информацию о начале метода
        methodTimings.put(methodName + "_start", startTime);
        memoryUsage.put(methodName + "_before", memoryBefore);
    }
    
    public void endMethod(String methodName) {
        long endTime = System.nanoTime();
        long memoryAfter = runtime.totalMemory() - runtime.freeMemory();
        
        // Получаем время начала
        Long startTime = methodTimings.get(methodName + "_start");
        if (startTime != null) {
            long duration = endTime - startTime;
            
            // Обновляем статистику
            methodTimings.merge(methodName + "_total", duration, Long::sum);
            methodCalls.merge(methodName, 1, Integer::sum);
            
            // Память
            long memoryDelta = memoryAfter - memoryUsage.get(methodName + "_before");
            memoryUsage.merge(methodName + "_delta", memoryDelta, Long::sum);
        }
    }
    
    public void profileMemory(String checkpoint) {
        long currentMemory = runtime.totalMemory() - runtime.freeMemory();
        memoryUsage.put(checkpoint, currentMemory);
    }
    
    public void printReport() {
        System.out.println("=== Отчет о производительности ===");
        System.out.println("Общее время работы: " + (System.currentTimeMillis() - startTime) + " мс");
        System.out.println();
        
        System.out.println("Время выполнения методов:");
        for (Map.Entry<String, Long> entry : methodTimings.entrySet()) {
            if (entry.getKey().endsWith("_total")) {
                String methodName = entry.getKey().replace("_total", "");
                Integer calls = methodCalls.get(methodName);
                if (calls != null) {
                    double avgTime = (double) entry.getValue() / calls;
                    System.out.printf("  %s: %d вызовов, общее время: %.2f мс, среднее: %.2f мс%n", 
                                    methodName, calls, entry.getValue() / 1_000_000.0, avgTime / 1_000_000.0);
                }
            }
        }
        
        System.out.println();
        System.out.println("Использование памяти:");
        for (Map.Entry<String, Long> entry : memoryUsage.entrySet()) {
            if (!entry.getKey().contains("_")) {
                System.out.printf("  %s: %.2f МБ%n", entry.getKey(), entry.getValue() / (1024.0 * 1024.0));
            }
        }
    }
    
    public Map<String, Long> getMethodTimings() { return methodTimings; }
    public Map<String, Integer> getMethodCalls() { return methodCalls; }
    public Map<String, Long> getMemoryUsage() { return memoryUsage; }
}
```

#### 2. Оптимизированный игровой мир
```java
public class OptimizedGameWorld {
    private Map<Position, GameEntity> entityGrid;
    private List<GameEntity> allEntities;
    private Map<Class<?>, List<GameEntity>> entitiesByType;
    private SpatialHashGrid spatialGrid;
    private ObjectPool<GameEntity> entityPool;
    private PerformanceProfiler profiler;
    
    public OptimizedGameWorld() {
        this.entityGrid = new HashMap<>();
        this.allEntities = new ArrayList<>();
        this.entitiesByType = new HashMap<>();
        this.spatialGrid = new SpatialHashGrid(100, 100, 10);
        this.entityPool = new ObjectPool<>(1000);
        this.profiler = new PerformanceProfiler();
    }
    
    public void addEntity(GameEntity entity) {
        profiler.startMethod("addEntity");
        
        // Добавляем в основные коллекции
        allEntities.add(entity);
        entityGrid.put(entity.getPosition(), entity);
        
        // Добавляем в пространственную сетку
        spatialGrid.addEntity(entity);
        
        // Добавляем в типизированные коллекции
        Class<?> entityClass = entity.getClass();
        entitiesByType.computeIfAbsent(entityClass, k -> new ArrayList<>()).add(entity);
        
        profiler.endMethod("addEntity");
    }
    
    public void removeEntity(GameEntity entity) {
        profiler.startMethod("removeEntity");
        
        // Удаляем из всех коллекций
        allEntities.remove(entity);
        entityGrid.remove(entity.getPosition());
        spatialGrid.removeEntity(entity);
        
        Class<?> entityClass = entity.getClass();
        List<GameEntity> typeList = entitiesByType.get(entityClass);
        if (typeList != null) {
            typeList.remove(entity);
        }
        
        // Возвращаем в пул
        entityPool.returnObject(entity);
        
        profiler.endMethod("removeEntity");
    }
    
    public List<GameEntity> getEntitiesInRange(Position center, double radius) {
        profiler.startMethod("getEntitiesInRange");
        
        // Используем пространственную сетку для быстрого поиска
        List<GameEntity> result = spatialGrid.getEntitiesInRange(center, radius);
        
        profiler.endMethod("getEntitiesInRange");
        return result;
    }
    
    public <T extends GameEntity> List<T> getEntitiesOfType(Class<T> type) {
        profiler.startMethod("getEntitiesOfType");
        
        @SuppressWarnings("unchecked")
        List<T> result = (List<T>) entitiesByType.getOrDefault(type, new ArrayList<>());
        
        profiler.endMethod("getEntitiesOfType");
        return result;
    }
    
    public void update() {
        profiler.startMethod("update");
        
        // Обновляем все сущности
        for (GameEntity entity : allEntities) {
            if (entity.isAlive()) {
                entity.update();
            }
        }
        
        // Очищаем мертвые сущности
        cleanupDeadEntities();
        
        profiler.endMethod("update");
    }
    
    private void cleanupDeadEntities() {
        profiler.startMethod("cleanupDeadEntities");
        
        // Используем итератор для безопасного удаления
        Iterator<GameEntity> iterator = allEntities.iterator();
        while (iterator.hasNext()) {
            GameEntity entity = iterator.next();
            if (!entity.isAlive()) {
                iterator.remove();
                entityGrid.remove(entity.getPosition());
                spatialGrid.removeEntity(entity);
                
                // Удаляем из типизированных коллекций
                Class<?> entityClass = entity.getClass();
                List<GameEntity> typeList = entitiesByType.get(entityClass);
                if (typeList != null) {
                    typeList.remove(entity);
                }
            }
        }
        
        profiler.endMethod("cleanupDeadEntities");
    }
    
    public PerformanceProfiler getProfiler() { return profiler; }
}
```

#### 3. Пространственная хеш-сетка
```java
public class SpatialHashGrid {
    private Map<Long, List<GameEntity>> grid;
    private int cellSize;
    private int width;
    private int height;
    
    public SpatialHashGrid(int width, int height, int cellSize) {
        this.grid = new HashMap<>();
        this.width = width;
        this.height = height;
        this.cellSize = cellSize;
    }
    
    public void addEntity(GameEntity entity) {
        long cellKey = getCellKey(entity.getPosition());
        grid.computeIfAbsent(cellKey, k -> new ArrayList<>()).add(entity);
    }
    
    public void removeEntity(GameEntity entity) {
        long cellKey = getCellKey(entity.getPosition());
        List<GameEntity> cell = grid.get(cellKey);
        if (cell != null) {
            cell.remove(entity);
            if (cell.isEmpty()) {
                grid.remove(cellKey);
            }
        }
    }
    
    public List<GameEntity> getEntitiesInRange(Position center, double radius) {
        Set<GameEntity> result = new HashSet<>();
        
        // Определяем границы поиска
        int minX = Math.max(0, (int)((center.getX() - radius) / cellSize));
        int maxX = Math.min(width / cellSize, (int)((center.getX() + radius) / cellSize));
        int minY = Math.max(0, (int)((center.getY() - radius) / cellSize));
        int maxY = Math.min(height / cellSize, (int)((center.getY() + radius) / cellSize));
        
        // Проверяем все ячейки в радиусе
        for (int x = minX; x <= maxX; x++) {
            for (int y = minY; y <= maxY; y++) {
                long cellKey = getCellKey(x, y);
                List<GameEntity> cell = grid.get(cellKey);
                if (cell != null) {
                    for (GameEntity entity : cell) {
                        if (center.distanceTo(entity.getPosition()) <= radius) {
                            result.add(entity);
                        }
                    }
                }
            }
        }
        
        return new ArrayList<>(result);
    }
    
    private long getCellKey(Position position) {
        return getCellKey((int)(position.getX() / cellSize), (int)(position.getY() / cellSize));
    }
    
    private long getCellKey(int x, int y) {
        return ((long) x << 32) | (y & 0xFFFFFFFFL);
    }
    
    public void clear() {
        grid.clear();
    }
    
    public int getCellCount() {
        return grid.size();
    }
}
```

#### 4. Пул объектов
```java
public class ObjectPool<T> {
    private Queue<T> availableObjects;
    private Set<T> inUseObjects;
    private Supplier<T> objectFactory;
    private int maxSize;
    
    public ObjectPool(int maxSize, Supplier<T> objectFactory) {
        this.maxSize = maxSize;
        this.objectFactory = objectFactory;
        this.availableObjects = new LinkedList<>();
        this.inUseObjects = new HashSet<>();
    }
    
    public T borrowObject() {
        T object;
        
        if (availableObjects.isEmpty() && inUseObjects.size() < maxSize) {
            // Создаем новый объект
            object = objectFactory.get();
        } else if (!availableObjects.isEmpty()) {
            // Берем из пула
            object = availableObjects.poll();
        } else {
            // Пул переполнен
            return null;
        }
        
        inUseObjects.add(object);
        return object;
    }
    
    public void returnObject(T object) {
        if (inUseObjects.remove(object)) {
            // Сбрасываем состояние объекта
            resetObject(object);
            
            // Возвращаем в пул
            availableObjects.offer(object);
        }
    }
    
    private void resetObject(T object) {
        // Сбрасываем состояние объекта
        if (object instanceof Resettable) {
            ((Resettable) object).reset();
        }
    }
    
    public int getAvailableCount() {
        return availableObjects.size();
    }
    
    public int getInUseCount() {
        return inUseObjects.size();
    }
    
    public int getTotalCount() {
        return availableObjects.size() + inUseObjects.size();
    }
    
    public void clear() {
        availableObjects.clear();
        inUseObjects.clear();
    }
    
    public interface Resettable {
        void reset();
    }
}
```

#### 5. Кэш для часто используемых данных
```java
public class GameCache<K, V> {
    private Map<K, CacheEntry<V>> cache;
    private int maxSize;
    private long defaultTTL;
    
    public GameCache(int maxSize, long defaultTTL) {
        this.cache = new LinkedHashMap<>(maxSize, 0.75f, true) {
            @Override
            protected boolean removeEldestEntry(Map.Entry<K, CacheEntry<V>> eldest) {
                return size() > maxSize;
            }
        };
        this.maxSize = maxSize;
        this.defaultTTL = defaultTTL;
    }
    
    public V get(K key) {
        CacheEntry<V> entry = cache.get(key);
        
        if (entry != null && !entry.isExpired()) {
            // Обновляем время последнего доступа
            entry.updateAccessTime();
            return entry.getValue();
        } else if (entry != null) {
            // Удаляем истекшую запись
            cache.remove(key);
        }
        
        return null;
    }
    
    public void put(K key, V value) {
        put(key, value, defaultTTL);
    }
    
    public void put(K key, V value, long ttl) {
        cache.put(key, new CacheEntry<>(value, ttl));
    }
    
    public void remove(K key) {
        cache.remove(key);
    }
    
    public void clear() {
        cache.clear();
    }
    
    public int size() {
        return cache.size();
    }
    
    public boolean containsKey(K key) {
        CacheEntry<V> entry = cache.get(key);
        return entry != null && !entry.isExpired();
    }
    
    public void cleanup() {
        // Удаляем истекшие записи
        cache.entrySet().removeIf(entry -> entry.getValue().isExpired());
    }
    
    private static class CacheEntry<V> {
        private V value;
        private long creationTime;
        private long lastAccessTime;
        private long ttl;
        
        public CacheEntry(V value, long ttl) {
            this.value = value;
            this.creationTime = System.currentTimeMillis();
            this.lastAccessTime = this.creationTime;
            this.ttl = ttl;
        }
        
        public V getValue() {
            return value;
        }
        
        public boolean isExpired() {
            return System.currentTimeMillis() - creationTime > ttl;
        }
        
        public void updateAccessTime() {
            this.lastAccessTime = System.currentTimeMillis();
        }
        
        public long getLastAccessTime() {
            return lastAccessTime;
        }
    }
}
```

#### 6. Оптимизированный рендерер
```java
public class OptimizedRenderer {
    private Map<String, BufferedImage> textureCache;
    private List<RenderCommand> renderQueue;
    private PerformanceProfiler profiler;
    private boolean enableCulling;
    private boolean enableLOD;
    
    public OptimizedRenderer() {
        this.textureCache = new HashMap<>();
        this.renderQueue = new ArrayList<>();
        this.profiler = new PerformanceProfiler();
        this.enableCulling = true;
        this.enableLOD = true;
    }
    
    public void render(GameWorld world, Graphics2D g) {
        profiler.startMethod("render");
        
        // Очищаем очередь рендеринга
        renderQueue.clear();
        
        // Собираем объекты для рендеринга
        collectRenderObjects(world);
        
        // Сортируем по глубине (z-order)
        sortRenderQueue();
        
        // Рендерим объекты
        executeRenderQueue(g);
        
        profiler.endMethod("render");
    }
    
    private void collectRenderObjects(GameWorld world) {
        profiler.startMethod("collectRenderObjects");
        
        for (GameEntity entity : world.getAllEntities()) {
            if (shouldRender(entity)) {
                RenderCommand command = createRenderCommand(entity);
                if (command != null) {
                    renderQueue.add(command);
                }
            }
        }
        
        profiler.endMethod("collectRenderObjects");
    }
    
    private boolean shouldRender(GameEntity entity) {
        if (!enableCulling) {
            return true;
        }
        
        // Проверяем видимость объекта
        return isVisible(entity) && isInViewport(entity);
    }
    
    private boolean isVisible(GameEntity entity) {
        // Проверяем, видим ли объект
        return entity.isVisible() && entity.getCurrentHealth() > 0;
    }
    
    private boolean isInViewport(GameEntity entity) {
        // Проверяем, находится ли объект в области видимости
        Position pos = entity.getPosition();
        return pos.getX() >= 0 && pos.getX() < 800 && 
               pos.getY() >= 0 && pos.getY() < 600;
    }
    
    private RenderCommand createRenderCommand(GameEntity entity) {
        // Создаем команду рендеринга с учетом LOD
        if (enableLOD) {
            return createLODRenderCommand(entity);
        } else {
            return new RenderCommand(entity, RenderQuality.HIGH);
        }
    }
    
    private RenderCommand createLODRenderCommand(GameEntity entity) {
        Position pos = entity.getPosition();
        double distance = Math.sqrt(pos.getX() * pos.getX() + pos.getY() * pos.getY());
        
        RenderQuality quality;
        if (distance < 100) {
            quality = RenderQuality.HIGH;
        } else if (distance < 300) {
            quality = RenderQuality.MEDIUM;
        } else {
            quality = RenderQuality.LOW;
        }
        
        return new RenderCommand(entity, quality);
    }
    
    private void sortRenderQueue() {
        profiler.startMethod("sortRenderQueue");
        
        // Сортируем по z-координате (глубине)
        renderQueue.sort(Comparator.comparingInt(cmd -> cmd.getEntity().getPosition().getZ()));
        
        profiler.endMethod("sortRenderQueue");
    }
    
    private void executeRenderQueue(Graphics2D g) {
        profiler.startMethod("executeRenderQueue");
        
        for (RenderCommand command : renderQueue) {
            renderEntity(command, g);
        }
        
        profiler.endMethod("executeRenderQueue");
    }
    
    private void renderEntity(RenderCommand command, Graphics2D g) {
        GameEntity entity = command.getEntity();
        RenderQuality quality = command.getQuality();
        
        // Рендерим с учетом качества
        switch (quality) {
            case HIGH:
                renderHighQuality(entity, g);
                break;
            case MEDIUM:
                renderMediumQuality(entity, g);
                break;
            case LOW:
                renderLowQuality(entity, g);
                break;
        }
    }
    
    private void renderHighQuality(GameEntity entity, Graphics2D g) {
        // Высококачественный рендеринг
        Position pos = entity.getPosition();
        g.setColor(Color.BLUE);
        g.fillOval((int)pos.getX() - 10, (int)pos.getY() - 10, 20, 20);
        g.setColor(Color.BLACK);
        g.drawString(entity.getName(), (int)pos.getX(), (int)pos.getY() - 15);
    }
    
    private void renderMediumQuality(GameEntity entity, Graphics2D g) {
        // Среднекачественный рендеринг
        Position pos = entity.getPosition();
        g.setColor(Color.BLUE);
        g.fillOval((int)pos.getX() - 8, (int)pos.getY() - 8, 16, 16);
    }
    
    private void renderLowQuality(GameEntity entity, Graphics2D g) {
        // Низкокачественный рендеринг
        Position pos = entity.getPosition();
        g.setColor(Color.BLUE);
        g.fillOval((int)pos.getX() - 5, (int)pos.getY() - 5, 10, 10);
    }
    
    public void setEnableCulling(boolean enableCulling) {
        this.enableCulling = enableCulling;
    }
    
    public void setEnableLOD(boolean enableLOD) {
        this.enableLOD = enableLOD;
    }
    
    public PerformanceProfiler getProfiler() { return profiler; }
    
    public enum RenderQuality {
        HIGH, MEDIUM, LOW
    }
    
    public static class RenderCommand {
        private GameEntity entity;
        private RenderQuality quality;
        
        public RenderCommand(GameEntity entity, RenderQuality quality) {
            this.entity = entity;
            this.quality = quality;
        }
        
        public GameEntity getEntity() { return entity; }
        public RenderQuality getQuality() { return quality; }
    }
}
```

#### 7. Главный класс с демонстрацией оптимизации
```java
public class Game {
    private OptimizedGameWorld gameWorld;
    private OptimizedRenderer renderer;
    private PerformanceProfiler profiler;
    private long lastFrameTime;
    private int frameCount;
    private double fps;
    
    public Game() {
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация оптимизированной игры...");
        
        this.gameWorld = new OptimizedGameWorld();
        this.renderer = new OptimizedRenderer();
        this.profiler = new PerformanceProfiler();
        this.lastFrameTime = System.nanoTime();
        this.frameCount = 0;
        this.fps = 0.0;
        
        // Создаем тестовые сущности
        createTestEntities();
        
        System.out.println("Оптимизированная игра инициализирована");
    }
    
    private void createTestEntities() {
        // Создаем множество сущностей для тестирования производительности
        for (int i = 0; i < 1000; i++) {
            Position pos = new Position((int)(Math.random() * 800), (int)(Math.random() * 600));
            GameEntity entity = new TestEntity(pos, "Entity-" + i);
            gameWorld.addEntity(entity);
        }
    }
    
    public void start() {
        System.out.println("Запуск оптимизированной игры...");
        
        // Основной игровой цикл
        for (int frame = 1; frame <= 100; frame++) {
            long frameStartTime = System.nanoTime();
            
            // Обновляем игровой мир
            updateGame();
            
            // Рендерим кадр
            renderFrame();
            
            // Обновляем статистику FPS
            updateFPS(frameStartTime);
            
            // Показываем статистику каждые 10 кадров
            if (frame % 10 == 0) {
                displayStats(frame);
            }
            
            // Небольшая пауза для стабилизации FPS
            try {
                Thread.sleep(16); // ~60 FPS
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
        
        // Показываем финальный отчет
        showFinalReport();
    }
    
    private void updateGame() {
        profiler.startMethod("updateGame");
        
        // Обновляем игровой мир
        gameWorld.update();
        
        profiler.endMethod("updateGame");
    }
    
    private void renderFrame() {
        profiler.startMethod("renderFrame");
        
        // Создаем графический контекст для рендеринга
        BufferedImage buffer = new BufferedImage(800, 600, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = buffer.createGraphics();
        
        // Рендерим кадр
        renderer.render(gameWorld, g);
        
        g.dispose();
        
        profiler.endMethod("renderFrame");
    }
    
    private void updateFPS(long frameStartTime) {
        long currentTime = System.nanoTime();
        long frameTime = currentTime - lastFrameTime;
        
        if (frameTime > 0) {
            fps = 1_000_000_000.0 / frameTime;
        }
        
        lastFrameTime = currentTime;
        frameCount++;
    }
    
    private void displayStats(int frame) {
        System.out.printf("Кадр %d - FPS: %.1f, Сущностей: %d%n", 
                         frame, fps, gameWorld.getAllEntities().size());
    }
    
    private void showFinalReport() {
        System.out.println("\n=== Финальный отчет ===");
        System.out.println("Общее количество кадров: " + frameCount);
        System.out.println("Средний FPS: " + fps);
        
        // Показываем отчет профилировщика
        profiler.printReport();
        
        // Показываем отчет игрового мира
        gameWorld.getProfiler().printReport();
        
        // Показываем отчет рендерера
        renderer.getProfiler().printReport();
    }
    
    public static void main(String[] args) {
        Game game = new Game();
        game.start();
    }
}

// Вспомогательные классы
class TestEntity extends GameEntity {
    private static int entityCount = 0;
    
    public TestEntity(Position position, String name) {
        super(position, name);
        entityCount++;
    }
    
    @Override
    public void update() {
        // Простое обновление для тестирования
        if (Math.random() > 0.99) {
            setAlive(false);
        }
    }
    
    @Override
    public boolean isVisible() {
        return true;
    }
    
    @Override
    public int getPosition() {
        return 0;
    }
    
    public static int getEntityCount() {
        return entityCount;
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную оптимизацию:

### 1. **Гонки на выживание**
- Оптимизация: физика движения, коллизии, рендеринг трасс
- Методы: пространственное хеширование, LOD для дальних объектов
- Профилирование: время рендеринга, физические вычисления

### 2. **Космическая колонизация**
- Оптимизация: генерация планет, торговые маршруты, колонизация
- Методы: процедурная генерация, кэширование данных, пулы объектов
- Профилирование: генерация контента, экономические расчеты

### 3. **Подземелье и драконы**
- Оптимизация: генерация подземелий, ИИ монстров, боевая система
- Методы: алгоритмы генерации, поведенческие деревья, кэш карт
- Профилирование: генерация уровней, ИИ вычисления

### 4. **Город-государство**
- Оптимизация: симуляция горожан, экономика, строительство
- Методы: многопоточность, кэширование расчетов, пулы агентов
- Профилирование: симуляция агентов, экономические расчеты

### 5. **Пиратская стратегия**
- Оптимизация: морские сражения, навигация, торговля
- Методы: пространственное хеширование, алгоритмы поиска пути
- Профилирование: физика кораблей, торговые расчеты

### 6. **Фермерское хозяйство**
- Оптимизация: симуляция растений, животные, погода
- Методы: клеточные автоматы, кэширование состояний, пулы объектов
- Профилирование: симуляция природы, экономические расчеты

### 7. **Киберпанк-тактика**
- Оптимизация: сетевые атаки, цифровая экономика, хакерские системы
- Методы: алгоритмы шифрования, кэширование сетевых данных
- Профилирование: сетевые операции, криптографические вычисления

### 8. **Средневековая осада**
- Оптимизация: военные расчеты, физика осадных орудий, тактика
- Методы: алгоритмы поиска пути, кэширование тактических данных
- Профилирование: военные расчеты, физические симуляции

### 9. **Зомби-выживание**
- Оптимизация: ИИ зомби, генерация уровней, боевая система
- Методы: поведенческие деревья, пространственное хеширование
- Профилирование: ИИ вычисления, генерация контента

### 10. **Фэнтези-война**
- Оптимизация: магические эффекты, боевые расчеты, ИИ существ
- Методы: системы частиц, алгоритмы магии, кэширование заклинаний
- Профилирование: магические вычисления, боевые расчеты

## Требования к реализации

### Обязательные требования:
1. **Создать профилировщик производительности** для анализа кода
2. **Реализовать оптимизированный игровой мир** с пространственным хешированием
3. **Создать пул объектов** для переиспользования
4. **Реализовать кэш** для часто используемых данных
5. **Создать оптимизированный рендерер** с LOD и culling
6. **Демонстрировать улучшение производительности** на примерах

### Дополнительные требования:
1. **Добавить многопоточность** для параллельных вычислений
2. **Реализовать сжатие данных** для экономии памяти
3. **Создать конфигурацию оптимизации** через файлы
4. **Добавить визуализацию** производительности в реальном времени

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Профилировщик** | 3 | Анализ времени и памяти |
| **Оптимизированный мир** | 3 | Пространственное хеширование |
| **Пул объектов** | 2 | Переиспользование объектов |
| **Кэш данных** | 2 | Хранение частых данных |
| **Оптимизированный рендерер** | 3 | LOD, culling, сортировка |
| **Демонстрация оптимизации** | 3 | Измерение улучшений |
| **Качество кода** | 2 | Читаемость, комментарии |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужна оптимизация производительности в играх?
2. Как работает профилирование кода?
3. Что такое пространственное хеширование?
4. Как пул объектов улучшает производительность?
5. Что такое LOD и culling?
6. Как измерить эффективность оптимизации?
7. Какие методы оптимизации наиболее эффективны?

## Заключение

В данной лабораторной работе вы изучили методы оптимизации производительности в Java на примере создания игровых систем. Вы научились:

- Профилировать код и находить узкие места
- Оптимизировать игровой мир с помощью пространственного хеширования
- Использовать пулы объектов для переиспользования
- Кэшировать часто используемые данные
- Создавать оптимизированные рендереры
- Измерять эффективность оптимизации

Полученные знания позволят вам создавать высокопроизводительные игровые приложения.
