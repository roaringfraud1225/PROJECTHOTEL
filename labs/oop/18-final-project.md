# Лабораторная работа 18: Финальный проект

## Цель работы
Создать полноценную игру на Java, применяя все изученные концепции ООП, паттерны проектирования, многопоточность, сетевые технологии и современные практики разработки.

## Теоретические основы

### Финальный проект
- **Интеграция знаний** - применение всех изученных концепций
- **Архитектура игры** - планирование и реализация системы
- **Качество кода** - соблюдение стандартов и лучших практик
- **Тестирование** - комплексное тестирование всех компонентов
- **Документация** - полная документация проекта

### Критерии качества
- **Функциональность** - выполнение всех требований
- **Производительность** - оптимизация и эффективность
- **Надежность** - стабильная работа без ошибок
- **Расширяемость** - возможность добавления новых функций
- **Поддерживаемость** - читаемость и структурированность кода

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Архитектура финального проекта

#### 1. Структура проекта
```
kingdom-game/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/kingdom/
│   │   │       ├── core/           # Основные классы
│   │   │       ├── entities/       # Игровые сущности
│   │   │       ├── systems/        # Игровые системы
│   │   │       ├── ai/             # Искусственный интеллект
│   │   │       ├── ui/             # Пользовательский интерфейс
│   │   │       ├── network/        # Сетевое взаимодействие
│   │   │       ├── utils/          # Утилиты
│   │   │       └── Main.java       # Главный класс
│   │   ├── resources/
│   │   │   ├── config/             # Конфигурационные файлы
│   │   │   ├── assets/             # Графика и звук
│   │   │   └── data/               # Игровые данные
│   │   └── web/                    # Веб-интерфейс (опционально)
│   └── test/
│       └── java/
│           └── com/kingdom/
│               ├── unit/            # Unit тесты
│               ├── integration/     # Integration тесты
│               └── performance/     # Performance тесты
├── docs/                           # Документация
├── scripts/                        # Скрипты сборки и развертывания
├── pom.xml                         # Maven конфигурация
├── README.md                       # Описание проекта
└── LICENSE                         # Лицензия
```

#### 2. Основные компоненты системы

##### GameEngine (Движок игры)
```java
/**
 * Основной движок игры, управляющий всеми системами
 */
public class GameEngine {
    private GameWorld gameWorld;
    private GameRenderer renderer;
    private InputManager inputManager;
    private AudioManager audioManager;
    private NetworkManager networkManager;
    private AISystem aiSystem;
    private GameState gameState;
    private PerformanceMonitor performanceMonitor;
    
    private volatile boolean running;
    private Thread gameThread;
    private ExecutorService threadPool;
    
    public GameEngine() {
        initializeComponents();
        setupThreading();
    }
    
    private void initializeComponents() {
        // Инициализация всех компонентов
        gameWorld = new GameWorld();
        renderer = new GameRenderer();
        inputManager = new InputManager();
        audioManager = new AudioManager();
        networkManager = new NetworkManager();
        aiSystem = new AISystem();
        gameState = new GameState();
        performanceMonitor = new PerformanceMonitor();
        
        // Настройка связей между компонентами
        setupComponentConnections();
    }
    
    private void setupComponentConnections() {
        // Подписываем компоненты на события
        EventBus.getInstance().subscribe(gameWorld, GameEvent.class);
        EventBus.getInstance().subscribe(renderer, RenderEvent.class);
        EventBus.getInstance().subscribe(audioManager, AudioEvent.class);
        EventBus.getInstance().subscribe(networkManager, NetworkEvent.class);
        EventBus.getInstance().subscribe(aiSystem, AIEvent.class);
    }
    
    private void setupThreading() {
        threadPool = Executors.newFixedThreadPool(4);
        running = false;
    }
    
    public void start() {
        if (running) return;
        
        running = true;
        gameThread = new Thread(this::gameLoop, "GameThread");
        gameThread.start();
        
        // Запускаем дополнительные потоки
        startBackgroundThreads();
        
        System.out.println("Игровой движок запущен");
    }
    
    public void stop() {
        if (!running) return;
        
        running = false;
        
        // Останавливаем все потоки
        if (gameThread != null) {
            gameThread.interrupt();
            try {
                gameThread.join(5000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        
        threadPool.shutdown();
        try {
            if (!threadPool.awaitTermination(5, TimeUnit.SECONDS)) {
                threadPool.shutdownNow();
            }
        } catch (InterruptedException e) {
            threadPool.shutdownNow();
            Thread.currentThread().interrupt();
        }
        
        System.out.println("Игровой движок остановлен");
    }
    
    private void gameLoop() {
        long lastTime = System.nanoTime();
        long targetTime = 16_666_667L; // 60 FPS
        
        while (running && !Thread.currentThread().isInterrupted()) {
            long currentTime = System.nanoTime();
            long deltaTime = currentTime - lastTime;
            
            if (deltaTime >= targetTime) {
                update(deltaTime);
                render();
                lastTime = currentTime;
            } else {
                // Небольшая пауза для экономии CPU
                try {
                    Thread.sleep(1);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
    }
    
    private void update(long deltaTime) {
        performanceMonitor.startFrame();
        
        // Обновляем игровой мир
        gameWorld.update(deltaTime);
        
        // Обновляем ИИ
        aiSystem.update(deltaTime);
        
        // Обрабатываем сетевые сообщения
        networkManager.processMessages();
        
        // Обновляем состояние игры
        gameState.update(deltaTime);
        
        performanceMonitor.endFrame();
    }
    
    private void render() {
        renderer.render(gameWorld, gameState);
    }
    
    private void startBackgroundThreads() {
        // Поток для ИИ
        threadPool.submit(() -> {
            while (running && !Thread.currentThread().isInterrupted()) {
                try {
                    aiSystem.processAI();
                    Thread.sleep(100); // 10 раз в секунду
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
        
        // Поток для сетевого взаимодействия
        threadPool.submit(() -> {
            while (running && !Thread.currentThread().isInterrupted()) {
                try {
                    networkManager.update();
                    Thread.sleep(50); // 20 раз в секунду
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
        
        // Поток для аудио
        threadPool.submit(() -> {
            while (running && !Thread.currentThread().isInterrupted()) {
                try {
                    audioManager.update();
                    Thread.sleep(16); // 60 раз в секунду
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });
    }
    
    public GameWorld getGameWorld() { return gameWorld; }
    public GameRenderer getRenderer() { return renderer; }
    public InputManager getInputManager() { return inputManager; }
    public AudioManager getAudioManager() { return audioManager; }
    public NetworkManager getNetworkManager() { return networkManager; }
    public AISystem getAISystem() { return aiSystem; }
    public GameState getGameState() { return gameState; }
    public PerformanceMonitor getPerformanceMonitor() { return performanceMonitor; }
}
```

##### GameWorld (Игровой мир)
```java
/**
 * Управляет игровым миром и всеми сущностями
 */
public class GameWorld {
    private final Map<String, GameEntity> entities;
    private final SpatialHashGrid spatialGrid;
    private final ResourceManager resourceManager;
    private final BuildingManager buildingManager;
    private final UnitManager unitManager;
    private final EventManager eventManager;
    private final GameRules gameRules;
    
    private final ReadWriteLock worldLock;
    private final ExecutorService updateExecutor;
    
    private int width, height;
    private long gameTime;
    private int currentTurn;
    
    public GameWorld() {
        this.entities = new ConcurrentHashMap<>();
        this.spatialGrid = new SpatialHashGrid(1000, 1000, 50);
        this.resourceManager = new ResourceManager();
        this.buildingManager = new BuildingManager();
        this.unitManager = new UnitManager();
        this.eventManager = new EventManager();
        this.gameRules = new GameRules();
        
        this.worldLock = new ReentrantReadWriteLock();
        this.updateExecutor = Executors.newFixedThreadPool(2);
        
        this.width = 1000;
        this.height = 1000;
        this.gameTime = 0;
        this.currentTurn = 0;
    }
    
    public void update(long deltaTime) {
        gameTime += deltaTime;
        
        // Обновляем мир в отдельном потоке
        updateExecutor.submit(() -> {
            try {
                worldLock.writeLock().lock();
                
                // Обновляем все сущности
                List<GameEntity> entitiesToUpdate = new ArrayList<>(entities.values());
                
                // Разбиваем на батчи для параллельной обработки
                int batchSize = 100;
                for (int i = 0; i < entitiesToUpdate.size(); i += batchSize) {
                    int end = Math.min(i + batchSize, entitiesToUpdate.size());
                    List<GameEntity> batch = entitiesToUpdate.subList(i, end);
                    
                    // Параллельное обновление батча
                    batch.parallelStream()
                          .filter(GameEntity::isAlive)
                          .forEach(entity -> {
                              try {
                                  entity.update();
                              } catch (Exception e) {
                                  System.err.println("Ошибка обновления сущности: " + entity.getName());
                                  e.printStackTrace();
                              }
                          });
                }
                
                // Очищаем мертвые сущности
                cleanupDeadEntities();
                
                // Обновляем пространственную сетку
                spatialGrid.update();
                
                // Проверяем правила игры
                gameRules.checkRules(this);
                
            } finally {
                worldLock.writeLock().unlock();
            }
        });
    }
    
    public void addEntity(GameEntity entity) {
        try {
            worldLock.writeLock().lock();
            
            entities.put(entity.getId(), entity);
            spatialGrid.addEntity(entity);
            
            // Уведомляем о добавлении
            eventManager.fireEvent(new EntityAddedEvent(entity));
            
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public void removeEntity(GameEntity entity) {
        try {
            worldLock.writeLock().lock();
            
            entities.remove(entity.getId());
            spatialGrid.removeEntity(entity);
            
            // Уведомляем об удалении
            eventManager.fireEvent(new EntityRemovedEvent(entity));
            
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    public GameEntity getEntity(String id) {
        try {
            worldLock.readLock().lock();
            return entities.get(id);
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    public List<GameEntity> getEntitiesInRange(Position center, double radius) {
        try {
            worldLock.readLock().lock();
            return spatialGrid.getEntitiesInRange(center, radius);
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    public List<GameEntity> getEntitiesByType(Class<? extends GameEntity> type) {
        try {
            worldLock.readLock().lock();
            return entities.values().stream()
                         .filter(type::isInstance)
                         .collect(Collectors.toList());
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    private void cleanupDeadEntities() {
        List<String> deadEntityIds = entities.values().stream()
                                            .filter(entity -> !entity.isAlive())
                                            .map(GameEntity::getId)
                                            .collect(Collectors.toList());
        
        deadEntityIds.forEach(id -> {
            GameEntity entity = entities.get(id);
            if (entity != null) {
                removeEntity(entity);
            }
        });
    }
    
    public void nextTurn() {
        currentTurn++;
        
        // Обновляем ресурсы
        resourceManager.updateProduction();
        
        // Обновляем здания
        buildingManager.updateBuildings();
        
        // Обновляем юниты
        unitManager.updateUnits();
        
        // Уведомляем о новом ходе
        eventManager.fireEvent(new TurnEndedEvent(currentTurn));
    }
    
    public void save(String filename) {
        try {
            worldLock.readLock().lock();
            
            GameWorldData data = new GameWorldData();
            data.setEntities(new ArrayList<>(entities.values()));
            data.setGameTime(gameTime);
            data.setCurrentTurn(currentTurn);
            data.setWidth(width);
            data.setHeight(height);
            
            // Сериализуем в JSON
            ObjectMapper mapper = new ObjectMapper();
            mapper.writeValue(new File(filename), data);
            
        } catch (IOException e) {
            throw new RuntimeException("Ошибка сохранения мира", e);
        } finally {
            worldLock.readLock().unlock();
        }
    }
    
    public void load(String filename) {
        try {
            worldLock.writeLock().lock();
            
            // Очищаем текущий мир
            entities.clear();
            spatialGrid.clear();
            
            // Загружаем данные
            ObjectMapper mapper = new ObjectMapper();
            GameWorldData data = mapper.readValue(new File(filename), GameWorldData.class);
            
            // Восстанавливаем состояние
            this.gameTime = data.getGameTime();
            this.currentTurn = data.getCurrentTurn();
            this.width = data.getWidth();
            this.height = data.getHeight();
            
            // Восстанавливаем сущности
            for (GameEntity entity : data.getEntities()) {
                addEntity(entity);
            }
            
        } catch (IOException e) {
            throw new RuntimeException("Ошибка загрузки мира", e);
        } finally {
            worldLock.writeLock().unlock();
        }
    }
    
    // Геттеры
    public int getEntityCount() { return entities.size(); }
    public long getGameTime() { return gameTime; }
    public int getCurrentTurn() { return currentTurn; }
    public int getWidth() { return width; }
    public int getHeight() { return height; }
    public ResourceManager getResourceManager() { return resourceManager; }
    public BuildingManager getBuildingManager() { return buildingManager; }
    public UnitManager getUnitManager() { return unitManager; }
    public EventManager getEventManager() { return eventManager; }
    public GameRules getGameRules() { return gameRules; }
}
```

##### GameRenderer (Рендерер)
```java
/**
 * Отвечает за рендеринг игрового мира
 */
public class GameRenderer {
    private final Graphics2D graphics;
    private final BufferedImage buffer;
    private final Camera camera;
    private final RenderQueue renderQueue;
    private final TextureManager textureManager;
    private final ShaderManager shaderManager;
    
    private final Map<RenderLayer, List<RenderCommand>> layers;
    private final PerformanceProfiler profiler;
    
    public GameRenderer(int width, int height) {
        this.buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        this.graphics = buffer.createGraphics();
        this.camera = new Camera(width, height);
        this.renderQueue = new RenderQueue();
        this.textureManager = new TextureManager();
        this.shaderManager = new ShaderManager();
        
        this.layers = new EnumMap<>(RenderLayer.class);
        for (RenderLayer layer : RenderLayer.values()) {
            layers.put(layer, new ArrayList<>());
        }
        
        this.profiler = new PerformanceProfiler();
        
        setupGraphics();
    }
    
    private void setupGraphics() {
        // Включаем сглаживание
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                                 RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, 
                                 RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_RENDERING, 
                                 RenderingHints.VALUE_RENDER_QUALITY);
    }
    
    public void render(GameWorld world, GameState gameState) {
        profiler.startMethod("render");
        
        // Очищаем буфер
        graphics.setColor(new Color(0, 0, 0, 0));
        graphics.fillRect(0, 0, buffer.getWidth(), buffer.getHeight());
        
        // Собираем объекты для рендеринга
        collectRenderObjects(world);
        
        // Сортируем по слоям и глубине
        sortRenderQueue();
        
        // Рендерим каждый слой
        renderLayers();
        
        // Рендерим UI
        renderUI(gameState);
        
        profiler.endMethod("render");
    }
    
    private void collectRenderObjects(GameWorld world) {
        profiler.startMethod("collectRenderObjects");
        
        // Очищаем слои
        layers.values().forEach(List::clear);
        
        // Получаем все сущности в области видимости
        Rectangle viewport = camera.getViewport();
        List<GameEntity> visibleEntities = world.getEntitiesInRange(
            camera.getPosition(), camera.getViewDistance());
        
        for (GameEntity entity : visibleEntities) {
            if (entity.isVisible()) {
                RenderCommand command = createRenderCommand(entity);
                if (command != null) {
                    RenderLayer layer = determineRenderLayer(entity);
                    layers.get(layer).add(command);
                }
            }
        }
        
        profiler.endMethod("collectRenderObjects");
    }
    
    private RenderCommand createRenderCommand(GameEntity entity) {
        // Создаем команду рендеринга с учетом LOD
        double distance = camera.getPosition().distanceTo(entity.getPosition());
        RenderQuality quality = determineRenderQuality(distance);
        
        return new RenderCommand(entity, quality);
    }
    
    private RenderLayer determineRenderLayer(GameEntity entity) {
        if (entity instanceof Terrain) return RenderLayer.TERRAIN;
        if (entity instanceof Building) return RenderLayer.BUILDINGS;
        if (entity instanceof Unit) return RenderLayer.UNITS;
        if (entity instanceof Effect) return RenderLayer.EFFECTS;
        return RenderLayer.DEFAULT;
    }
    
    private RenderQuality determineRenderQuality(double distance) {
        if (distance < 100) return RenderQuality.HIGH;
        if (distance < 300) return RenderQuality.MEDIUM;
        return RenderQuality.LOW;
    }
    
    private void sortRenderQueue() {
        profiler.startMethod("sortRenderQueue");
        
        // Сортируем каждый слой по глубине
        for (List<RenderCommand> layer : layers.values()) {
            layer.sort(Comparator.comparingInt(cmd -> 
                cmd.getEntity().getPosition().getZ()));
        }
        
        profiler.endMethod("sortRenderQueue");
    }
    
    private void renderLayers() {
        profiler.startMethod("renderLayers");
        
        // Рендерим слои в порядке приоритета
        RenderLayer[] layerOrder = {
            RenderLayer.TERRAIN,
            RenderLayer.BUILDINGS,
            RenderLayer.UNITS,
            RenderLayer.EFFECTS,
            RenderLayer.DEFAULT
        };
        
        for (RenderLayer layer : layerOrder) {
            List<RenderCommand> commands = layers.get(layer);
            for (RenderCommand command : commands) {
                renderEntity(command);
            }
        }
        
        profiler.endMethod("renderLayers");
    }
    
    private void renderEntity(RenderCommand command) {
        GameEntity entity = command.getEntity();
        RenderQuality quality = command.getQuality();
        
        // Применяем шейдеры если нужно
        if (quality == RenderQuality.HIGH) {
            shaderManager.applyShader("high_quality");
        }
        
        // Рендерим с учетом качества
        switch (quality) {
            case HIGH:
                renderHighQuality(entity);
                break;
            case MEDIUM:
                renderMediumQuality(entity);
                break;
            case LOW:
                renderLowQuality(entity);
                break;
        }
        
        // Сбрасываем шейдеры
        shaderManager.resetShader();
    }
    
    private void renderHighQuality(GameEntity entity) {
        Position pos = entity.getPosition();
        Point screenPos = camera.worldToScreen(pos);
        
        // Рендерим с высоким качеством
        if (entity instanceof Unit) {
            renderUnitHighQuality((Unit) entity, screenPos);
        } else if (entity instanceof Building) {
            renderBuildingHighQuality((Building) entity, screenPos);
        }
    }
    
    private void renderMediumQuality(GameEntity entity) {
        Position pos = entity.getPosition();
        Point screenPos = camera.worldToScreen(pos);
        
        // Рендерим со средним качеством
        if (entity instanceof Unit) {
            renderUnitMediumQuality((Unit) entity, screenPos);
        } else if (entity instanceof Building) {
            renderBuildingMediumQuality((Building) entity, screenPos);
        }
    }
    
    private void renderLowQuality(GameEntity entity) {
        Position pos = entity.getPosition();
        Point screenPos = camera.worldToScreen(pos);
        
        // Рендерим с низким качеством
        if (entity instanceof Unit) {
            renderUnitLowQuality((Unit) entity, screenPos);
        } else if (entity instanceof Building) {
            renderBuildingLowQuality((Building) entity, screenPos);
        }
    }
    
    private void renderUnitHighQuality(Unit unit, Point screenPos) {
        // Рендерим юнита с высоким качеством
        graphics.setColor(unit.getTeam() == 1 ? Color.BLUE : Color.RED);
        graphics.fillOval(screenPos.x - 15, screenPos.y - 15, 30, 30);
        
        // Рендерим имя
        graphics.setColor(Color.WHITE);
        graphics.setFont(new Font("Arial", Font.BOLD, 12));
        graphics.drawString(unit.getName(), screenPos.x - 20, screenPos.y - 20);
        
        // Рендерим полосу здоровья
        renderHealthBar(unit, screenPos);
    }
    
    private void renderUnitMediumQuality(Unit unit, Point screenPos) {
        // Рендерим юнита со средним качеством
        graphics.setColor(unit.getTeam() == 1 ? Color.BLUE : Color.RED);
        graphics.fillOval(screenPos.x - 12, screenPos.y - 12, 24, 24);
        
        // Рендерим полосу здоровья
        renderHealthBar(unit, screenPos);
    }
    
    private void renderUnitLowQuality(Unit unit, Point screenPos) {
        // Рендерим юнита с низким качеством
        graphics.setColor(unit.getTeam() == 1 ? Color.BLUE : Color.RED);
        graphics.fillOval(screenPos.x - 8, screenPos.y - 8, 16, 16);
    }
    
    private void renderHealthBar(Unit unit, Point screenPos) {
        int barWidth = 30;
        int barHeight = 4;
        int barX = screenPos.x - barWidth / 2;
        int barY = screenPos.y + 20;
        
        // Фон полосы здоровья
        graphics.setColor(Color.BLACK);
        graphics.fillRect(barX, barY, barWidth, barHeight);
        
        // Полоса здоровья
        double healthPercent = (double) unit.getCurrentHealth() / unit.getMaxHealth();
        int healthWidth = (int) (barWidth * healthPercent);
        
        if (healthPercent > 0.7) {
            graphics.setColor(Color.GREEN);
        } else if (healthPercent > 0.3) {
            graphics.setColor(Color.YELLOW);
        } else {
            graphics.setColor(Color.RED);
        }
        
        graphics.fillRect(barX, barY, healthWidth, barHeight);
    }
    
    private void renderBuildingHighQuality(Building building, Point screenPos) {
        // Рендерим здание с высоким качеством
        graphics.setColor(new Color(139, 69, 19)); // Коричневый
        graphics.fillRect(screenPos.x - 20, screenPos.y - 20, 40, 40);
        
        // Рендерим название
        graphics.setColor(Color.WHITE);
        graphics.setFont(new Font("Arial", Font.BOLD, 10));
        graphics.drawString(building.getName(), screenPos.x - 25, screenPos.y - 25);
        
        // Рендерим уровень
        graphics.drawString("Ур. " + building.getLevel(), screenPos.x - 15, screenPos.y + 35);
    }
    
    private void renderBuildingMediumQuality(Building building, Point screenPos) {
        // Рендерим здание со средним качеством
        graphics.setColor(new Color(139, 69, 19));
        graphics.fillRect(screenPos.x - 16, screenPos.y - 16, 32, 32);
    }
    
    private void renderBuildingLowQuality(Building building, Point screenPos) {
        // Рендерим здание с низким качеством
        graphics.setColor(new Color(139, 69, 19));
        graphics.fillRect(screenPos.x - 12, screenPos.y - 12, 24, 24);
    }
    
    private void renderUI(GameState gameState) {
        // Рендерим пользовательский интерфейс
        renderMinimap();
        renderResourcePanel();
        renderUnitInfo();
        renderTurnInfo(gameState);
    }
    
    private void renderMinimap() {
        // Рендерим мини-карту
        int minimapSize = 150;
        int minimapX = buffer.getWidth() - minimapSize - 10;
        int minimapY = 10;
        
        graphics.setColor(new Color(0, 0, 0, 128));
        graphics.fillRect(minimapX, minimapY, minimapSize, minimapSize);
        
        graphics.setColor(Color.WHITE);
        graphics.drawRect(minimapX, minimapY, minimapSize, minimapSize);
    }
    
    private void renderResourcePanel() {
        // Рендерим панель ресурсов
        int panelX = 10;
        int panelY = 10;
        int panelWidth = 200;
        int panelHeight = 100;
        
        graphics.setColor(new Color(0, 0, 0, 128));
        graphics.fillRect(panelX, panelY, panelWidth, panelHeight);
        
        graphics.setColor(Color.WHITE);
        graphics.setFont(new Font("Arial", Font.BOLD, 14));
        graphics.drawString("Ресурсы:", panelX + 10, panelY + 25);
        graphics.drawString("Золото: 1000", panelX + 10, panelY + 45);
        graphics.drawString("Дерево: 500", panelX + 10, panelY + 65);
        graphics.drawString("Камень: 300", panelX + 10, panelY + 85);
    }
    
    private void renderUnitInfo() {
        // Рендерим информацию о выбранном юните
        // (если есть выбранный юнит)
    }
    
    private void renderTurnInfo(GameState gameState) {
        // Рендерим информацию о ходе
        int infoX = buffer.getWidth() / 2 - 50;
        int infoY = 30;
        
        graphics.setColor(new Color(0, 0, 0, 128));
        graphics.fillRect(infoX, infoY, 100, 30);
        
        graphics.setColor(Color.WHITE);
        graphics.setFont(new Font("Arial", Font.BOLD, 16));
        graphics.drawString("Ход: " + gameState.getCurrentTurn(), infoX + 10, infoY + 20);
    }
    
    public BufferedImage getBuffer() { return buffer; }
    public Camera getCamera() { return camera; }
    public PerformanceProfiler getProfiler() { return profiler; }
    
    public enum RenderLayer {
        TERRAIN, BUILDINGS, UNITS, EFFECTS, DEFAULT
    }
    
    public enum RenderQuality {
        HIGH, MEDIUM, LOW
    }
}
```

#### 3. Главный класс игры
```java
/**
 * Главный класс игры "Королевство"
 */
public class Main {
    private static GameEngine gameEngine;
    private static GameWindow gameWindow;
    private static GameController gameController;
    
    public static void main(String[] args) {
        try {
            // Инициализируем игру
            initializeGame();
            
            // Запускаем игровой цикл
            startGame();
            
        } catch (Exception e) {
            System.err.println("Критическая ошибка при запуске игры:");
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    private static void initializeGame() {
        System.out.println("Инициализация игры 'Королевство'...");
        
        // Создаем основные компоненты
        gameEngine = new GameEngine();
        gameWindow = new GameWindow("Королевство - Стратегическая игра", 1200, 800);
        gameController = new GameController(gameEngine, gameWindow);
        
        // Настраиваем связи между компонентами
        setupComponentConnections();
        
        // Загружаем ресурсы
        loadResources();
        
        // Создаем начальное состояние игры
        createInitialGameState();
        
        System.out.println("Игра инициализирована успешно");
    }
    
    private static void setupComponentConnections() {
        // Подписываем окно на события движка
        gameEngine.getEventManager().subscribe(gameWindow, GameEvent.class);
        
        // Подписываем контроллер на события окна
        gameWindow.getEventManager().subscribe(gameController, WindowEvent.class);
        
        // Подписываем контроллер на события ввода
        gameWindow.getInputManager().subscribe(gameController, InputEvent.class);
    }
    
    private static void loadResources() {
        System.out.println("Загрузка ресурсов...");
        
        try {
            // Загружаем текстуры
            gameEngine.getTextureManager().loadTextures();
            
            // Загружаем звуки
            gameEngine.getAudioManager().loadSounds();
            
            // Загружаем конфигурацию
            loadConfiguration();
            
        } catch (Exception e) {
            System.err.println("Ошибка загрузки ресурсов: " + e.getMessage());
            // Продолжаем без некоторых ресурсов
        }
    }
    
    private static void loadConfiguration() {
        try {
            Properties config = new Properties();
            config.load(new FileInputStream("config/game.properties"));
            
            // Загружаем настройки игры
            GameConfig.setMaxPlayers(Integer.parseInt(config.getProperty("game.maxPlayers", "4")));
            GameConfig.setTurnTime(Long.parseLong(config.getProperty("game.turnTime", "30000")));
            GameConfig.setMaxUnits(Integer.parseInt(config.getProperty("game.maxUnits", "100")));
            
        } catch (IOException e) {
            System.err.println("Ошибка загрузки конфигурации: " + e.getMessage());
            // Используем значения по умолчанию
        }
    }
    
    private static void createInitialGameState() {
        System.out.println("Создание начального состояния игры...");
        
        GameWorld world = gameEngine.getGameWorld();
        
        // Создаем игрока
        Player player = new Player("Игрок", 1);
        world.addPlayer(player);
        
        // Создаем начальные ресурсы
        ResourceManager resources = world.getResourceManager();
        resources.addResource(new BasicResource("Золото", 1000));
        resources.addResource(new BasicResource("Дерево", 500));
        resources.addResource(new BasicResource("Камень", 300));
        resources.addResource(new BasicResource("Еда", 200));
        
        // Создаем начальные здания
        BuildingManager buildings = world.getBuildingManager();
        Position townHallPos = new Position(500, 500);
        Building townHall = buildings.createBuilding("TownHall", townHallPos);
        world.addEntity(townHall);
        
        // Создаем начальные юниты
        UnitManager units = world.getUnitManager();
        Position warriorPos = new Position(510, 510);
        Unit warrior = units.createUnit("Warrior", warriorPos);
        warrior.setTeam(1);
        world.addEntity(warrior);
        
        // Создаем ИИ противника
        createAIOpponent(world);
        
        System.out.println("Начальное состояние игры создано");
    }
    
    private static void createAIOpponent(GameWorld world) {
        Player aiPlayer = new AIPlayer("ИИ Противник", 2);
        world.addPlayer(aiPlayer);
        
        // Создаем ИИ здания и юниты
        BuildingManager buildings = world.getBuildingManager();
        UnitManager units = world.getUnitManager();
        
        // ИИ база
        Position aiBasePos = new Position(100, 100);
        Building aiBase = buildings.createBuilding("Barracks", aiBasePos);
        aiBase.setTeam(2);
        world.addEntity(aiBase);
        
        // ИИ юниты
        for (int i = 0; i < 3; i++) {
            Position unitPos = new Position(110 + i * 20, 110 + i * 20);
            Unit aiUnit = units.createUnit("Warrior", unitPos);
            aiUnit.setTeam(2);
            world.addEntity(aiUnit);
        }
    }
    
    private static void startGame() {
        System.out.println("Запуск игры...");
        
        // Запускаем игровой движок
        gameEngine.start();
        
        // Показываем главное окно
        gameWindow.setVisible(true);
        
        // Запускаем игровой цикл
        gameController.startGameLoop();
        
        System.out.println("Игра запущена");
    }
    
    public static void shutdown() {
        System.out.println("Завершение работы игры...");
        
        try {
            // Останавливаем игровой движок
            if (gameEngine != null) {
                gameEngine.stop();
            }
            
            // Закрываем окно
            if (gameWindow != null) {
                gameWindow.dispose();
            }
            
            // Сохраняем игру если нужно
            if (gameController != null) {
                gameController.saveGameIfNeeded();
            }
            
        } catch (Exception e) {
            System.err.println("Ошибка при завершении работы: " + e.getMessage());
        }
        
        System.out.println("Игра завершена");
        System.exit(0);
    }
    
    // Обработчик завершения работы
    static {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            shutdown();
        }));
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и создать аналогичный финальный проект:

### 1. **Гонки на выживание**
- Архитектура: физический движок, система коллизий, ИИ гонщиков
- Компоненты: автомобили, трассы, препятствия, система очков
- Особенности: 3D графика, физика движения, многопользовательские гонки

### 2. **Космическая колонизация**
- Архитектура: процедурная генерация, экономическая система, ИИ цивилизаций
- Компоненты: планеты, корабли, колонии, технологии
- Особенности: открытый космос, торговля, дипломатия

### 3. **Подземелье и драконы**
- Архитектура: генерация уровней, ролевая система, боевая механика
- Компоненты: персонажи, монстры, заклинания, квесты
- Особенности: процедурная генерация, система опыта, инвентарь

### 4. **Город-государство**
- Архитектура: симуляция агентов, экономическая модель, политическая система
- Компоненты: горожане, здания, экономика, события
- Особенности: детальная симуляция, историческая точность

### 5. **Пиратская стратегия**
- Архитектура: морская физика, торговая система, навигация
- Компоненты: корабли, острова, сокровища, торговые маршруты
- Особенности: открытый мир, морские сражения, исследование

### 6. **Фермерское хозяйство**
- Архитектура: симуляция природы, экономическая система, сезонность
- Компоненты: фермы, животные, культуры, рынок
- Особенности: реалистичная симуляция, сезонные циклы

### 7. **Киберпанк-тактика**
- Архитектура: сетевые системы, цифровая экономика, хакерские механики
- Компоненты: хакеры, сети, программы, корпорации
- Особенности: кибернетические операции, цифровые сражения

### 8. **Средневековая осада**
- Архитектура: военная тактика, физика осадных орудий, ИИ армий
- Компоненты: замки, армии, орудия, укрепления
- Особенности: историческая точность, тактические сражения

### 9. **Зомби-выживание**
- Архитектура: ИИ зомби, система выживания, процедурная генерация
- Компоненты: зомби, убежища, ресурсы, оружие
- Особенности: выживание, строительство, исследование

### 10. **Фэнтези-война**
- Архитектура: магическая система, боевая механика, ИИ существ
- Компоненты: маги, существа, заклинания, армии
- Особенности: магические эффекты, эпические сражения

## Требования к реализации

### Обязательные требования:
1. **Полноценная игра** с основным геймплеем
2. **Архитектура MVC** с разделением ответственности
3. **Многопоточность** для производительности
4. **Сетевые технологии** для многопользовательской игры
5. **Система тестирования** с покрытием кода
6. **Полная документация** проекта

### Дополнительные требования:
1. **Оптимизация производительности** и управление памятью
2. **Система модификаций** и расширений
3. **Интернационализация** и локализация
4. **Система достижений** и статистики
5. **Интеграция с базой данных** для сохранений

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Функциональность** | 4 | Полнота реализации игры |
| **Архитектура** | 4 | Качество архитектуры и дизайна |
| **Многопоточность** | 3 | Эффективное использование потоков |
| **Сетевое взаимодействие** | 3 | Многопользовательские возможности |
| **Тестирование** | 2 | Качество и покрытие тестов |
| **Документация** | 2 | Полнота и качество документации |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Как спланировать архитектуру большой игры?
2. Как организовать многопоточность в игровом движке?
3. Как реализовать сетевую игру?
4. Как тестировать сложные игровые системы?
5. Как оптимизировать производительность игры?
6. Как организовать командную разработку игры?

## Заключение

В данной лабораторной работе вы создали полноценную игру, применяя все изученные концепции ООП, паттерны проектирования, многопоточность и современные практики разработки. Вы научились:

- Планировать и реализовывать сложную архитектуру игры
- Эффективно использовать многопоточность
- Создавать сетевые компоненты
- Тестировать и оптимизировать игровые системы
- Документировать и поддерживать код

Полученные знания позволят вам создавать профессиональные игровые проекты.
