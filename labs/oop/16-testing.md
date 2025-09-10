# Лабораторная работа 16: Тестирование

## Цель работы
Изучить методы тестирования в Java на примере разработки игровых систем. Научиться создавать unit-тесты, integration-тесты, использовать моки и стабы, а также применять TDD подход к разработке.

## Теоретические основы

### Тестирование в Java
- **Unit-тесты** - тестирование отдельных компонентов
- **Integration-тесты** - тестирование взаимодействия компонентов
- **Mock-объекты** - имитация зависимостей
- **Stub-объекты** - упрощенные реализации
- **TDD** - Test-Driven Development
- **JUnit** - фреймворк для тестирования

### Типы тестирования
- **Функциональное** - проверка функциональности
- **Нагрузочное** - проверка производительности
- **Регрессионное** - проверка отсутствия регрессий
- **Покрытие кода** - измерение покрытия тестами

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Тестирование игровых систем

#### 1. Тестирование базовых классов
```java
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GameEntityTest {
    
    private GameEntity gameEntity;
    
    @BeforeEach
    void setUp() {
        gameEntity = new TestGameEntity(new Position(10, 20), "TestEntity");
    }
    
    @Test
    @DisplayName("Создание сущности с корректными параметрами")
    void testCreateEntity() {
        assertNotNull(gameEntity);
        assertEquals("TestEntity", gameEntity.getName());
        assertEquals(new Position(10, 20), gameEntity.getPosition());
        assertTrue(gameEntity.isAlive());
        assertEquals(100, gameEntity.getCurrentHealth());
        assertEquals(100, gameEntity.getMaxHealth());
    }
    
    @Test
    @DisplayName("Изменение позиции сущности")
    void testChangePosition() {
        Position newPosition = new Position(30, 40);
        gameEntity.setPosition(newPosition);
        
        assertEquals(newPosition, gameEntity.getPosition());
    }
    
    @Test
    @DisplayName("Получение урона")
    void testTakeDamage() {
        int initialHealth = gameEntity.getCurrentHealth();
        int damage = 30;
        
        gameEntity.takeDamage(damage);
        
        assertEquals(initialHealth - damage, gameEntity.getCurrentHealth());
        assertTrue(gameEntity.isAlive());
    }
    
    @Test
    @DisplayName("Смерть сущности от критического урона")
    void testEntityDeath() {
        int lethalDamage = gameEntity.getMaxHealth() + 10;
        
        gameEntity.takeDamage(lethalDamage);
        
        assertEquals(0, gameEntity.getCurrentHealth());
        assertFalse(gameEntity.isAlive());
    }
    
    @Test
    @DisplayName("Исцеление сущности")
    void testHealEntity() {
        // Сначала наносим урон
        gameEntity.takeDamage(50);
        assertEquals(50, gameEntity.getCurrentHealth());
        
        // Исцеляем
        gameEntity.heal(30);
        assertEquals(80, gameEntity.getCurrentHealth());
        
        // Исцеление не должно превышать максимум
        gameEntity.heal(50);
        assertEquals(100, gameEntity.getCurrentHealth());
    }
    
    @Test
    @DisplayName("Проверка границ позиции")
    void testPositionBounds() {
        // Тестируем граничные значения
        assertThrows(IllegalArgumentException.class, () -> {
            gameEntity.setPosition(new Position(-1, 0));
        });
        
        assertThrows(IllegalArgumentException.class, () -> {
            gameEntity.setPosition(new Position(0, -1));
        });
        
        // Корректные позиции
        assertDoesNotThrow(() -> {
            gameEntity.setPosition(new Position(0, 0));
            gameEntity.setPosition(new Position(1000, 1000));
        });
    }
    
    // Вспомогательный класс для тестирования
    private static class TestGameEntity extends GameEntity {
        public TestGameEntity(Position position, String name) {
            super(position, name);
        }
        
        @Override
        public void update() {
            // Простая реализация для тестирования
        }
        
        @Override
        public boolean isVisible() {
            return true;
        }
        
        @Override
        public int getPosition() {
            return 0;
        }
    }
}

@ExtendWith(MockitoExtension.class)
class PositionTest {
    
    @Test
    @DisplayName("Создание позиции")
    void testCreatePosition() {
        Position pos = new Position(15, 25);
        
        assertEquals(15, pos.getX());
        assertEquals(25, pos.getY());
        assertEquals(0, pos.getZ());
    }
    
    @Test
    @DisplayName("Расчет расстояния между позициями")
    void testDistanceCalculation() {
        Position pos1 = new Position(0, 0);
        Position pos2 = new Position(3, 4);
        
        double distance = pos1.distanceTo(pos2);
        assertEquals(5.0, distance, 0.001);
    }
    
    @Test
    @DisplayName("Сложение позиций")
    void testPositionAddition() {
        Position pos1 = new Position(10, 20);
        Position pos2 = new Position(5, 15);
        
        Position result = pos1.add(pos2);
        assertEquals(new Position(15, 35), result);
    }
    
    @Test
    @DisplayName("Проверка равенства позиций")
    void testPositionEquality() {
        Position pos1 = new Position(10, 20);
        Position pos2 = new Position(10, 20);
        Position pos3 = new Position(20, 10);
        
        assertEquals(pos1, pos2);
        assertNotEquals(pos1, pos3);
        assertEquals(pos1.hashCode(), pos2.hashCode());
    }
}
```

#### 2. Тестирование игрового мира
```java
@ExtendWith(MockitoExtension.class)
class GameWorldTest {
    
    private GameWorld gameWorld;
    
    @Mock
    private GameEntity mockEntity1;
    
    @Mock
    private GameEntity mockEntity2;
    
    @BeforeEach
    void setUp() {
        gameWorld = new GameWorld();
        
        // Настраиваем моки
        when(mockEntity1.getPosition()).thenReturn(new Position(10, 20));
        when(mockEntity1.getName()).thenReturn("MockEntity1");
        when(mockEntity1.isAlive()).thenReturn(true);
        
        when(mockEntity2.getPosition()).thenReturn(new Position(30, 40));
        when(mockEntity2.getName()).thenReturn("MockEntity2");
        when(mockEntity2.isAlive()).thenReturn(true);
    }
    
    @Test
    @DisplayName("Добавление сущности в мир")
    void testAddEntity() {
        gameWorld.addEntity(mockEntity1);
        
        assertEquals(1, gameWorld.getEntityCount());
        assertTrue(gameWorld.containsEntity(mockEntity1));
    }
    
    @Test
    @DisplayName("Удаление сущности из мира")
    void testRemoveEntity() {
        gameWorld.addEntity(mockEntity1);
        gameWorld.addEntity(mockEntity2);
        
        assertEquals(2, gameWorld.getEntityCount());
        
        gameWorld.removeEntity(mockEntity1);
        
        assertEquals(1, gameWorld.getEntityCount());
        assertFalse(gameWorld.containsEntity(mockEntity1));
        assertTrue(gameWorld.containsEntity(mockEntity2));
    }
    
    @Test
    @DisplayName("Поиск сущности по позиции")
    void testFindEntityAtPosition() {
        gameWorld.addEntity(mockEntity1);
        
        GameEntity found = gameWorld.getEntityAt(10, 20);
        assertEquals(mockEntity1, found);
        
        GameEntity notFound = gameWorld.getEntityAt(50, 50);
        assertNull(notFound);
    }
    
    @Test
    @DisplayName("Получение сущностей в радиусе")
    void testGetEntitiesInRange() {
        gameWorld.addEntity(mockEntity1);
        gameWorld.addEntity(mockEntity2);
        
        List<GameEntity> entities = gameWorld.getEntitiesInRange(new Position(15, 25), 10);
        
        assertEquals(1, entities.size());
        assertTrue(entities.contains(mockEntity1));
        assertFalse(entities.contains(mockEntity2));
    }
    
    @Test
    @DisplayName("Обновление игрового мира")
    void testUpdateWorld() {
        gameWorld.addEntity(mockEntity1);
        gameWorld.addEntity(mockEntity2);
        
        gameWorld.update();
        
        // Проверяем, что update был вызван для всех сущностей
        verify(mockEntity1).update();
        verify(mockEntity2).update();
    }
    
    @Test
    @DisplayName("Очистка мертвых сущностей")
    void testCleanupDeadEntities() {
        when(mockEntity1.isAlive()).thenReturn(false);
        when(mockEntity2.isAlive()).thenReturn(true);
        
        gameWorld.addEntity(mockEntity1);
        gameWorld.addEntity(mockEntity2);
        
        assertEquals(2, gameWorld.getEntityCount());
        
        gameWorld.cleanup();
        
        assertEquals(1, gameWorld.getEntityCount());
        assertFalse(gameWorld.containsEntity(mockEntity1));
        assertTrue(gameWorld.containsEntity(mockEntity2));
    }
    
    @Test
    @DisplayName("Проверка границ мира")
    void testWorldBounds() {
        // Тестируем граничные значения
        assertThrows(IllegalArgumentException.class, () -> {
            gameWorld.setSize(-1, 100);
        });
        
        assertThrows(IllegalArgumentException.class, () -> {
            gameWorld.setSize(100, -1);
        });
        
        // Корректные размеры
        assertDoesNotThrow(() -> {
            gameWorld.setSize(1000, 1000);
        });
        
        assertEquals(1000, gameWorld.getWidth());
        assertEquals(1000, gameWorld.getHeight());
    }
}
```

#### 3. Тестирование боевой системы
```java
@ExtendWith(MockitoExtension.class)
class CombatSystemTest {
    
    private CombatSystem combatSystem;
    
    @Mock
    private Unit attacker;
    
    @Mock
    private Unit defender;
    
    @Mock
    private CombatStrategy strategy;
    
    @BeforeEach
    void setUp() {
        combatSystem = new CombatSystem();
        
        // Настраиваем моки для атакующего
        when(attacker.getName()).thenReturn("Attacker");
        when(attacker.getPosition()).thenReturn(new Position(0, 0));
        when(attacker.getAttackPower()).thenReturn(50);
        when(attacker.getCurrentHealth()).thenReturn(100);
        when(attacker.isAlive()).thenReturn(true);
        
        // Настраиваем моки для защищающегося
        when(defender.getName()).thenReturn("Defender");
        when(defender.getPosition()).thenReturn(new Position(1, 0));
        when(defender.getDefensePower()).thenReturn(20);
        when(defender.getCurrentHealth()).thenReturn(80);
        when(defender.isAlive()).thenReturn(true);
        
        // Настраиваем мок стратегии
        when(strategy.calculateDamage(anyInt(), anyInt())).thenReturn(30);
    }
    
    @Test
    @DisplayName("Атака в пределах дистанции")
    void testAttackInRange() {
        when(attacker.canAttack(defender)).thenReturn(true);
        
        CombatResult result = combatSystem.attack(attacker, defender, strategy);
        
        assertNotNull(result);
        assertTrue(result.isSuccessful());
        assertEquals(30, result.getDamageDealt());
        assertEquals(50, result.getDefenderHealthAfter());
        
        verify(defender).takeDamage(30);
    }
    
    @Test
    @DisplayName("Атака вне дистанции")
    void testAttackOutOfRange() {
        when(attacker.canAttack(defender)).thenReturn(false);
        
        CombatResult result = combatSystem.attack(attacker, defender, strategy);
        
        assertNotNull(result);
        assertFalse(result.isSuccessful());
        assertEquals("Цель вне дистанции атаки", result.getFailureReason());
        
        verify(defender, never()).takeDamage(anyInt());
    }
    
    @Test
    @DisplayName("Атака мертвой цели")
    void testAttackDeadTarget() {
        when(defender.isAlive()).thenReturn(false);
        when(attacker.canAttack(defender)).thenReturn(true);
        
        CombatResult result = combatSystem.attack(attacker, defender, strategy);
        
        assertNotNull(result);
        assertFalse(result.isSuccessful());
        assertEquals("Цель мертва", result.getFailureReason());
    }
    
    @Test
    @DisplayName("Критический удар")
    void testCriticalHit() {
        when(attacker.canAttack(defender)).thenReturn(true);
        when(strategy.calculateDamage(anyInt(), anyInt())).thenReturn(100);
        
        CombatResult result = combatSystem.attack(attacker, defender, strategy);
        
        assertNotNull(result);
        assertTrue(result.isSuccessful());
        assertTrue(result.isCritical());
        assertEquals(100, result.getDamageDealt());
        assertEquals(0, result.getDefenderHealthAfter());
        
        verify(defender).takeDamage(100);
    }
    
    @Test
    @DisplayName("Блокирование атаки")
    void testAttackBlocked() {
        when(attacker.canAttack(defender)).thenReturn(true);
        when(strategy.calculateDamage(anyInt(), anyInt())).thenReturn(0);
        
        CombatResult result = combatSystem.attack(attacker, defender, strategy);
        
        assertNotNull(result);
        assertTrue(result.isSuccessful());
        assertTrue(result.isBlocked());
        assertEquals(0, result.getDamageDealt());
        assertEquals(80, result.getDefenderHealthAfter());
    }
    
    @Test
    @DisplayName("Групповая атака")
    void testGroupAttack() {
        List<Unit> attackers = Arrays.asList(attacker, attacker);
        when(attacker.canAttack(defender)).thenReturn(true);
        when(strategy.calculateDamage(anyInt(), anyInt())).thenReturn(25);
        
        List<CombatResult> results = combatSystem.groupAttack(attackers, defender, strategy);
        
        assertEquals(2, results.size());
        assertTrue(results.stream().allMatch(CombatResult::isSuccessful));
        
        verify(defender, times(2)).takeDamage(25);
    }
}
```

#### 4. Тестирование системы ресурсов
```java
@ExtendWith(MockitoExtension.class)
class ResourceSystemTest {
    
    private ResourceManager resourceManager;
    
    @BeforeEach
    void setUp() {
        resourceManager = new ResourceManager();
    }
    
    @Test
    @DisplayName("Добавление ресурса")
    void testAddResource() {
        Resource gold = new BasicResource("Золото", 100);
        
        resourceManager.addResource(gold);
        
        assertEquals(100, resourceManager.getResourceAmount("Золото"));
        assertTrue(resourceManager.hasResource("Золото"));
    }
    
    @Test
    @DisplayName("Удаление ресурса")
    void testRemoveResource() {
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        resourceManager.removeResource("Золото", 30);
        
        assertEquals(70, resourceManager.getResourceAmount("Золото"));
    }
    
    @Test
    @DisplayName("Попытка удалить больше ресурса чем есть")
    void testRemoveMoreResourceThanAvailable() {
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        assertThrows(InsufficientResourceException.class, () -> {
            resourceManager.removeResource("Золото", 150);
        });
        
        assertEquals(100, resourceManager.getResourceAmount("Золото"));
    }
    
    @Test
    @DisplayName("Проверка достаточности ресурсов")
    void testCheckResourceSufficiency() {
        Resource gold = new BasicResource("Золото", 100);
        Resource wood = new BasicResource("Дерево", 50);
        
        resourceManager.addResource(gold);
        resourceManager.addResource(wood);
        
        assertTrue(resourceManager.hasEnoughResources("Золото", 80));
        assertFalse(resourceManager.hasEnoughResources("Золото", 120));
        assertTrue(resourceManager.hasEnoughResources("Дерево", 50));
    }
    
    @Test
    @DisplayName("Транзакция ресурсов")
    void testResourceTransaction() {
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        ResourceTransaction transaction = new ResourceTransaction();
        transaction.addResource("Золото", -30);
        
        resourceManager.executeTransaction(transaction);
        
        assertEquals(70, resourceManager.getResourceAmount("Золото"));
    }
    
    @Test
    @DisplayName("Производство ресурсов")
    void testResourceProduction() {
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        ProductionChain chain = new ProductionChain();
        chain.addStep("Золото", -10);
        chain.addStep("Дерево", 5);
        
        resourceManager.executeProduction(chain);
        
        assertEquals(90, resourceManager.getResourceAmount("Золото"));
        assertEquals(5, resourceManager.getResourceAmount("Дерево"));
    }
    
    @Test
    @DisplayName("Обновление цен ресурсов")
    void testResourcePriceUpdate() {
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        resourceManager.updateResourcePrice("Золото", 150);
        
        assertEquals(150, resourceManager.getResourcePrice("Золото"));
    }
    
    @Test
    @DisplayName("Экспорт/импорт ресурсов")
    void testResourceTrade() {
        ResourceManager otherManager = new ResourceManager();
        
        Resource gold = new BasicResource("Золото", 100);
        resourceManager.addResource(gold);
        
        TradeSystem tradeSystem = new TradeSystem();
        tradeSystem.trade(resourceManager, otherManager, "Золото", 50, 100);
        
        assertEquals(50, resourceManager.getResourceAmount("Золото"));
        assertEquals(50, otherManager.getResourceAmount("Золото"));
    }
}
```

#### 5. Тестирование с использованием моков
```java
@ExtendWith(MockitoExtension.class)
class GameManagerTest {
    
    @Mock
    private GameWorld mockWorld;
    
    @Mock
    private Player mockPlayer;
    
    @Mock
    private GameRules mockRules;
    
    @Mock
    private EventManager mockEventManager;
    
    private GameManager gameManager;
    
    @BeforeEach
    void setUp() {
        gameManager = new GameManager();
        gameManager.setWorld(mockWorld);
        gameManager.setPlayer(mockPlayer);
        gameManager.setRules(mockRules);
        gameManager.setEventManager(mockEventManager);
    }
    
    @Test
    @DisplayName("Инициализация игры")
    void testGameInitialization() {
        when(mockWorld.getEntityCount()).thenReturn(0);
        when(mockPlayer.getName()).thenReturn("TestPlayer");
        
        gameManager.initializeGame();
        
        verify(mockWorld).initialize();
        verify(mockPlayer).initialize();
        verify(mockRules).loadRules();
        verify(mockEventManager).start();
    }
    
    @Test
    @DisplayName("Запуск игрового цикла")
    void testGameLoop() {
        when(mockWorld.getEntityCount()).thenReturn(5);
        when(mockPlayer.isAlive()).thenReturn(true);
        
        gameManager.startGameLoop();
        
        verify(mockWorld, atLeastOnce()).update();
        verify(mockPlayer, atLeastOnce()).update();
    }
    
    @Test
    @DisplayName("Обработка игровых событий")
    void testGameEventHandling() {
        GameEvent event = new UnitMoveEvent(new Position(10, 20));
        
        gameManager.handleEvent(event);
        
        verify(mockEventManager).processEvent(event);
        verify(mockRules).validateEvent(event);
    }
    
    @Test
    @DisplayName("Сохранение игры")
    void testGameSave() {
        when(mockWorld.serialize()).thenReturn("world_data");
        when(mockPlayer.serialize()).thenReturn("player_data");
        
        gameManager.saveGame("save_file.dat");
        
        verify(mockWorld).serialize();
        verify(mockPlayer).serialize();
        // Проверяем, что данные были записаны в файл
    }
    
    @Test
    @DisplayName("Загрузка игры")
    void testGameLoad() {
        gameManager.loadGame("save_file.dat");
        
        verify(mockWorld).deserialize(anyString());
        verify(mockPlayer).deserialize(anyString());
    }
    
    @Test
    @DisplayName("Проверка победы")
    void testVictoryCondition() {
        when(mockRules.checkVictoryCondition(any())).thenReturn(true);
        
        boolean victory = gameManager.checkVictory();
        
        assertTrue(victory);
        verify(mockRules).checkVictoryCondition(any());
    }
    
    @Test
    @DisplayName("Проверка поражения")
    void testDefeatCondition() {
        when(mockRules.checkDefeatCondition(any())).thenReturn(true);
        
        boolean defeat = gameManager.checkDefeat();
        
        assertTrue(defeat);
        verify(mockRules).checkDefeatCondition(any());
    }
}
```

#### 6. Интеграционные тесты
```java
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class GameIntegrationTest {
    
    private GameWorld gameWorld;
    private Player player;
    private ResourceManager resourceManager;
    private BuildingManager buildingManager;
    private UnitManager unitManager;
    
    @BeforeAll
    void setUp() {
        gameWorld = new GameWorld();
        player = new Player("TestPlayer");
        resourceManager = new ResourceManager();
        buildingManager = new BuildingManager();
        unitManager = new UnitManager();
        
        // Инициализируем связи между компонентами
        gameWorld.setResourceManager(resourceManager);
        gameWorld.setBuildingManager(buildingManager);
        gameWorld.setUnitManager(unitManager);
    }
    
    @Test
    @DisplayName("Полный цикл строительства здания")
    void testCompleteBuildingCycle() {
        // 1. Проверяем начальные ресурсы
        resourceManager.addResource(new BasicResource("Золото", 1000));
        resourceManager.addResource(new BasicResource("Дерево", 500));
        
        assertEquals(1000, resourceManager.getResourceAmount("Золото"));
        assertEquals(500, resourceManager.getResourceAmount("Дерево"));
        
        // 2. Строим здание
        Building house = buildingManager.createBuilding("House", new Position(10, 10));
        assertNotNull(house);
        assertTrue(gameWorld.containsEntity(house));
        
        // 3. Проверяем затраты ресурсов
        assertEquals(900, resourceManager.getResourceAmount("Золото"));
        assertEquals(450, resourceManager.getResourceAmount("Дерево"));
        
        // 4. Проверяем, что здание работает
        assertTrue(house.isOperational());
        assertEquals(1, house.getLevel());
    }
    
    @Test
    @DisplayName("Полный цикл создания юнита")
    void testCompleteUnitCreation() {
        // 1. Создаем казарму
        Building barracks = buildingManager.createBuilding("Barracks", new Position(20, 20));
        assertNotNull(barracks);
        
        // 2. Создаем юнита
        Unit warrior = unitManager.createUnit("Warrior", new Position(21, 21));
        assertNotNull(warrior);
        assertTrue(gameWorld.containsEntity(warrior));
        
        // 3. Проверяем, что юнит может двигаться
        Position newPos = new Position(22, 22);
        warrior.moveTo(newPos);
        assertEquals(newPos, warrior.getPosition());
    }
    
    @Test
    @DisplayName("Взаимодействие между системами")
    void testSystemInteraction() {
        // 1. Создаем ферму
        Building farm = buildingManager.createBuilding("Farm", new Position(30, 30));
        
        // 2. Ферма производит еду
        resourceManager.addResource(new BasicResource("Еда", 0));
        
        // 3. Обновляем игровой мир
        gameWorld.update();
        
        // 4. Проверяем, что еда была произведена
        assertTrue(resourceManager.getResourceAmount("Еда") > 0);
    }
    
    @Test
    @DisplayName("Боевая система")
    void testCombatSystem() {
        // 1. Создаем двух юнитов
        Unit attacker = unitManager.createUnit("Warrior", new Position(40, 40));
        Unit defender = unitManager.createUnit("Archer", new Position(41, 40));
        
        // 2. Проверяем начальное здоровье
        int initialHealth = defender.getCurrentHealth();
        
        // 3. Атакуем
        CombatSystem combatSystem = new CombatSystem();
        CombatResult result = combatSystem.attack(attacker, defender, new MeleeCombatStrategy());
        
        // 4. Проверяем результат
        assertTrue(result.isSuccessful());
        assertTrue(defender.getCurrentHealth() < initialHealth);
    }
    
    @AfterEach
    void tearDown() {
        // Очищаем состояние после каждого теста
        gameWorld.clear();
        resourceManager.clear();
        buildingManager.clear();
        unitManager.clear();
    }
}
```

#### 7. Тестирование производительности
```java
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class PerformanceTest {
    
    private GameWorld gameWorld;
    private PerformanceProfiler profiler;
    
    @BeforeAll
    void setUp() {
        gameWorld = new GameWorld();
        profiler = new PerformanceProfiler();
    }
    
    @Test
    @DisplayName("Тест производительности добавления сущностей")
    void testEntityAdditionPerformance() {
        int entityCount = 10000;
        
        profiler.startMethod("addEntities");
        
        for (int i = 0; i < entityCount; i++) {
            GameEntity entity = new TestEntity(new Position(i % 100, i / 100), "Entity" + i);
            gameWorld.addEntity(entity);
        }
        
        profiler.endMethod("addEntities");
        
        assertEquals(entityCount, gameWorld.getEntityCount());
        
        // Проверяем, что добавление заняло разумное время
        long totalTime = profiler.getMethodTimings().get("addEntities_total");
        assertTrue(totalTime < 1000000000L); // Менее 1 секунды
    }
    
    @Test
    @DisplayName("Тест производительности поиска сущностей")
    void testEntitySearchPerformance() {
        // Создаем множество сущностей
        int entityCount = 5000;
        for (int i = 0; i < entityCount; i++) {
            GameEntity entity = new TestEntity(new Position(i % 100, i / 100), "Entity" + i);
            gameWorld.addEntity(entity);
        }
        
        profiler.startMethod("searchEntities");
        
        // Ищем сущности в радиусе
        for (int i = 0; i < 1000; i++) {
            Position searchPos = new Position(i % 100, i / 100);
            List<GameEntity> found = gameWorld.getEntitiesInRange(searchPos, 5);
        }
        
        profiler.endMethod("searchEntities");
        
        // Проверяем производительность поиска
        long totalTime = profiler.getMethodTimings().get("searchEntities_total");
        assertTrue(totalTime < 5000000000L); // Менее 5 секунд
    }
    
    @Test
    @DisplayName("Тест производительности обновления мира")
    void testWorldUpdatePerformance() {
        // Создаем множество сущностей
        int entityCount = 1000;
        for (int i = 0; i < entityCount; i++) {
            GameEntity entity = new TestEntity(new Position(i % 50, i / 50), "Entity" + i);
            gameWorld.addEntity(entity);
        }
        
        profiler.startMethod("updateWorld");
        
        // Обновляем мир несколько раз
        for (int i = 0; i < 100; i++) {
            gameWorld.update();
        }
        
        profiler.endMethod("updateWorld");
        
        // Проверяем производительность обновления
        long totalTime = profiler.getMethodTimings().get("updateWorld_total");
        assertTrue(totalTime < 1000000000L); // Менее 1 секунды
    }
    
    @Test
    @DisplayName("Тест использования памяти")
    void testMemoryUsage() {
        profiler.profileMemory("before");
        
        // Создаем множество сущностей
        int entityCount = 10000;
        for (int i = 0; i < entityCount; i++) {
            GameEntity entity = new TestEntity(new Position(i % 100, i / 100), "Entity" + i);
            gameWorld.addEntity(entity);
        }
        
        profiler.profileMemory("after");
        
        // Проверяем, что использование памяти разумно
        long beforeMemory = profiler.getMemoryUsage().get("before");
        long afterMemory = profiler.getMemoryUsage().get("after");
        long memoryIncrease = afterMemory - beforeMemory;
        
        // Увеличение памяти должно быть разумным (менее 100 МБ)
        assertTrue(memoryIncrease < 100 * 1024 * 1024);
    }
    
    private static class TestEntity extends GameEntity {
        public TestEntity(Position position, String name) {
            super(position, name);
        }
        
        @Override
        public void update() {
            // Простая реализация для тестирования
        }
        
        @Override
        public boolean isVisible() {
            return true;
        }
        
        @Override
        public int getPosition() {
            return 0;
        }
    }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичное тестирование:

### 1. **Гонки на выживание**
- Тесты: физика движения, коллизии, система очков
- Моки: трасса, препятствия, другие гонщики
- Интеграция: гонка от старта до финиша

### 2. **Космическая колонизация**
- Тесты: генерация планет, экономика, исследования
- Моки: галактика, инопланетные цивилизации
- Интеграция: полный цикл колонизации

### 3. **Подземелье и драконы**
- Тесты: генерация уровней, ИИ монстров, боевая система
- Моки: подземелье, NPC, квесты
- Интеграция: прохождение уровня

### 4. **Город-государство**
- Тесты: симуляция горожан, экономика, политика
- Моки: внешние государства, события
- Интеграция: развитие города

### 5. **Пиратская стратегия**
- Тесты: морские сражения, торговля, навигация
- Моки: другие пираты, торговые корабли
- Интеграция: пиратский рейд

### 6. **Фермерское хозяйство**
- Тесты: симуляция растений, животные, рынок
- Моки: погода, рыночные цены
- Интеграция: сельскохозяйственный цикл

### 7. **Киберпанк-тактика**
- Тесты: сетевые атаки, цифровая экономика, хакерские системы
- Моки: цифровые сети, корпорации
- Интеграция: кибернетическая операция

### 8. **Средневековая осада**
- Тесты: военные расчеты, осадные орудия, тактика
- Моки: вражеская армия, укрепления
- Интеграция: осада замка

### 9. **Зомби-выживание**
- Тесты: ИИ зомби, система выживания, строительство
- Моки: зомби, ресурсы, события
- Интеграция: выживание в течение дня

### 10. **Фэнтези-война**
- Тесты: магические эффекты, боевые расчеты, ИИ существ
- Моки: магические силы, вражеские армии
- Интеграция: магическое сражение

## Требования к реализации

### Обязательные требования:
1. **Создать unit-тесты** для всех основных классов
2. **Реализовать integration-тесты** для взаимодействия систем
3. **Использовать моки и стабы** для изоляции тестов
4. **Создать тесты производительности** для критических операций
5. **Демонстрировать покрытие кода** тестами
6. **Реализовать TDD подход** для новых функций

### Дополнительные требования:
1. **Добавить тесты безопасности** для критических операций
2. **Реализовать автоматизированные тесты** для CI/CD
3. **Создать тесты пользовательского интерфейса** (GUI тесты)
4. **Добавить тесты совместимости** с разными версиями Java

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Unit-тесты** | 4 | Тестирование отдельных компонентов |
| **Integration-тесты** | 3 | Тестирование взаимодействия систем |
| **Моки и стабы** | 3 | Изоляция тестов от зависимостей |
| **Тесты производительности** | 2 | Измерение производительности |
| **Покрытие кода** | 2 | Процент покрытия тестами |
| **TDD подход** | 2 | Разработка через тестирование |
| **Качество тестов** | 2 | Читаемость, покрытие, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужно тестирование в разработке игр?
2. В чем разница между unit и integration тестами?
3. Как использовать моки для изоляции тестов?
4. Как измерить покрытие кода тестами?
5. Что такое TDD и как его применять?
6. Как тестировать производительность?
7. Как организовать автоматизированное тестирование?

## Заключение

В данной лабораторной работе вы изучили методы тестирования в Java на примере создания игровых систем. Вы научились:

- Создавать unit-тесты для отдельных компонентов
- Реализовывать integration-тесты для взаимодействия систем
- Использовать моки и стабы для изоляции тестов
- Создавать тесты производительности
- Применять TDD подход к разработке
- Измерять покрытие кода тестами

Полученные знания позволят вам создавать надежные и качественные игровые приложения.
