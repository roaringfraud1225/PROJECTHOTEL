# Лабораторная работа 15: Продвинутый GUI

## Цель работы
Изучить создание продвинутых графических интерфейсов в Java на примере разработки игровых систем. Научиться создавать интерактивные элементы, анимации, пользовательские компоненты и современные интерфейсы.

## Теоретические основы

### Продвинутый GUI в Java
- **Swing и JavaFX** - современные библиотеки GUI
- **Пользовательские компоненты** - создание уникальных элементов
- **Анимации и эффекты** - плавные переходы и визуальные эффекты
- **Обработка событий** - реакция на действия пользователя
- **Макеты и компоновка** - гибкая организация интерфейса

### Принципы дизайна
- **Usability** - удобство использования
- **Responsiveness** - отзывчивость интерфейса
- **Visual Hierarchy** - визуальная иерархия
- **Consistency** - единообразие элементов
- **Accessibility** - доступность для всех пользователей

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Продвинутый GUI

#### 1. Главное окно игры
```java
public class KingdomGameWindow extends JFrame {
    private GamePanel gamePanel;
    private ControlPanel controlPanel;
    private InfoPanel infoPanel;
    private MenuBar menuBar;
    private StatusBar statusBar;
    
    public KingdomGameWindow() {
        setupWindow();
        createComponents();
        layoutComponents();
        setupEventHandlers();
        setupAnimations();
    }
    
    private void setupWindow() {
        setTitle("Королевство - Стратегическая игра");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(1200, 800);
        setLocationRelativeTo(null);
        setResizable(true);
        
        // Устанавливаем Look and Feel
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeel());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private void createComponents() {
        gamePanel = new GamePanel();
        controlPanel = new ControlPanel();
        infoPanel = new InfoPanel();
        menuBar = new MenuBar();
        statusBar = new StatusBar();
    }
    
    private void layoutComponents() {
        setJMenuBar(menuBar);
        
        // Основная панель с игровым полем
        JSplitPane mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, 
                                                 gamePanel, controlPanel);
        mainSplitPane.setDividerLocation(800);
        mainSplitPane.setResizeWeight(0.7);
        
        // Правая панель с информацией
        JSplitPane rightSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, 
                                                  mainSplitPane, infoPanel);
        rightSplitPane.setDividerLocation(600);
        
        add(rightSplitPane);
        add(statusBar, BorderLayout.SOUTH);
    }
    
    private void setupEventHandlers() {
        // Обработчики событий окна
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                saveGame();
            }
        });
        
        // Обработчики изменения размера
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                gamePanel.updateSize();
            }
        });
    }
    
    private void setupAnimations() {
        // Анимация появления окна
        setOpacity(0.0f);
        Timer fadeInTimer = new Timer(50, new ActionListener() {
            private float opacity = 0.0f;
            
            @Override
            public void actionPerformed(ActionEvent e) {
                opacity += 0.1f;
                if (opacity >= 1.0f) {
                    opacity = 1.0f;
                    ((Timer) e.getSource()).stop();
                }
                setOpacity(opacity);
            }
        });
        fadeInTimer.start();
    }
    
    private void saveGame() {
        int result = JOptionPane.showConfirmDialog(this, 
            "Сохранить игру перед выходом?", "Выход", 
            JOptionPane.YES_NO_CANCEL_OPTION);
        
        if (result == JOptionPane.YES_OPTION) {
            // Логика сохранения
            JOptionPane.showMessageDialog(this, "Игра сохранена!");
        } else if (result == JOptionPane.CANCEL_OPTION) {
            return; // Отменяем закрытие
        }
        
        dispose();
    }
    
    public void updateStatus(String message) {
        statusBar.updateStatus(message);
    }
    
    public void showInfo(String title, String message) {
        infoPanel.showInfo(title, message);
    }
}
```

#### 2. Игровая панель
```java
public class GamePanel extends JPanel {
    private GameWorld gameWorld;
    private GameRenderer renderer;
    private MouseHandler mouseHandler;
    private KeyboardHandler keyboardHandler;
    private AnimationManager animationManager;
    private List<GameEntity> selectedEntities;
    private Point lastMousePosition;
    
    public GamePanel() {
        initializeComponents();
        setupEventHandlers();
        setupAnimations();
    }
    
    private void initializeComponents() {
        setBackground(Color.DARK_GRAY);
        setPreferredSize(new Dimension(800, 600));
        setFocusable(true);
        
        gameWorld = new GameWorld();
        renderer = new GameRenderer();
        mouseHandler = new MouseHandler();
        keyboardHandler = new KeyboardHandler();
        animationManager = new AnimationManager();
        selectedEntities = new ArrayList<>();
        
        // Создаем тестовые сущности
        createTestEntities();
    }
    
    private void setupEventHandlers() {
        addMouseListener(mouseHandler);
        addMouseMotionListener(mouseHandler);
        addMouseWheelListener(mouseHandler);
        addKeyListener(keyboardHandler);
        
        // Обработчик перерисовки
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                updateSize();
            }
        });
    }
    
    private void setupAnimations() {
        // Таймер анимации
        Timer animationTimer = new Timer(16, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                animationManager.update();
                repaint();
            }
        });
        animationTimer.start();
    }
    
    private void createTestEntities() {
        // Создаем тестовые сущности
        for (int i = 0; i < 10; i++) {
            Position pos = new Position(50 + i * 60, 50 + i * 40);
            GameEntity entity = new TestEntity(pos, "Entity-" + i);
            gameWorld.addEntity(entity);
        }
    }
    
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        
        // Включаем сглаживание
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                             RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, 
                             RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        
        // Рендерим игровой мир
        renderer.render(g2d, gameWorld, getSize());
        
        // Рендерим выделенные сущности
        renderSelection(g2d);
        
        // Рендерим анимации
        animationManager.render(g2d);
    }
    
    private void renderSelection(Graphics2D g2d) {
        g2d.setColor(Color.YELLOW);
        g2d.setStroke(new BasicStroke(2.0f));
        
        for (GameEntity entity : selectedEntities) {
            Position pos = entity.getPosition();
            int x = (int) pos.getX();
            int y = (int) pos.getY();
            
            // Рисуем рамку выделения
            g2d.drawRect(x - 15, y - 15, 30, 30);
            
            // Рисуем диагонали
            g2d.drawLine(x - 15, y - 15, x + 15, y + 15);
            g2d.drawLine(x - 15, y + 15, x + 15, y - 15);
        }
    }
    
    public void updateSize() {
        // Обновляем размер игрового мира
        Dimension size = getSize();
        gameWorld.setSize(size.width, size.height);
    }
    
    public void selectEntity(GameEntity entity) {
        if (entity != null) {
            selectedEntities.clear();
            selectedEntities.add(entity);
            
            // Анимация выделения
            animationManager.addSelectionAnimation(entity);
            
            repaint();
        }
    }
    
    public void clearSelection() {
        selectedEntities.clear();
        repaint();
    }
    
    // Внутренние классы для обработки событий
    private class MouseHandler extends MouseAdapter {
        @Override
        public void mouseClicked(MouseEvent e) {
            requestFocusInWindow();
            
            Point mousePos = e.getPoint();
            GameEntity clickedEntity = gameWorld.getEntityAt(mousePos.x, mousePos.y);
            
            if (clickedEntity != null) {
                selectEntity(clickedEntity);
            } else {
                clearSelection();
            }
        }
        
        @Override
        public void mousePressed(MouseEvent e) {
            lastMousePosition = e.getPoint();
        }
        
        @Override
        public void mouseDragged(MouseEvent e) {
            if (lastMousePosition != null) {
                Point currentPos = e.getPoint();
                int deltaX = currentPos.x - lastMousePosition.x;
                int deltaY = currentPos.y - lastMousePosition.y;
                
                // Прокрутка игрового мира
                gameWorld.scroll(deltaX, deltaY);
                lastMousePosition = currentPos;
                repaint();
            }
        }
        
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            // Зум игрового мира
            int notches = e.getWheelRotation();
            gameWorld.zoom(notches > 0 ? 0.9 : 1.1);
            repaint();
        }
    }
    
    private class KeyboardHandler extends KeyAdapter {
        @Override
        public void keyPressed(KeyEvent e) {
            switch (e.getKeyCode()) {
                case KeyEvent.VK_ESCAPE:
                    clearSelection();
                    break;
                case KeyEvent.VK_DELETE:
                    deleteSelectedEntities();
                    break;
                case KeyEvent.VK_SPACE:
                    pauseGame();
                    break;
            }
        }
        
        private void deleteSelectedEntities() {
            for (GameEntity entity : selectedEntities) {
                gameWorld.removeEntity(entity);
            }
            selectedEntities.clear();
            repaint();
        }
        
        private void pauseGame() {
            // Логика паузы
            System.out.println("Игра поставлена на паузу");
        }
    }
}
```

#### 3. Панель управления
```java
public class ControlPanel extends JPanel {
    private JTabbedPane tabbedPane;
    private BuildingPanel buildingPanel;
    private UnitPanel unitPanel;
    private ResourcePanel resourcePanel;
    private ActionPanel actionPanel;
    
    public ControlPanel() {
        setPreferredSize(new Dimension(300, 600));
        setBorder(BorderFactory.createTitledBorder("Управление"));
        
        createComponents();
        layoutComponents();
        setupEventHandlers();
    }
    
    private void createComponents() {
        tabbedPane = new JTabbedPane();
        buildingPanel = new BuildingPanel();
        unitPanel = new UnitPanel();
        resourcePanel = new ResourcePanel();
        actionPanel = new ActionPanel();
    }
    
    private void layoutComponents() {
        setLayout(new BorderLayout());
        
        // Добавляем вкладки
        tabbedPane.addTab("Здания", new ImageIcon("icons/building.png"), buildingPanel);
        tabbedPane.addTab("Юниты", new ImageIcon("icons/unit.png"), unitPanel);
        tabbedPane.addTab("Ресурсы", new ImageIcon("icons/resource.png"), resourcePanel);
        tabbedPane.addTab("Действия", new ImageIcon("icons/action.png"), actionPanel);
        
        add(tabbedPane, BorderLayout.CENTER);
    }
    
    private void setupEventHandlers() {
        // Обработчик смены вкладок
        tabbedPane.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int selectedIndex = tabbedPane.getSelectedIndex();
                System.out.println("Выбрана вкладка: " + selectedIndex);
            }
        });
    }
    
    public void updateBuildingList(List<Building> buildings) {
        buildingPanel.updateBuildings(buildings);
    }
    
    public void updateUnitList(List<Unit> units) {
        unitPanel.updateUnits(units);
    }
    
    public void updateResources(Map<String, Integer> resources) {
        resourcePanel.updateResources(resources);
    }
}
```

#### 4. Панель зданий
```java
public class BuildingPanel extends JPanel {
    private JList<Building> buildingList;
    private DefaultListModel<Building> listModel;
    private JButton buildButton;
    private JButton upgradeButton;
    private JButton demolishButton;
    private BuildingInfoPanel infoPanel;
    
    public BuildingPanel() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        
        createComponents();
        layoutComponents();
        setupEventHandlers();
    }
    
    private void createComponents() {
        listModel = new DefaultListModel<>();
        buildingList = new JList<>(listModel);
        buildingList.setCellRenderer(new BuildingListRenderer());
        
        buildButton = new JButton("Строить");
        upgradeButton = new JButton("Улучшить");
        demolishButton = new JButton("Снести");
        
        infoPanel = new BuildingInfoPanel();
        
        // Настройка кнопок
        buildButton.setIcon(new ImageIcon("icons/build.png"));
        upgradeButton.setIcon(new ImageIcon("icons/upgrade.png"));
        demolishButton.setIcon(new ImageIcon("icons/demolish.png"));
    }
    
    private void layoutComponents() {
        // Панель с кнопками
        JPanel buttonPanel = new JPanel(new GridLayout(3, 1, 5, 5));
        buttonPanel.add(buildButton);
        buttonPanel.add(upgradeButton);
        buttonPanel.add(demolishButton);
        
        // Левая панель
        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.add(new JScrollPane(buildingList), BorderLayout.CENTER);
        leftPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        // Разделитель
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, 
                                             leftPanel, infoPanel);
        splitPane.setDividerLocation(150);
        splitPane.setResizeWeight(0.4);
        
        add(splitPane, BorderLayout.CENTER);
    }
    
    private void setupEventHandlers() {
        // Обработчик выбора здания
        buildingList.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    Building selectedBuilding = buildingList.getSelectedValue();
                    if (selectedBuilding != null) {
                        infoPanel.showBuilding(selectedBuilding);
                        updateButtonStates(selectedBuilding);
                    }
                }
            }
        });
        
        // Обработчики кнопок
        buildButton.addActionListener(e -> buildNewBuilding());
        upgradeButton.addActionListener(e -> upgradeSelectedBuilding());
        demolishButton.addActionListener(e -> demolishSelectedBuilding());
    }
    
    private void updateButtonStates(Building building) {
        upgradeButton.setEnabled(building.canUpgrade());
        demolishButton.setEnabled(building.canDemolish());
    }
    
    private void buildNewBuilding() {
        // Диалог выбора типа здания
        String[] buildingTypes = {"Жилой дом", "Ферма", "Казарма", "Магическая башня"};
        String selected = (String) JOptionPane.showInputDialog(this, 
            "Выберите тип здания:", "Строительство", 
            JOptionPane.QUESTION_MESSAGE, null, buildingTypes, buildingTypes[0]);
        
        if (selected != null) {
            System.out.println("Строится: " + selected);
        }
    }
    
    private void upgradeSelectedBuilding() {
        Building selectedBuilding = buildingList.getSelectedValue();
        if (selectedBuilding != null && selectedBuilding.canUpgrade()) {
            selectedBuilding.upgrade();
            infoPanel.showBuilding(selectedBuilding);
            updateButtonStates(selectedBuilding);
        }
    }
    
    private void demolishSelectedBuilding() {
        Building selectedBuilding = buildingList.getSelectedValue();
        if (selectedBuilding != null && selectedBuilding.canDemolish()) {
            int result = JOptionPane.showConfirmDialog(this, 
                "Уничтожить здание " + selectedBuilding.getName() + "?", 
                "Подтверждение", JOptionPane.YES_NO_OPTION);
            
            if (result == JOptionPane.YES_OPTION) {
                selectedBuilding.demolish();
                listModel.removeElement(selectedBuilding);
                infoPanel.clear();
            }
        }
    }
    
    public void updateBuildings(List<Building> buildings) {
        listModel.clear();
        for (Building building : buildings) {
            listModel.addElement(building);
        }
    }
    
    // Пользовательский рендерер для списка зданий
    private class BuildingListRenderer extends DefaultListCellRenderer {
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, 
                                                   int index, boolean isSelected, 
                                                   boolean cellHasFocus) {
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            
            if (value instanceof Building) {
                Building building = (Building) value;
                setText(building.getName() + " (Ур. " + building.getLevel() + ")");
                setIcon(new ImageIcon("icons/" + building.getType().toLowerCase() + ".png"));
                
                if (isSelected) {
                    setBackground(new Color(100, 150, 255));
                    setForeground(Color.WHITE);
                } else {
                    setBackground(Color.WHITE);
                    setForeground(Color.BLACK);
                }
            }
            
            return this;
        }
    }
}
```

#### 5. Панель информации о здании
```java
public class BuildingInfoPanel extends JPanel {
    private JLabel nameLabel;
    private JLabel typeLabel;
    private JLabel levelLabel;
    private JLabel healthLabel;
    private JProgressBar healthBar;
    private JTextArea descriptionArea;
    private JButton actionButton;
    
    public BuildingInfoPanel() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createTitledBorder("Информация"));
        
        createComponents();
        layoutComponents();
    }
    
    private void createComponents() {
        nameLabel = new JLabel("Название");
        typeLabel = new JLabel("Тип");
        levelLabel = new JLabel("Уровень");
        healthLabel = new JLabel("Здоровье");
        
        healthBar = new JProgressBar(0, 100);
        healthBar.setStringPainted(true);
        
        descriptionArea = new JTextArea();
        descriptionArea.setEditable(false);
        descriptionArea.setLineWrap(true);
        descriptionArea.setWrapStyleWord(true);
        
        actionButton = new JButton("Действие");
    }
    
    private void layoutComponents() {
        // Панель с основной информацией
        JPanel infoPanel = new JPanel(new GridLayout(4, 2, 5, 5));
        infoPanel.add(new JLabel("Название:"));
        infoPanel.add(nameLabel);
        infoPanel.add(new JLabel("Тип:"));
        infoPanel.add(typeLabel);
        infoPanel.add(new JLabel("Уровень:"));
        infoPanel.add(levelLabel);
        infoPanel.add(new JLabel("Здоровье:"));
        infoPanel.add(healthBar);
        
        // Панель с описанием
        JPanel descPanel = new JPanel(new BorderLayout());
        descPanel.add(new JLabel("Описание:"), BorderLayout.NORTH);
        descPanel.add(new JScrollPane(descriptionArea), BorderLayout.CENTER);
        
        // Панель с кнопкой действия
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(actionButton);
        
        // Компоновка
        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.add(infoPanel, BorderLayout.NORTH);
        topPanel.add(descPanel, BorderLayout.CENTER);
        topPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        add(topPanel, BorderLayout.CENTER);
    }
    
    public void showBuilding(Building building) {
        nameLabel.setText(building.getName());
        typeLabel.setText(building.getType());
        levelLabel.setText(String.valueOf(building.getLevel()));
        
        int healthPercent = (int)((double) building.getCurrentHealth() / 
                                  building.getMaxHealth() * 100);
        healthBar.setValue(healthPercent);
        healthBar.setString(healthPercent + "%");
        
        // Цвет полосы здоровья
        if (healthPercent > 70) {
            healthBar.setForeground(Color.GREEN);
        } else if (healthPercent > 30) {
            healthBar.setForeground(Color.YELLOW);
        } else {
            healthBar.setForeground(Color.RED);
        }
        
        descriptionArea.setText(building.getDescription());
        
        // Настройка кнопки действия
        actionButton.setText(building.getActionButtonText());
        actionButton.setEnabled(building.canPerformAction());
        
        // Обработчик кнопки действия
        actionButton.removeActionListeners();
        actionButton.addActionListener(e -> building.performAction());
    }
    
    public void clear() {
        nameLabel.setText("");
        typeLabel.setText("");
        levelLabel.setText("");
        healthBar.setValue(0);
        descriptionArea.setText("");
        actionButton.setText("");
        actionButton.setEnabled(false);
    }
}
```

#### 6. Менеджер анимаций
```java
public class AnimationManager {
    private List<Animation> animations;
    private Map<String, Animation> animationTemplates;
    
    public AnimationManager() {
        this.animations = new ArrayList<>();
        this.animationTemplates = new HashMap<>();
        
        createAnimationTemplates();
    }
    
    private void createAnimationTemplates() {
        // Анимация выделения
        animationTemplates.put("selection", new SelectionAnimation());
        
        // Анимация строительства
        animationTemplates.put("building", new BuildingAnimation());
        
        // Анимация атаки
        animationTemplates.put("attack", new AttackAnimation());
    }
    
    public void addAnimation(String type, Object target) {
        Animation template = animationTemplates.get(type);
        if (template != null) {
            Animation animation = template.clone();
            animation.setTarget(target);
            animations.add(animation);
        }
    }
    
    public void addSelectionAnimation(GameEntity entity) {
        addAnimation("selection", entity);
    }
    
    public void update() {
        // Обновляем все анимации
        Iterator<Animation> iterator = animations.iterator();
        while (iterator.hasNext()) {
            Animation animation = iterator.next();
            animation.update();
            
            if (animation.isFinished()) {
                iterator.remove();
            }
        }
    }
    
    public void render(Graphics2D g) {
        // Рендерим все анимации
        for (Animation animation : animations) {
            animation.render(g);
        }
    }
    
    public void clear() {
        animations.clear();
    }
    
    // Базовый класс анимации
    public abstract static class Animation {
        protected Object target;
        protected boolean finished;
        protected long startTime;
        protected long duration;
        
        public Animation(long duration) {
            this.duration = duration;
            this.startTime = System.currentTimeMillis();
            this.finished = false;
        }
        
        public abstract void update();
        public abstract void render(Graphics2D g);
        
        public void setTarget(Object target) {
            this.target = target;
        }
        
        public boolean isFinished() {
            return finished;
        }
        
        public Animation clone() {
            try {
                return (Animation) super.clone();
            } catch (CloneNotSupportedException e) {
                return null;
            }
        }
        
        protected float getProgress() {
            long elapsed = System.currentTimeMillis() - startTime;
            return Math.min(1.0f, (float) elapsed / duration);
        }
    }
    
    // Анимация выделения
    private static class SelectionAnimation extends Animation {
        private float alpha;
        private float scale;
        
        public SelectionAnimation() {
            super(1000); // 1 секунда
        }
        
        @Override
        public void update() {
            float progress = getProgress();
            
            // Пульсация
            alpha = 0.5f + 0.5f * (float) Math.sin(progress * Math.PI * 4);
            scale = 1.0f + 0.2f * (float) Math.sin(progress * Math.PI * 2);
            
            if (progress >= 1.0f) {
                finished = true;
            }
        }
        
        @Override
        public void render(Graphics2D g) {
            if (target instanceof GameEntity) {
                GameEntity entity = (GameEntity) target;
                Position pos = entity.getPosition();
                
                // Сохраняем текущие настройки
                Composite originalComposite = g.getComposite();
                Stroke originalStroke = g.getStroke();
                
                // Устанавливаем прозрачность
                g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha));
                
                // Рисуем выделение
                g.setColor(Color.YELLOW);
                g.setStroke(new BasicStroke(3.0f));
                
                int x = (int) pos.getX();
                int y = (int) pos.getY();
                int size = (int)(30 * scale);
                
                g.drawRect(x - size/2, y - size/2, size, size);
                
                // Восстанавливаем настройки
                g.setComposite(originalComposite);
                g.setStroke(originalStroke);
            }
        }
    }
    
    // Анимация строительства
    private static class BuildingAnimation extends Animation {
        private float height;
        
        public BuildingAnimation() {
            super(2000); // 2 секунды
        }
        
        @Override
        public void update() {
            float progress = getProgress();
            height = progress * 50; // Здание растет от 0 до 50 пикселей
            
            if (progress >= 1.0f) {
                finished = true;
            }
        }
        
        @Override
        public void render(Graphics2D g) {
            if (target instanceof Building) {
                Building building = (Building) target;
                Position pos = building.getPosition();
                
                g.setColor(new Color(139, 69, 19)); // Коричневый
                int x = (int) pos.getX();
                int y = (int) pos.getY();
                
                g.fillRect(x - 20, y - (int)height/2, 40, (int)height);
            }
        }
    }
    
    // Анимация атаки
    private static class AttackAnimation extends Animation {
        private float distance;
        
        public AttackAnimation() {
            super(500); // 0.5 секунды
        }
        
        @Override
        public void update() {
            float progress = getProgress();
            distance = progress * 20; // Атака движется на 20 пикселей
            
            if (progress >= 1.0f) {
                finished = true;
            }
        }
        
        @Override
        public void render(Graphics2D g) {
            if (target instanceof Unit) {
                Unit unit = (Unit) target;
                Position pos = unit.getPosition();
                
                g.setColor(Color.RED);
                int x = (int) pos.getX();
                int y = (int) pos.getY();
                
                // Рисуем линию атаки
                g.drawLine(x, y, x + (int)distance, y);
            }
        }
    }
}
```

#### 7. Главный класс с демонстрацией GUI
```java
public class Game {
    private KingdomGameWindow mainWindow;
    private GameWorld gameWorld;
    private Timer gameTimer;
    
    public Game() {
        initializeGame();
    }
    
    private void initializeGame() {
        System.out.println("Инициализация игры с продвинутым GUI...");
        
        this.gameWorld = new GameWorld();
        this.mainWindow = new KingdomGameWindow();
        
        // Создаем тестовые данные
        createTestData();
        
        // Настраиваем игровой таймер
        setupGameTimer();
        
        System.out.println("Игра с продвинутым GUI инициализирована");
    }
    
    private void createTestData() {
        // Создаем тестовые здания
        List<Building> buildings = Arrays.asList(
            new TestBuilding("Главный замок", "Castle", 5),
            new TestBuilding("Ферма", "Farm", 3),
            new TestBuilding("Казарма", "Barracks", 4)
        );
        
        // Создаем тестовые юниты
        List<Unit> units = Arrays.asList(
            new TestUnit("Воин", "Warrior", 100),
            new TestUnit("Лучник", "Archer", 80),
            new TestUnit("Маг", "Mage", 60)
        );
        
        // Создаем тестовые ресурсы
        Map<String, Integer> resources = new HashMap<>();
        resources.put("Золото", 1000);
        resources.put("Дерево", 500);
        resources.put("Камень", 300);
        resources.put("Еда", 200);
        
        // Обновляем панели
        mainWindow.getControlPanel().updateBuildingList(buildings);
        mainWindow.getControlPanel().updateUnitList(units);
        mainWindow.getControlPanel().updateResources(resources);
    }
    
    private void setupGameTimer() {
        gameTimer = new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // Обновляем статус
                mainWindow.updateStatus("Игра работает... Время: " + 
                                      System.currentTimeMillis() / 1000 + "с");
            }
        });
        gameTimer.start();
    }
    
    public void start() {
        System.out.println("Запуск игры с продвинутым GUI...");
        
        // Показываем главное окно
        mainWindow.setVisible(true);
        
        // Показываем приветственное сообщение
        mainWindow.showInfo("Добро пожаловать!", 
                           "Добро пожаловать в игру 'Королевство'!\n" +
                           "Используйте мышь для управления и выбора объектов.\n" +
                           "Нажмите ESC для отмены выделения.");
        
        System.out.println("Игра запущена");
    }
    
    public static void main(String[] args) {
        // Запускаем в EDT
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                Game game = new Game();
                game.start();
            }
        });
    }
}

// Вспомогательные классы
class TestBuilding implements Building {
    private String name;
    private String type;
    private int level;
    private int health;
    private int maxHealth;
    
    public TestBuilding(String name, String type, int level) {
        this.name = name;
        this.type = type;
        this.level = level;
        this.maxHealth = 100;
        this.health = maxHealth;
    }
    
    @Override
    public String getName() { return name; }
    @Override
    public String getType() { return type; }
    @Override
    public int getLevel() { return level; }
    @Override
    public int getCurrentHealth() { return health; }
    @Override
    public int getMaxHealth() { return maxHealth; }
    @Override
    public boolean canUpgrade() { return level < 10; }
    @Override
    public boolean canDemolish() { return true; }
    @Override
    public boolean canPerformAction() { return true; }
    @Override
    public String getActionButtonText() { return "Действие"; }
    @Override
    public String getDescription() { 
        return "Тестовое здание типа " + type + " уровня " + level; 
    }
    
    @Override
    public void upgrade() { level++; }
    @Override
    public void demolish() { health = 0; }
    @Override
    public void performAction() { 
        System.out.println("Выполняется действие для " + name); 
    }
}

class TestUnit implements Unit {
    private String name;
    private String type;
    private int health;
    
    public TestUnit(String name, String type, int health) {
        this.name = name;
        this.type = type;
        this.health = health;
    }
    
    @Override
    public String getName() { return name; }
    @Override
    public String getType() { return type; }
    @Override
    public int getCurrentHealth() { return health; }
    @Override
    public int getMaxHealth() { return 100; }
    @Override
    public boolean isAlive() { return health > 0; }
    @Override
    public boolean isVisible() { return true; }
    @Override
    public Position getPosition() { return new Position(100, 100); }
    @Override
    public int getPosition() { return 0; }
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичный GUI:

### 1. **Гонки на выживание**
- GUI: панель автомобиля, карта трассы, меню гонок
- Компоненты: спидометр, тахометр, мини-карта, панель повреждений
- Анимации: эффекты скорости, повреждения, финиш

### 2. **Космическая колонизация**
- GUI: панель колонии, карта галактики, меню исследований
- Компоненты: ресурсы колонии, технологическое дерево, дипломатия
- Анимации: полеты кораблей, строительство колоний, исследования

### 3. **Подземелье и драконы**
- GUI: панель персонажа, карта подземелья, инвентарь
- Компоненты: характеристики, заклинания, экипировка, квесты
- Анимации: боевые эффекты, заклинания, переходы между уровнями

### 4. **Город-государство**
- GUI: панель города, экономика, дипломатия
- Компоненты: статистика населения, торговля, политика, культура
- Анимации: строительство, развитие города, дипломатические встречи

### 5. **Пиратская стратегия**
- GUI: панель корабля, карта морей, торговля
- Компоненты: состояние корабля, команда, сокровища, репутация
- Анимации: морские сражения, плавание, торговля

### 6. **Фермерское хозяйство**
- GUI: панель фермы, поля, животные
- Компоненты: урожай, скот, рынок, сезоны
- Анимации: рост растений, животные, погодные эффекты

### 7. **Киберпанк-тактика**
- GUI: панель хакера, цифровые сети, киберпространство
- Компоненты: программы, вирусы, защита, цифровая валюта
- Анимации: взломы, цифровые эффекты, сетевые атаки

### 8. **Средневековая осада**
- GUI: панель осады, укрепления, осадные орудия
- Компоненты: состояние стен, боеприпасы, защитники, атакующие
- Анимации: стрельба, разрушения, строительство

### 9. **Зомби-выживание**
- GUI: панель выживания, карта, инвентарь
- Компоненты: здоровье, голод, оружие, убежище
- Анимации: атаки зомби, эффекты оружия, строительство

### 10. **Фэнтези-война**
- GUI: панель армии, карта сражения, магия
- Компоненты: войска, заклинания, тактика, герои
- Анимации: боевые эффекты, магия, тактические действия

## Требования к реализации

### Обязательные требования:
1. **Создать главное окно** с меню и статусной строкой
2. **Реализовать игровую панель** с обработкой событий мыши и клавиатуры
3. **Создать панель управления** с вкладками для разных функций
4. **Реализовать информационные панели** с детальной информацией
5. **Создать систему анимаций** для визуальных эффектов
6. **Демонстрировать работу** всех компонентов GUI

### Дополнительные требования:
1. **Добавить пользовательские компоненты** с уникальным дизайном
2. **Реализовать темы оформления** и настройки интерфейса
3. **Создать контекстные меню** и всплывающие подсказки
4. **Добавить звуковые эффекты** для интерфейса

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Главное окно** | 3 | Меню, статусная строка, компоновка |
| **Игровая панель** | 3 | Обработка событий, рендеринг |
| **Панель управления** | 3 | Вкладки, компоненты управления |
| **Информационные панели** | 2 | Детальная информация, обновление |
| **Система анимаций** | 3 | Визуальные эффекты, анимации |
| **Демонстрация** | 2 | Работающий пример всех компонентов |
| **Качество кода** | 2 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужен продвинутый GUI в играх?
2. Как организовать компоновку компонентов?
3. Как обрабатывать события мыши и клавиатуры?
4. Как создать пользовательские компоненты?
5. Как реализовать анимации в Swing?
6. Как организовать обновление интерфейса?
7. Как создать отзывчивый интерфейс?

## Заключение

В данной лабораторной работе вы изучили создание продвинутых графических интерфейсов в Java на примере создания игровых систем. Вы научились:

- Создавать главные окна с меню и статусными строками
- Реализовывать игровые панели с обработкой событий
- Создавать панели управления с вкладками
- Реализовывать информационные панели
- Создавать систему анимаций
- Организовывать компоновку компонентов

Полученные знания позволят вам создавать современные и удобные игровые интерфейсы.
