# Лабораторная работа 12: Сетевое программирование

## Цель работы
Изучить принципы сетевого программирования в Java на примере разработки многопользовательских игровых систем. Научиться создавать клиент-сервер архитектуру, обрабатывать сетевые сообщения и реализовывать многопользовательскую игру.

## Теоретические основы

### Сетевое программирование
- **TCP/UDP протоколы** - надежная и быстрая передача данных
- **Клиент-сервер архитектура** - разделение ответственности
- **Сокеты** - интерфейс для сетевого взаимодействия
- **Сериализация** - преобразование объектов в байты

### Применение в играх
- **Многопользовательские игры** - взаимодействие игроков
- **Синхронизация состояния** - обновление игрового мира
- **Чат и коммуникация** - общение между игроками
- **Сохранение прогресса** - облачные сохранения

## Практическое задание: Игра "Королевство"

### Описание игры
"Королевство" - пошаговая стратегия, где игрок управляет королевством, развивает экономику, строит здания и ведет тактические сражения.

### Сетевая архитектура

#### 1. Сервер игры - `GameServer`
```java
public class GameServer {
    private ServerSocket serverSocket;
    private List<ClientHandler> clients;
    private GameWorld gameWorld;
    private Map<String, Player> players;
    private final Object clientsLock;
    private final Object playersLock;
    private volatile boolean running;
    
    public GameServer(int port) {
        this.clients = new ArrayList<>();
        this.gameWorld = new GameWorld();
        this.players = new HashMap<>();
        this.clientsLock = new Object();
        this.playersLock = new Object();
        this.running = false;
        
        try {
            this.serverSocket = new ServerSocket(port);
            System.out.println("Сервер запущен на порту " + port);
        } catch (IOException e) {
            System.err.println("Ошибка запуска сервера: " + e.getMessage());
        }
    }
    
    public void start() {
        running = true;
        System.out.println("Сервер начал работу");
        
        // Запускаем поток для принятия подключений
        new Thread(this::acceptConnections).start();
        
        // Запускаем основной игровой цикл
        new Thread(this::gameLoop).start();
    }
    
    private void acceptConnections() {
        while (running) {
            try {
                Socket clientSocket = serverSocket.accept();
                System.out.println("Новое подключение: " + clientSocket.getInetAddress());
                
                ClientHandler clientHandler = new ClientHandler(clientSocket, this);
                synchronized (clientsLock) {
                    clients.add(clientHandler);
                }
                
                clientHandler.start();
                
            } catch (IOException e) {
                if (running) {
                    System.err.println("Ошибка принятия подключения: " + e.getMessage());
                }
            }
        }
    }
    
    private void gameLoop() {
        while (running) {
            try {
                // Обновляем игровой мир
                gameWorld.update();
                
                // Синхронизируем состояние с клиентами
                broadcastGameState();
                
                // Очищаем отключенных клиентов
                cleanupDisconnectedClients();
                
                Thread.sleep(100); // 10 FPS
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                System.err.println("Ошибка в игровом цикле: " + e.getMessage());
            }
        }
    }
    
    private void broadcastGameState() {
        GameState gameState = createGameState();
        String stateMessage = serializeGameState(gameState);
        
        synchronized (clientsLock) {
            for (ClientHandler client : clients) {
                if (client.isConnected()) {
                    client.sendMessage(stateMessage);
                }
            }
        }
    }
    
    private GameState createGameState() {
        synchronized (playersLock) {
            return new GameState(
                new ArrayList<>(players.values()),
                gameWorld.getUnits(),
                gameWorld.getBuildings(),
                gameWorld.getResources()
            );
        }
    }
    
    private String serializeGameState(GameState state) {
        // Упрощенная сериализация в JSON
        return "GAME_STATE:" + state.toJson();
    }
    
    private void cleanupDisconnectedClients() {
        synchronized (clientsLock) {
            clients.removeIf(client -> !client.isConnected());
        }
    }
    
    public void addPlayer(String playerId, Player player) {
        synchronized (playersLock) {
            players.put(playerId, player);
        }
    }
    
    public void removePlayer(String playerId) {
        synchronized (playersLock) {
            players.remove(playerId);
        }
    }
    
    public Player getPlayer(String playerId) {
        synchronized (playersLock) {
            return players.get(playerId);
        }
    }
    
    public void broadcastMessage(String message, String senderId) {
        synchronized (clientsLock) {
            for (ClientHandler client : clients) {
                if (client.isConnected() && !client.getPlayerId().equals(senderId)) {
                    client.sendMessage("CHAT:" + senderId + ":" + message);
                }
            }
        }
    }
    
    public void stop() {
        running = false;
        
        // Останавливаем всех клиентов
        synchronized (clientsLock) {
            for (ClientHandler client : clients) {
                client.disconnect();
            }
            clients.clear();
        }
        
        // Закрываем сервер
        try {
            if (serverSocket != null && !serverSocket.isClosed()) {
                serverSocket.close();
            }
        } catch (IOException e) {
            System.err.println("Ошибка закрытия сервера: " + e.getMessage());
        }
        
        System.out.println("Сервер остановлен");
    }
    
    public boolean isRunning() { return running; }
    public int getClientCount() { 
        synchronized (clientsLock) { 
            return clients.size(); 
        } 
    }
    public int getPlayerCount() { 
        synchronized (playersLock) { 
            return players.size(); 
        } 
    }
}
```

#### 2. Обработчик клиента - `ClientHandler`
```java
public class ClientHandler extends Thread {
    private Socket clientSocket;
    private GameServer server;
    private BufferedReader input;
    private PrintWriter output;
    private String playerId;
    private volatile boolean connected;
    
    public ClientHandler(Socket socket, GameServer server) {
        this.clientSocket = socket;
        this.server = server;
        this.connected = true;
        
        try {
            this.input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            this.output = new PrintWriter(socket.getOutputStream(), true);
        } catch (IOException e) {
            System.err.println("Ошибка создания потоков ввода/вывода: " + e.getMessage());
            this.connected = false;
        }
    }
    
    @Override
    public void run() {
        try {
            // Обрабатываем сообщения от клиента
            String message;
            while (connected && (message = input.readLine()) != null) {
                processMessage(message);
            }
        } catch (IOException e) {
            if (connected) {
                System.err.println("Ошибка чтения от клиента: " + e.getMessage());
            }
        } finally {
            disconnect();
        }
    }
    
    private void processMessage(String message) {
        try {
            String[] parts = message.split(":", 3);
            String command = parts[0];
            
            switch (command) {
                case "JOIN":
                    handleJoin(parts[1], parts[2]);
                    break;
                case "MOVE":
                    handleMove(parts[1], parts[2]);
                    break;
                case "BUILD":
                    handleBuild(parts[1], parts[2]);
                    break;
                case "ATTACK":
                    handleAttack(parts[1], parts[2]);
                    break;
                case "CHAT":
                    handleChat(parts[1]);
                    break;
                case "QUIT":
                    handleQuit();
                    break;
                default:
                    System.out.println("Неизвестная команда: " + command);
            }
        } catch (Exception e) {
            System.err.println("Ошибка обработки сообщения: " + e.getMessage());
            sendMessage("ERROR:Неверный формат сообщения");
        }
    }
    
    private void handleJoin(String playerName, String playerType) {
        playerId = generatePlayerId();
        Player player = new Player(playerName, PlayerType.valueOf(playerType));
        
        server.addPlayer(playerId, player);
        
        // Отправляем подтверждение подключения
        sendMessage("JOIN_SUCCESS:" + playerId);
        sendMessage("INFO:Добро пожаловать в игру, " + playerName + "!");
        
        System.out.println("Игрок " + playerName + " присоединился к игре");
    }
    
    private void handleMove(String unitId, String position) {
        if (playerId == null) {
            sendMessage("ERROR:Не авторизован");
            return;
        }
        
        try {
            Position newPos = Position.fromString(position);
            // Логика перемещения юнита
            sendMessage("MOVE_SUCCESS:" + unitId + ":" + position);
        } catch (Exception e) {
            sendMessage("ERROR:Неверная позиция");
        }
    }
    
    private void handleBuild(String buildingType, String position) {
        if (playerId == null) {
            sendMessage("ERROR:Не авторизован");
            return;
        }
        
        try {
            Position buildPos = Position.fromString(position);
            // Логика строительства
            sendMessage("BUILD_SUCCESS:" + buildingType + ":" + position);
        } catch (Exception e) {
            sendMessage("ERROR:Неверная позиция строительства");
        }
    }
    
    private void handleAttack(String unitId, String targetId) {
        if (playerId == null) {
            sendMessage("ERROR:Не авторизован");
            return;
        }
        
        // Логика атаки
        sendMessage("ATTACK_SUCCESS:" + unitId + ":" + targetId);
    }
    
    private void handleChat(String message) {
        if (playerId == null) {
            sendMessage("ERROR:Не авторизован");
            return;
        }
        
        Player player = server.getPlayer(playerId);
        if (player != null) {
            server.broadcastMessage(message, playerId);
        }
    }
    
    private void handleQuit() {
        sendMessage("QUIT_SUCCESS");
        disconnect();
    }
    
    private String generatePlayerId() {
        return "player_" + System.currentTimeMillis() + "_" + Thread.currentThread().getId();
    }
    
    public void sendMessage(String message) {
        if (connected && output != null) {
            output.println(message);
        }
    }
    
    public void disconnect() {
        connected = false;
        
        // Удаляем игрока с сервера
        if (playerId != null) {
            server.removePlayer(playerId);
        }
        
        // Закрываем соединение
        try {
            if (input != null) input.close();
            if (output != null) output.close();
            if (clientSocket != null && !clientSocket.isClosed()) {
                clientSocket.close();
            }
        } catch (IOException e) {
            System.err.println("Ошибка закрытия соединения: " + e.getMessage());
        }
        
        System.out.println("Клиент отключен: " + playerId);
    }
    
    public boolean isConnected() { return connected; }
    public String getPlayerId() { return playerId; }
}
```

#### 3. Клиент игры - `GameClient`
```java
public class GameClient {
    private Socket socket;
    private BufferedReader input;
    private PrintWriter output;
    private String playerId;
    private String playerName;
    private volatile boolean connected;
    private GameClientUI ui;
    private Thread messageListener;
    
    public GameClient(String serverAddress, int port, String playerName) {
        this.playerName = playerName;
        this.connected = false;
        
        try {
            this.socket = new Socket(serverAddress, port);
            this.input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            this.output = new PrintWriter(socket.getOutputStream(), true);
            this.connected = true;
            
            System.out.println("Подключен к серверу " + serverAddress + ":" + port);
            
            // Отправляем запрос на подключение
            sendJoinRequest();
            
            // Запускаем слушатель сообщений
            startMessageListener();
            
        } catch (IOException e) {
            System.err.println("Ошибка подключения к серверу: " + e.getMessage());
        }
    }
    
    private void sendJoinRequest() {
        sendMessage("JOIN:" + playerName + ":HUMAN");
    }
    
    private void startMessageListener() {
        messageListener = new Thread(() -> {
            try {
                String message;
                while (connected && (message = input.readLine()) != null) {
                    processServerMessage(message);
                }
            } catch (IOException e) {
                if (connected) {
                    System.err.println("Ошибка чтения от сервера: " + e.getMessage());
                }
            } finally {
                disconnect();
            }
        });
        messageListener.start();
    }
    
    private void processServerMessage(String message) {
        try {
            String[] parts = message.split(":", 3);
            String command = parts[0];
            
            switch (command) {
                case "JOIN_SUCCESS":
                    handleJoinSuccess(parts[1]);
                    break;
                case "GAME_STATE":
                    handleGameState(parts[1]);
                    break;
                case "MOVE_SUCCESS":
                    handleMoveSuccess(parts[1], parts[2]);
                    break;
                case "BUILD_SUCCESS":
                    handleBuildSuccess(parts[1], parts[2]);
                    break;
                case "ATTACK_SUCCESS":
                    handleAttackSuccess(parts[1], parts[2]);
                    break;
                case "CHAT":
                    handleChatMessage(parts[1], parts[2]);
                    break;
                case "ERROR":
                    handleError(parts[1]);
                    break;
                case "INFO":
                    handleInfo(parts[1]);
                    break;
                default:
                    System.out.println("Неизвестное сообщение от сервера: " + command);
            }
        } catch (Exception e) {
            System.err.println("Ошибка обработки сообщения сервера: " + e.getMessage());
        }
    }
    
    private void handleJoinSuccess(String newPlayerId) {
        this.playerId = newPlayerId;
        System.out.println("Успешно подключен к игре. ID: " + playerId);
        
        if (ui != null) {
            ui.onConnected();
        }
    }
    
    private void handleGameState(String stateJson) {
        try {
            GameState gameState = GameState.fromJson(stateJson);
            
            if (ui != null) {
                ui.updateGameState(gameState);
            }
        } catch (Exception e) {
            System.err.println("Ошибка парсинга состояния игры: " + e.getMessage());
        }
    }
    
    private void handleMoveSuccess(String unitId, String position) {
        System.out.println("Юнит " + unitId + " перемещен в " + position);
        
        if (ui != null) {
            ui.onUnitMoved(unitId, position);
        }
    }
    
    private void handleBuildSuccess(String buildingType, String position) {
        System.out.println("Построено здание " + buildingType + " в " + position);
        
        if (ui != null) {
            ui.onBuildingConstructed(buildingType, position);
        }
    }
    
    private void handleAttackSuccess(String unitId, String targetId) {
        System.out.println("Юнит " + unitId + " атаковал " + targetId);
        
        if (ui != null) {
            ui.onAttack(unitId, targetId);
        }
    }
    
    private void handleChatMessage(String senderId, String message) {
        String senderName = getPlayerNameById(senderId);
        System.out.println(senderName + ": " + message);
        
        if (ui != null) {
            ui.onChatMessage(senderName, message);
        }
    }
    
    private void handleError(String errorMessage) {
        System.err.println("Ошибка сервера: " + errorMessage);
        
        if (ui != null) {
            ui.onError(errorMessage);
        }
    }
    
    private void handleInfo(String infoMessage) {
        System.out.println("Информация: " + infoMessage);
        
        if (ui != null) {
            ui.onInfo(infoMessage);
        }
    }
    
    private String getPlayerNameById(String playerId) {
        // В реальной игре здесь был бы поиск по ID
        return playerId.equals(this.playerId) ? "Вы" : "Игрок " + playerId;
    }
    
    public void sendMessage(String message) {
        if (connected && output != null) {
            output.println(message);
        }
    }
    
    public void moveUnit(String unitId, Position position) {
        sendMessage("MOVE:" + unitId + ":" + position.toString());
    }
    
    public void build(String buildingType, Position position) {
        sendMessage("BUILD:" + buildingType + ":" + position.toString());
    }
    
    public void attack(String unitId, String targetId) {
        sendMessage("ATTACK:" + unitId + ":" + targetId);
    }
    
    public void sendChat(String message) {
        sendMessage("CHAT:" + message);
    }
    
    public void quit() {
        sendMessage("QUIT");
        disconnect();
    }
    
    public void disconnect() {
        connected = false;
        
        // Останавливаем слушатель сообщений
        if (messageListener != null && messageListener.isAlive()) {
            messageListener.interrupt();
        }
        
        // Закрываем соединение
        try {
            if (input != null) input.close();
            if (output != null) output.close();
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
        } catch (IOException e) {
            System.err.println("Ошибка закрытия соединения: " + e.getMessage());
        }
        
        System.out.println("Отключен от сервера");
    }
    
    public void setUI(GameClientUI ui) {
        this.ui = ui;
    }
    
    public boolean isConnected() { return connected; }
    public String getPlayerId() { return playerId; }
    public String getPlayerName() { return playerName; }
}
```

#### 4. Сетевое сообщение - `NetworkMessage`
```java
public class NetworkMessage {
    private String type;
    private String senderId;
    private String data;
    private long timestamp;
    
    public NetworkMessage(String type, String senderId, String data) {
        this.type = type;
        this.senderId = senderId;
        this.data = data;
        this.timestamp = System.currentTimeMillis();
    }
    
    public String toNetworkString() {
        return String.format("%s:%s:%s:%d", type, senderId, data, timestamp);
    }
    
    public static NetworkMessage fromNetworkString(String message) {
        String[] parts = message.split(":", 4);
        if (parts.length >= 4) {
            NetworkMessage msg = new NetworkMessage(parts[0], parts[1], parts[2]);
            try {
                msg.timestamp = Long.parseLong(parts[3]);
            } catch (NumberFormatException e) {
                // Используем текущее время если не удалось распарсить
            }
            return msg;
        }
        throw new IllegalArgumentException("Неверный формат сетевого сообщения");
    }
    
    public String getType() { return type; }
    public String getSenderId() { return senderId; }
    public String getData() { return data; }
    public long getTimestamp() { return timestamp; }
    
    @Override
    public String toString() {
        return String.format("NetworkMessage{type='%s', senderId='%s', data='%s', timestamp=%d}", 
                           type, senderId, data, timestamp);
    }
}
```

#### 5. Менеджер соединений - `ConnectionManager`
```java
public class ConnectionManager {
    private Map<String, ClientConnection> connections;
    private List<ConnectionListener> listeners;
    private final Object connectionsLock;
    private final Object listenersLock;
    
    public ConnectionManager() {
        this.connections = new HashMap<>();
        this.listeners = new ArrayList<>();
        this.connectionsLock = new Object();
        this.listenersLock = new Object();
    }
    
    public void addConnection(String playerId, ClientConnection connection) {
        synchronized (connectionsLock) {
            connections.put(playerId, connection);
        }
        
        // Уведомляем слушателей
        notifyConnectionAdded(playerId, connection);
    }
    
    public void removeConnection(String playerId) {
        ClientConnection connection;
        synchronized (connectionsLock) {
            connection = connections.remove(playerId);
        }
        
        if (connection != null) {
            // Уведомляем слушателей
            notifyConnectionRemoved(playerId, connection);
        }
    }
    
    public ClientConnection getConnection(String playerId) {
        synchronized (connectionsLock) {
            return connections.get(playerId);
        }
    }
    
    public void broadcastMessage(String message) {
        List<ClientConnection> currentConnections;
        synchronized (connectionsLock) {
            currentConnections = new ArrayList<>(connections.values());
        }
        
        for (ClientConnection connection : currentConnections) {
            if (connection.isConnected()) {
                connection.sendMessage(message);
            }
        }
    }
    
    public void sendMessageToPlayer(String playerId, String message) {
        ClientConnection connection = getConnection(playerId);
        if (connection != null && connection.isConnected()) {
            connection.sendMessage(message);
        }
    }
    
    public void addListener(ConnectionListener listener) {
        synchronized (listenersLock) {
            listeners.add(listener);
        }
    }
    
    public void removeListener(ConnectionListener listener) {
        synchronized (listenersLock) {
            listeners.remove(listener);
        }
    }
    
    private void notifyConnectionAdded(String playerId, ClientConnection connection) {
        synchronized (listenersLock) {
            for (ConnectionListener listener : listeners) {
                listener.onConnectionAdded(playerId, connection);
            }
        }
    }
    
    private void notifyConnectionRemoved(String playerId, ClientConnection connection) {
        synchronized (listenersLock) {
            for (ConnectionListener listener : listeners) {
                listener.onConnectionRemoved(playerId, connection);
            }
        }
    }
    
    public int getConnectionCount() {
        synchronized (connectionsLock) {
            return connections.size();
        }
    }
    
    public List<String> getConnectedPlayerIds() {
        synchronized (connectionsLock) {
            return new ArrayList<>(connections.keySet());
        }
    }
    
    public interface ConnectionListener {
        void onConnectionAdded(String playerId, ClientConnection connection);
        void onConnectionRemoved(String playerId, ClientConnection connection);
    }
}

public class ClientConnection {
    private Socket socket;
    private PrintWriter output;
    private volatile boolean connected;
    
    public ClientConnection(Socket socket) {
        this.socket = socket;
        this.connected = true;
        
        try {
            this.output = new PrintWriter(socket.getOutputStream(), true);
        } catch (IOException e) {
            this.connected = false;
        }
    }
    
    public void sendMessage(String message) {
        if (connected && output != null) {
            output.println(message);
        }
    }
    
    public void disconnect() {
        connected = false;
        
        try {
            if (output != null) output.close();
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
        } catch (IOException e) {
            System.err.println("Ошибка закрытия соединения: " + e.getMessage());
        }
    }
    
    public boolean isConnected() { return connected; }
    public Socket getSocket() { return socket; }
}
```

#### 6. Главный класс с демонстрацией сетевого взаимодействия
```java
public class NetworkGame {
    private GameServer server;
    private GameClient client;
    private boolean isServer;
    
    public NetworkGame(boolean isServer) {
        this.isServer = isServer;
    }
    
    public void start() {
        if (isServer) {
            startServer();
        } else {
            startClient();
        }
    }
    
    private void startServer() {
        System.out.println("Запуск сервера...");
        server = new GameServer(8080);
        server.start();
        
        // Даем серверу поработать
        try {
            Thread.sleep(30000); // 30 секунд
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            server.stop();
        }
    }
    
    private void startClient() {
        System.out.println("Запуск клиента...");
        client = new GameClient("localhost", 8080, "Игрок_" + System.currentTimeMillis());
        
        // Даем клиенту поработать
        try {
            Thread.sleep(10000); // 10 секунд
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            client.quit();
        }
    }
    
    public static void main(String[] args) {
        if (args.length > 0 && args[0].equals("server")) {
            NetworkGame game = new NetworkGame(true);
            game.start();
        } else {
            NetworkGame game = new NetworkGame(false);
            game.start();
        }
    }
}

// Вспомогательные классы
class GameState {
    private List<Player> players;
    private List<Unit> units;
    private List<Building> buildings;
    private Map<String, Resource> resources;
    
    public GameState(List<Player> players, List<Unit> units, 
                    List<Building> buildings, Map<String, Resource> resources) {
        this.players = players;
        this.units = units;
        this.buildings = buildings;
        this.resources = resources;
    }
    
    public String toJson() {
        // Упрощенная сериализация
        return String.format("{\"players\":%d,\"units\":%d,\"buildings\":%d,\"resources\":%d}", 
                           players.size(), units.size(), buildings.size(), resources.size());
    }
    
    public static GameState fromJson(String json) {
        // Упрощенная десериализация
        return new GameState(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new HashMap<>());
    }
    
    public List<Player> getPlayers() { return players; }
    public List<Unit> getUnits() { return units; }
    public List<Building> getBuildings() { return buildings; }
    public Map<String, Resource> getResources() { return resources; }
}

class Player {
    private String name;
    private PlayerType type;
    
    public Player(String name, PlayerType type) {
        this.name = name;
        this.type = type;
    }
    
    public String getName() { return name; }
    public PlayerType getType() { return type; }
}

enum PlayerType {
    HUMAN, AI
}

class Position {
    private int x, y;
    
    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    public static Position fromString(String str) {
        String[] parts = str.split(",");
        return new Position(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
    }
    
    @Override
    public String toString() {
        return x + "," + y;
    }
    
    public int getX() { return x; }
    public int getY() { return y; }
}

// Интерфейс для UI клиента
interface GameClientUI {
    void onConnected();
    void updateGameState(GameState state);
    void onUnitMoved(String unitId, String position);
    void onBuildingConstructed(String buildingType, String position);
    void onAttack(String unitId, String targetId);
    void onChatMessage(String sender, String message);
    void onError(String error);
    void onInfo(String info);
}
```

## Варианты игр для самостоятельной разработки

Студенты должны выбрать одну из следующих игр и реализовать аналогичную сетевую архитектуру:

### 1. **Гонки на выживание**
- Сервер: управление гонками, синхронизация позиций
- Клиент: управление автомобилем, чат между гонщиками
- Протокол: UDP для быстрых обновлений, TCP для чата

### 2. **Космическая колонизация**
- Сервер: управление колониями, межпланетная торговля
- Клиент: управление колонией, дипломатия
- Протокол: TCP для надежной передачи данных

### 3. **Подземелье и драконы**
- Сервер: управление подземельями, синхронизация персонажей
- Клиент: управление персонажем, групповые квесты
- Протокол: TCP для надежности, WebSocket для чата

### 4. **Город-государство**
- Сервер: управление городами, экономика, дипломатия
- Клиент: управление городом, торговля
- Протокол: TCP для экономических операций

### 5. **Пиратская стратегия**
- Сервер: управление флотами, морские сражения
- Клиент: управление кораблем, пиратские рейды
- Протокол: UDP для боевых действий, TCP для торговли

### 6. **Фермерское хозяйство**
- Сервер: управление фермами, рыночные цены
- Клиент: управление фермой, торговля урожаем
- Протокол: TCP для экономических операций

### 7. **Киберпанк-тактика**
- Сервер: управление сетями, хакерские атаки
- Клиент: управление хакером, цифровая торговля
- Протокол: TCP для сетевых операций

### 8. **Средневековая осада**
- Сервер: управление осадами, военные операции
- Клиент: управление армией, осадные действия
- Протокол: UDP для боевых действий, TCP для управления

### 9. **Зомби-выживание**
- Сервер: управление зомби, ресурсы выживания
- Клиент: управление выжившим, групповые операции
- Протокол: UDP для боевых действий, TCP для торговли

### 10. **Фэнтези-война**
- Сервер: управление армиями, магические битвы
- Клиент: управление армией, магические заклинания
- Протокол: TCP для надежности, UDP для быстрых действий

## Требования к реализации

### Обязательные требования:
1. **Создать сервер** для управления игрой
2. **Реализовать клиент** для подключения к серверу
3. **Создать протокол обмена** сообщениями
4. **Реализовать синхронизацию** игрового состояния
5. **Демонстрировать работу** клиент-сервер взаимодействия
6. **Создать обработку сетевых** сообщений

### Дополнительные требования:
1. **Добавить чат** между игроками
2. **Реализовать обработку ошибок** сети
3. **Создать конфигурацию** сетевых параметров
4. **Добавить логирование** сетевых операций

## Критерии оценки

| Критерий | Баллы | Описание |
|----------|-------|----------|
| **Сервер** | 4 | Управление подключениями и игрой |
| **Клиент** | 3 | Подключение и взаимодействие с сервером |
| **Протокол обмена** | 3 | Формат и обработка сообщений |
| **Синхронизация** | 3 | Обновление игрового состояния |
| **Обработка сообщений** | 2 | Различные типы сетевых команд |
| **Демонстрация** | 2 | Работающий пример сетевой игры |
| **Качество кода** | 1 | Читаемость, комментарии, именование |

**Максимальный балл: 18**

## Вопросы для самопроверки

1. Зачем нужна клиент-сервер архитектура в играх?
2. Как синхронизировать состояние между клиентами?
3. Какой протокол выбрать для разных типов данных?
4. Как обрабатывать сетевые ошибки?
5. Как обеспечить безопасность сетевого взаимодействия?
6. Как масштабировать сетевую игру?
7. Как оптимизировать сетевой трафик?

## Заключение

В данной лабораторной работе вы изучили принципы сетевого программирования в Java на примере создания многопользовательских игровых систем. Вы научились:

- Создавать клиент-сервер архитектуру
- Реализовывать протокол обмена сообщениями
- Синхронизировать игровое состояние
- Обрабатывать сетевые подключения
- Создавать многопользовательскую игру

Полученные знания позволят вам создавать сетевые приложения и многопользовательские игры.
