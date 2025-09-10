---
marp: true
theme: default
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# SOLID принципы
## Лекция 8: Основы качественного кода

**Преподаватель:** Каледин О.Е.
**Группа:** 203  
**Семестр:** Осень 2025

---

# 📚 Цель лекции

Понять и освоить SOLID принципы объектно-ориентированного проектирования, которые являются основой для создания качественного, поддерживаемого и расширяемого кода.

---

# 🕐 План лекции

1. **Что такое SOLID принципы?**
2. **S - Single Responsibility Principle (SRP)**
3. **O - Open/Closed Principle (OCP)**
4. **L - Liskov Substitution Principle (LSP)**
5. **I - Interface Segregation Principle (ISP)**
6. **D - Dependency Inversion Principle (DIP)**
7. **Практические примеры применения**
8. **Антипаттерны и как их избежать**

---

# Что такое SOLID принципы?

## Определение:
**SOLID** — это аббревиатура пяти основных принципов объектно-ориентированного проектирования, предложенных Робертом Мартином.

## Цель:
- **Улучшение читаемости** кода
- **Повышение поддерживаемости** и расширяемости
- **Упрощение тестирования** и рефакторинга
- **Создание гибкой архитектуры**

---

# S - Single Responsibility Principle (SRP)

## Принцип единственной ответственности:
**Класс должен иметь только одну причину для изменения.**

## Что это означает:
- Каждый класс должен отвечать за одну конкретную задачу
- Изменения в требованиях должны затрагивать только один класс
- Класс должен иметь одну область ответственности

---

# Пример нарушения SRP

```java
// ❌ Плохо: класс делает слишком много
public class UserManager {
    private List<User> users;
    
    public void addUser(User user) { /* ... */ }
    public void removeUser(User user) { /* ... */ }
    public void updateUser(User user) { /* ... */ }
    
    public void saveToDatabase() { /* ... */ }
    public void loadFromDatabase() { /* ... */ }
    
    public void sendEmail(User user, String message) { /* ... */ }
    public void generateReport() { /* ... */ }
}
```

## Проблемы:
- **Слишком много ответственностей**
- **Сложно тестировать**
- **Сложно изменять**
- **Нарушение принципа единственной ответственности**

---

# Пример соблюдения SRP

```java
// ✅ Хорошо: каждый класс имеет одну ответственность
public class UserRepository {
    public void save(User user) { /* ... */ }
    public void delete(User user) { /* ... */ }
    public void update(User user) { /* ... */ }
    public User findById(String id) { /* ... */ }
}

public class EmailService {
    public void sendEmail(User user, String message) { /* ... */ }
    public void sendBulkEmail(List<User> users, String message) { /* ... */ }
}

public class ReportGenerator {
    public void generateUserReport(List<User> users) { /* ... */ }
    public void generateStatisticsReport() { /* ... */ }
}

public class UserService {
    private UserRepository userRepository;
    private EmailService emailService;
    
    public void registerUser(User user) {
        userRepository.save(user);
        emailService.sendEmail(user, "Добро пожаловать!");
    }
}
```

---

# O - Open/Closed Principle (OCP)

## Принцип открытости/закрытости:
**Программные сущности должны быть открыты для расширения, но закрыты для изменения.**

## Что это означает:
- **Открытость для расширения** — можно добавлять новую функциональность
- **Закрытость для изменения** — существующий код не должен изменяться
- **Использование полиморфизма** и наследования

---

# Пример нарушения OCP

```java
// ❌ Плохо: нужно изменять существующий код
public class AreaCalculator {
    public double calculateArea(Object shape) {
        if (shape instanceof Rectangle) {
            Rectangle rect = (Rectangle) shape;
            return rect.getWidth() * rect.getHeight();
        } else if (shape instanceof Circle) {
            Circle circle = (Circle) shape;
            return Math.PI * circle.getRadius() * circle.getRadius();
        }
        // При добавлении нового типа нужно изменять этот метод
        return 0;
    }
}
```

## Проблемы:
- **Нарушение принципа закрытости**
- **Сложно добавлять новые типы**
- **Риск сломать существующий код**
- **Нарушение SRP**

---

# Пример соблюдения OCP

```java
// ✅ Хорошо: открыто для расширения, закрыто для изменения
public interface Shape {
    double calculateArea();
}

public class Rectangle implements Shape {
    private double width;
    private double height;
    
    @Override
    public double calculateArea() {
        return width * height;
    }
}

public class Circle implements Shape {
    private double radius;
    
    @Override
    public double calculateArea() {
        return Math.PI * radius * radius;
    }
}

// Новый тип можно добавить без изменения существующего кода
public class Triangle implements Shape {
    private double base;
    private double height;
    
    @Override
    public double calculateArea() {
        return 0.5 * base * height;
    }
}

public class AreaCalculator {
    public double calculateTotalArea(List<Shape> shapes) {
        return shapes.stream()
                    .mapToDouble(Shape::calculateArea)
                    .sum();
    }
}
```

---

# L - Liskov Substitution Principle (LSP)

## Принцип подстановки Лисков:
**Объекты базового класса могут быть заменены объектами его подклассов без изменения корректности программы.**

## Что это означает:
- **Подкласс должен быть взаимозаменяем** с базовым классом
- **Поведение подкласса** должно соответствовать ожиданиям базового класса
- **Не должно быть неожиданных эффектов** при замене

---

# Пример нарушения LSP

```java
// ❌ Плохо: нарушение принципа подстановки
public class Rectangle {
    protected int width;
    protected int height;
    
    public void setWidth(int width) { this.width = width; }
    public void setHeight(int height) { this.height = height; }
    public int getWidth() { return width; }
    public int getHeight() { return height; }
    public int getArea() { return width * height; }
}

public class Square extends Rectangle {
    @Override
    public void setWidth(int width) {
        this.width = width;
        this.height = width; // Нарушение LSP!
    }
    
    @Override
    public void setHeight(int height) {
        this.height = height;
        this.width = height; // Нарушение LSP!
    }
}

// Проблема: код, работающий с Rectangle, может сломаться
public void testRectangle(Rectangle rect) {
    rect.setWidth(5);
    rect.setHeight(4);
    assert rect.getArea() == 20; // Упадет для Square!
}
```

---

# Пример соблюдения LSP

```java
// ✅ Хорошо: соблюдение принципа подстановки
public interface Shape {
    int getArea();
}

public class Rectangle implements Shape {
    private int width;
    private int height;
    
    public void setWidth(int width) { this.width = width; }
    public void setHeight(int height) { this.height = height; }
    public int getWidth() { return width; }
    public int getHeight() { return height; }
    
    @Override
    public int getArea() { return width * height; }
}

public class Square implements Shape {
    private int side;
    
    public void setSide(int side) { this.side = side; }
    public int getSide() { return side; }
    
    @Override
    public int getArea() { return side * side; }
}

// Теперь код работает корректно с любым Shape
public void testShape(Shape shape) {
    // Код работает одинаково для всех реализаций
    int area = shape.getArea();
    // ...
}
```

---

# I - Interface Segregation Principle (ISP)

## Принцип разделения интерфейса:
**Клиенты не должны зависеть от интерфейсов, которые они не используют.**

## Что это означает:
- **Интерфейсы должны быть специфичными** для клиентов
- **Лучше много маленьких интерфейсов**, чем один большой
- **Клиенты не должны знать** о методах, которые они не используют

---

# Пример нарушения ISP

```java
// ❌ Плохо: один большой интерфейс
public interface Worker {
    void work();
    void eat();
    void sleep();
    void getSalary();
    void takeVacation();
    void reportToManager();
    void attendMeeting();
    void writeCode();
    void testCode();
    void deployCode();
}

// Проблема: разные типы работников должны реализовывать все методы
public class Programmer implements Worker {
    public void work() { /* программирование */ }
    public void eat() { /* еда */ }
    public void sleep() { /* сон */ }
    public void getSalary() { /* зарплата */ }
    public void takeVacation() { /* отпуск */ }
    public void reportToManager() { /* отчет */ }
    public void attendMeeting() { /* встречи */ }
    public void writeCode() { /* написание кода */ }
    public void testCode() { /* тестирование */ }
    public void deployCode() { /* развертывание */ }
}

public class Manager implements Worker {
    // Должен реализовать все методы, даже те, которые не нужны
    public void writeCode() { /* менеджер не пишет код! */ }
    public void testCode() { /* менеджер не тестирует код! */ }
    public void deployCode() { /* менеджер не разворачивает код! */ }
    // ... остальные методы
}
```

---

# Пример соблюдения ISP

```java
// ✅ Хорошо: разделение интерфейсов
public interface Workable {
    void work();
}

public interface Eatable {
    void eat();
}

public interface Sleepable {
    void sleep();
}

public interface Payable {
    void getSalary();
}

public interface Vacationable {
    void takeVacation();
}

public interface Reportable {
    void reportToManager();
}

public interface MeetingAttendable {
    void attendMeeting();
}

public interface Codable {
    void writeCode();
    void testCode();
    void deployCode();
}

// Теперь каждый класс реализует только нужные интерфейсы
public class Programmer implements Workable, Eatable, Sleepable, 
                                   Payable, Vacationable, Reportable, 
                                   MeetingAttendable, Codable {
    // Реализация только нужных методов
}

public class Manager implements Workable, Eatable, Sleepable, 
                             Payable, Vacationable, Reportable, 
                             MeetingAttendable {
    // Не реализует Codable - не нужно!
}
```

---

# D - Dependency Inversion Principle (DIP)

## Принцип инверсии зависимостей:
**Зависимости должны строиться на абстракциях, а не на конкретных классах.**

## Что это означает:
- **Модули высокого уровня** не должны зависеть от модулей низкого уровня
- **Оба должны зависеть от абстракций**
- **Абстракции не должны зависеть от деталей**

---

# Пример нарушения DIP

```java
// ❌ Плохо: зависимости от конкретных классов
public class UserService {
    private MySQLDatabase database; // Зависимость от конкретного класса
    
    public UserService() {
        this.database = new MySQLDatabase(); // Создание конкретного экземпляра
    }
    
    public void saveUser(User user) {
        database.save(user);
    }
    
    public User getUser(String id) {
        return database.findById(id);
    }
}

public class MySQLDatabase {
    public void save(User user) { /* MySQL специфичный код */ }
    public User findById(String id) { /* MySQL специфичный код */ }
}
```

## Проблемы:
- **Сложно тестировать** (нужна реальная база данных)
- **Сложно заменить** базу данных
- **Жесткая связанность** между классами
- **Нарушение принципа инверсии зависимостей**

---

# Пример соблюдения DIP

```java
// ✅ Хорошо: зависимости от абстракций
public interface UserRepository {
    void save(User user);
    User findById(String id);
    void delete(String id);
    List<User> findAll();
}

public class MySQLUserRepository implements UserRepository {
    @Override
    public void save(User user) { /* MySQL реализация */ }
    
    @Override
    public User findById(String id) { /* MySQL реализация */ }
    
    @Override
    public void delete(String id) { /* MySQL реализация */ }
    
    @Override
    public List<User> findAll() { /* MySQL реализация */ }
}

public class InMemoryUserRepository implements UserRepository {
    private Map<String, User> users = new HashMap<>();
    
    @Override
    public void save(User user) { users.put(user.getId(), user); }
    
    @Override
    public User findById(String id) { return users.get(id); }
    
    @Override
    public void delete(String id) { users.remove(id); }
    
    @Override
    public List<User> findAll() { return new ArrayList<>(users.values()); }
}

public class UserService {
    private UserRepository repository; // Зависимость от абстракции
    
    public UserService(UserRepository repository) { // Внедрение зависимости
        this.repository = repository;
    }
    
    public void saveUser(User user) {
        repository.save(user);
    }
    
    public User getUser(String id) {
        return repository.findById(id);
    }
}
```

---

# Практический пример: Игровая система

```java
// Применение SOLID принципов в игровой системе
public interface UnitRepository {
    void save(Unit unit);
    Unit findById(String id);
    List<Unit> findByType(UnitType type);
}

public interface CombatSystem {
    void attack(Unit attacker, Unit target);
    boolean canAttack(Unit attacker, Unit target);
}

public interface MovementSystem {
    void move(Unit unit, Position newPosition);
    boolean canMoveTo(Unit unit, Position position);
}

public interface ResourceSystem {
    void consumeResource(Unit unit, ResourceType type, int amount);
    boolean hasResource(Unit unit, ResourceType type, int amount);
}

// Сервис, который использует все системы
public class GameService {
    private final UnitRepository unitRepository;
    private final CombatSystem combatSystem;
    private final MovementSystem movementSystem;
    private final ResourceSystem resourceSystem;
    
    public GameService(UnitRepository unitRepository, 
                      CombatSystem combatSystem,
                      MovementSystem movementSystem,
                      ResourceSystem resourceSystem) {
        this.unitRepository = unitRepository;
        this.combatSystem = combatSystem;
        this.movementSystem = movementSystem;
        this.resourceSystem = resourceSystem;
    }
    
    public void performAction(Unit unit, GameAction action) {
        // Логика игры, использующая различные системы
    }
}
```

---

# Антипаттерны и как их избежать

## 1. **God Object (Божественный объект)**
- **Проблема:** Один класс делает слишком много
- **Решение:** Разделить на несколько классов по ответственности

## 2. **Tight Coupling (Жесткая связанность)**
- **Проблема:** Классы сильно зависят друг от друга
- **Решение:** Использовать интерфейсы и внедрение зависимостей

## 3. **Feature Envy (Зависть к функциональности)**
- **Проблема:** Метод использует данные другого объекта больше, чем свои
- **Решение:** Переместить метод в правильный класс

## 4. **Data Clumps (Скопления данных)**
- **Проблема:** Группы параметров передаются вместе
- **Решение:** Создать класс для этих данных

---

# Проверка соблюдения SOLID

## Вопросы для самопроверки:

### SRP:
- **Можно ли описать ответственность класса одним предложением?**
- **Изменяется ли класс по разным причинам?**

### OCP:
- **Можно ли добавить новую функциональность без изменения существующего кода?**
- **Используется ли полиморфизм?**

### LSP:
- **Можно ли заменить базовый класс подклассом без проблем?**
- **Поведение подкласса соответствует ожиданиям?**

### ISP:
- **Реализует ли класс методы, которые не использует?**
- **Можно ли разделить интерфейс на более мелкие?**

### DIP:
- **Зависит ли код от конкретных классов?**
- **Используются ли абстракции?**

---

# 🎯 Заключение

SOLID принципы помогают создавать:

✅ **Читаемый и понятный код**  
✅ **Легко тестируемые компоненты**  
✅ **Расширяемую архитектуру**  
✅ **Поддерживаемые системы**  
✅ **Гибкие решения**  

## Ключевые моменты:
- **SRP:** Одна ответственность на класс
- **OCP:** Открытость для расширения, закрытость для изменения
- **LSP:** Подклассы должны быть взаимозаменяемы
- **ISP:** Интерфейсы должны быть специфичными
- **DIP:** Зависимости от абстракций, а не от конкретных классов

---

# 📝 Практическое задание

## Задача: Рефакторинг игровой системы

Исходный код нарушает SOLID принципы. Необходимо:

1. **Разделить ответственности** (SRP)
2. **Сделать код расширяемым** (OCP)
3. **Обеспечить взаимозаменяемость** (LSP)
4. **Разделить интерфейсы** (ISP)
5. **Инвертировать зависимости** (DIP)

## Критерии оценки:
- Правильное применение всех 5 принципов (5 баллов)
- Читаемость и структурированность кода (3 балла)
- Дополнительная функциональность (2 балла)

**Максимальный балл: 10**

---

# Что дальше?

## На следующей лекции:
- **Архитектура приложений**
- **MVC паттерн**
- **Разделение ответственности**
- **Модульность и слои**

## Подготовка:
- Изучить SOLID принципы на практике
- Выполнить практическое задание
- Прочитать главу 5-6 из учебника

---

# Вопросы?

## Контакты:
- **Email:** [ваш.email@university.edu]
- **Telegram:** [@username]
- **Офис:** [номер кабинета]

## Следующая лекция: **Архитектура приложений**
