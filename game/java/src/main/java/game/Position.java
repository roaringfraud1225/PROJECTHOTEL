/**
 * Класс для представления позиции на игровом поле
 */
public class Position {
    private int x;
    private int y;
    
    /**
     * Конструктор по умолчанию
     */
    public Position() {
        this.x = 0;
        this.y = 0;
    }
    
    /**
     * Параметризованный конструктор
     * @param x координата X
     * @param y координата Y
     */
    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    /**
     * Конструктор копирования
     * @param other позиция для копирования
     */
    public Position(Position other) {
        this.x = other.x;
        this.y = other.y;
    }
    
    /**
     * Вычисляет расстояние до другой позиции
     * @param other другая позиция
     * @return евклидово расстояние
     */
    public double getDistanceTo(Position other) {
        int dx = this.x - other.x;
        int dy = this.y - other.y;
        return Math.sqrt(dx * dx + dy * dy);
    }
    
    /**
     * Проверяет, является ли позиция соседней
     * @param other другая позиция
     * @return true, если позиции соседние
     */
    public boolean isAdjacent(Position other) {
        int dx = Math.abs(this.x - other.x);
        int dy = Math.abs(this.y - other.y);
        return dx <= 1 && dy <= 1;
    }
    
    /**
     * Получает список всех соседних позиций
     * @return список соседних позиций
     */
    public Position[] getNeighbors() {
        Position[] neighbors = new Position[8];
        int index = 0;
        
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx != 0 || dy != 0) {
                    neighbors[index++] = new Position(this.x + dx, this.y + dy);
                }
            }
        }
        
        return neighbors;
    }
    
    // Геттеры и сеттеры
    public int getX() {
        return x;
    }
    
    public void setX(int x) {
        this.x = x;
    }
    
    public int getY() {
        return y;
    }
    
    public void setY(int y) {
        this.y = y;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        
        Position position = (Position) obj;
        return x == position.x && y == position.y;
    }
    
    @Override
    public int hashCode() {
        return java.util.Objects.hash(x, y);
    }
    
    @Override
    public String toString() {
        return "Position{" +
                "x=" + x +
                ", y=" + y +
                '}';
    }
}
