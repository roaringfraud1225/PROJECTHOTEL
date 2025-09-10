#!/bin/bash

# Скрипт для сборки презентаций курса ООП и ФП
# Требует установленный Marp CLI

echo "🚀 Начинаем сборку презентаций курса ООП и ФП..."

# Проверяем наличие Marp CLI
if ! command -v marp &> /dev/null; then
    echo "❌ Marp CLI не установлен. Устанавливаем..."
    npm install -g @marp-team/marp-cli
fi

# Создаем директории для выходных файлов
mkdir -p output/pdf
mkdir -p output/html

# Функция для сборки презентации
build_presentation() {
    local input_file=$1
    local output_name=$2
    
    if [ -f "$input_file" ]; then
        echo "📝 Собираем: $input_file"
        
        # Сборка в PDF
        marp "$input_file" --pdf --output "output/pdf/${output_name}.pdf"
        
        # Сборка в HTML
        marp "$input_file" --html --output "output/html/${output_name}.html"
        
        echo "✅ Готово: ${output_name}"
    else
        echo "⚠️  Файл не найден: $input_file"
    fi
}

echo "📚 Собираем лекции по ООП..."

# Лекции по ООП
build_presentation "lectures/oop/01-introduction.md" "01-oop-introduction"
build_presentation "lectures/oop/02-inheritance-polymorphism.md" "02-oop-inheritance-polymorphism"
build_presentation "lectures/oop/03-interfaces-abstract.md" "03-oop-interfaces-abstract"
build_presentation "lectures/oop/04-collections-generics.md" "04-oop-collections-generics"
build_presentation "lectures/oop/05-exceptions-logging.md" "05-oop-exceptions-logging"
build_presentation "lectures/oop/06-files-serialization.md" "06-oop-files-serialization"
build_presentation "lectures/oop/07-gui-javafx.md" "07-oop-gui-javafx"
build_presentation "lectures/oop/08-design-patterns.md" "08-oop-design-patterns"
build_presentation "lectures/oop/08-solid-principles.md" "08-oop-solid-principles"
build_presentation "lectures/oop/10-architecture.md" "10-oop-architecture"
build_presentation "lectures/oop/11-multithreading.md" "11-oop-multithreading"
build_presentation "lectures/oop/12-networking.md" "12-oop-networking"

echo "📚 Собираем лекции по ФП..."

# Лекции по ФП
build_presentation "lectures/fp/13-introduction-haskell.md" "13-fp-introduction-haskell"
build_presentation "lectures/fp/14-data-types.md" "14-fp-data-types"
build_presentation "lectures/fp/15-higher-order.md" "15-fp-higher-order"
build_presentation "lectures/fp/16-monads-io.md" "16-fp-monads-io"

echo "🔬 Собираем лабораторные работы..."

# Лабораторные по ООП
build_presentation "labs/oop/01-basic-classes.md" "lab01-oop-basic-classes"
build_presentation "labs/oop/02-inheritance.md" "lab02-oop-inheritance"
build_presentation "labs/oop/03-interfaces.md" "lab03-oop-interfaces"
build_presentation "labs/oop/04-collections-generics.md" "lab04-oop-collections-generics"
build_presentation "labs/oop/05-exceptions.md" "lab05-oop-exceptions"
build_presentation "labs/oop/06-game-board-movement.md" "lab06-oop-game-board-movement"
build_presentation "labs/oop/07-resource-system.md" "lab07-oop-resource-system"
build_presentation "labs/oop/08-building-system.md" "lab08-oop-building-system"
build_presentation "labs/oop/09-design-patterns.md" "lab09-oop-design-patterns"
build_presentation "labs/oop/10-application-architecture.md" "lab10-oop-application-architecture"
build_presentation "labs/oop/11-multithreading.md" "lab11-oop-multithreading"
build_presentation "labs/oop/12-networking.md" "lab12-oop-networking"
build_presentation "labs/oop/13-ai-opponent.md" "lab13-oop-ai-opponent"
build_presentation "labs/oop/14-performance-optimization.md" "lab14-oop-performance-optimization"
build_presentation "labs/oop/15-advanced-gui.md" "lab15-oop-advanced-gui"
build_presentation "labs/oop/16-testing.md" "lab16-oop-testing"
build_presentation "labs/oop/17-documentation.md" "lab17-oop-documentation"
build_presentation "labs/oop/18-final-project.md" "lab18-oop-final-project"

# Лабораторные по ФП
build_presentation "labs/fp/19-basic-functions.md" "lab19-fp-basic-functions"
build_presentation "labs/fp/20-data-types-pattern-matching.md" "lab20-fp-data-types-pattern-matching"
build_presentation "labs/fp/21-monads-io.md" "lab21-fp-monads-io"
build_presentation "labs/fp/22-final-project.md" "lab22-fp-final-project"
build_presentation "labs/fp/23-parallelism.md" "lab23-fp-parallelism"
build_presentation "labs/fp/24-comparative-analysis.md" "lab24-fp-comparative-analysis"

echo "📋 Создаем индексный файл..."

# Создаем HTML индекс
cat > output/index.html << EOF
<!DOCTYPE html>
<html lang="ru">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Курс ООП и ФП - Группа 203</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .section { margin: 20px 0; }
        .lecture, .lab { margin: 10px 0; padding: 10px; border-left: 4px solid #007acc; }
        .lecture { background-color: #f0f8ff; }
        .lab { background-color: #f0fff0; }
        a { color: #007acc; text-decoration: none; }
        a:hover { text-decoration: underline; }
        h1 { color: #333; }
        h2 { color: #007acc; }
    </style>
</head>
<body>
    <h1>🎓 Курс "Объектно-ориентированное и функциональное программирование"</h1>
    <p><strong>Группа:</strong> 203 | <strong>Семестр:</strong> Осенний 2024</p>
    
    <div class="section">
        <h2>📚 Лекции по ООП (Java)</h2>
        <div class="lecture">
            <a href="html/01-oop-introduction.html">Лекция 1: Введение в ООП</a>
            <br><small>PDF: <a href="pdf/01-oop-introduction.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/02-oop-inheritance-polymorphism.html">Лекция 2: Наследование и полиморфизм</a>
            <br><small>PDF: <a href="pdf/02-oop-inheritance-polymorphism.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/03-oop-interfaces-abstract.html">Лекция 3: Интерфейсы и абстрактные классы</a>
            <br><small>PDF: <a href="pdf/03-oop-interfaces-abstract.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/04-oop-collections-generics.html">Лекция 4: Коллекции и Generics</a>
            <br><small>PDF: <a href="pdf/04-oop-collections-generics.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/05-oop-exceptions-logging.html">Лекция 5: Исключения и логирование</a>
            <br><small>PDF: <a href="pdf/05-oop-exceptions-logging.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/06-oop-files-serialization.html">Лекция 6: Файлы и сериализация</a>
            <br><small>PDF: <a href="pdf/06-oop-files-serialization.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/07-oop-gui-javafx.html">Лекция 7: GUI и JavaFX</a>
            <br><small>PDF: <a href="pdf/07-oop-gui-javafx.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/08-oop-design-patterns.html">Лекция 8: Паттерны проектирования</a>
            <br><small>PDF: <a href="pdf/08-oop-design-patterns.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/08-oop-solid-principles.html">Лекция 8: SOLID принципы</a>
            <br><small>PDF: <a href="pdf/08-oop-solid-principles.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/10-oop-architecture.html">Лекция 10: Архитектура приложений</a>
            <br><small>PDF: <a href="pdf/10-oop-architecture.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/10-oop-multithreading.html">Лекция 10: Многопоточность</a>
            <br><small>PDF: <a href="pdf/10-oop-multithreading.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/11-oop-networking.html">Лекция 11: Сетевое программирование</a>
            <br><small>PDF: <a href="pdf/11-oop-networking.pdf">скачать</a></small>
        </div>
    </div>
    
    <div class="section">
        <h2>📚 Лекции по ФП (Haskell)</h2>
        <div class="lecture">
            <a href="html/13-fp-introduction-haskell.html">Лекция 13: Введение в Haskell</a>
            <br><small>PDF: <a href="pdf/13-fp-introduction-haskell.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/14-fp-data-types.html">Лекция 14: Типы данных</a>
            <br><small>PDF: <a href="pdf/14-fp-data-types.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/15-fp-higher-order.html">Лекция 15: Функции высшего порядка</a>
            <br><small>PDF: <a href="pdf/15-fp-higher-order.pdf">скачать</a></small>
        </div>
        <div class="lecture">
            <a href="html/16-fp-monads-io.html">Лекция 16: Монады и IO</a>
            <br><small>PDF: <a href="pdf/16-fp-monads-io.pdf">скачать</a></small>
        </div>
    </div>
    
    <div class="section">
        <h2>🔬 Лабораторные работы по ООП (Java)</h2>
        <div class="lab">
            <a href="html/lab01-oop-basic-classes.html">Лабораторная 1: Базовые классы</a>
            <br><small>PDF: <a href="pdf/lab01-oop-basic-classes.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab02-oop-inheritance.html">Лабораторная 2: Наследование</a>
            <br><small>PDF: <a href="pdf/lab02-oop-inheritance.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab03-oop-interfaces.html">Лабораторная 3: Интерфейсы</a>
            <br><small>PDF: <a href="pdf/lab03-oop-interfaces.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab04-oop-collections-generics.html">Лабораторная 4: Коллекции и Generics</a>
            <br><small>PDF: <a href="pdf/lab04-oop-collections-generics.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab05-oop-exceptions.html">Лабораторная 5: Исключения</a>
            <br><small>PDF: <a href="pdf/lab05-oop-exceptions.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab06-oop-game-board-movement.html">Лабораторная 6: Игровое поле и движение</a>
            <br><small>PDF: <a href="pdf/lab06-oop-game-board-movement.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab07-oop-resource-system.html">Лабораторная 7: Система ресурсов</a>
            <br><small>PDF: <a href="pdf/lab07-oop-resource-system.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab08-oop-building-system.html">Лабораторная 8: Система зданий</a>
            <br><small>PDF: <a href="pdf/lab08-oop-building-system.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab09-oop-design-patterns.html">Лабораторная 9: Паттерны проектирования</a>
            <br><small>PDF: <a href="pdf/lab09-oop-design-patterns.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab10-oop-application-architecture.html">Лабораторная 10: Архитектура приложения</a>
            <br><small>PDF: <a href="pdf/lab10-oop-application-architecture.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab11-oop-multithreading.html">Лабораторная 11: Многопоточность</a>
            <br><small>PDF: <a href="pdf/lab11-oop-multithreading.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab12-oop-networking.html">Лабораторная 12: Сетевое программирование</a>
            <br><small>PDF: <a href="pdf/lab12-oop-networking.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab13-oop-ai-opponent.html">Лабораторная 13: ИИ противник</a>
            <br><small>PDF: <a href="pdf/lab13-oop-ai-opponent.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab14-oop-performance-optimization.html">Лабораторная 14: Оптимизация производительности</a>
            <br><small>PDF: <a href="pdf/lab14-oop-performance-optimization.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab15-oop-advanced-gui.html">Лабораторная 15: Продвинутый GUI</a>
            <br><small>PDF: <a href="pdf/lab15-oop-advanced-gui.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab16-oop-testing.html">Лабораторная 16: Тестирование</a>
            <br><small>PDF: <a href="pdf/lab16-oop-testing.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab17-oop-documentation.html">Лабораторная 17: Документация</a>
            <br><small>PDF: <a href="pdf/lab17-oop-documentation.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab18-oop-final-project.html">Лабораторная 18: Финальный проект</a>
            <br><small>PDF: <a href="pdf/lab16-oop-final-project.pdf">скачать</a></small>
        </div>
    </div>
    
    <div class="section">
        <h2>🔬 Лабораторные работы по ФП (Haskell)</h2>
        <div class="lab">
            <a href="html/lab19-fp-basic-functions.html">Лабораторная 19: Базовые функции</a>
            <br><small>PDF: <a href="pdf/lab19-fp-basic-functions.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab20-fp-data-types-pattern-matching.html">Лабораторная 20: Типы данных и паттерн-матчинг</a>
            <br><small>PDF: <a href="pdf/lab20-fp-data-types-pattern-matching.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab21-fp-monads-io.html">Лабораторная 21: Монады и IO</a>
            <br><small>PDF: <a href="pdf/lab21-fp-monads-io.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab22-fp-final-project.html">Лабораторная 22: Финальный проект</a>
            <br><small>PDF: <a href="pdf/lab22-fp-final-project.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab23-fp-parallelism.html">Лабораторная 23: Параллелизм</a>
            <br><small>PDF: <a href="pdf/lab23-fp-parallelism.pdf">скачать</a></small>
        </div>
        <div class="lab">
            <a href="html/lab24-fp-comparative-analysis.html">Лабораторная 24: Сравнительный анализ</a>
            <br><small>PDF: <a href="pdf/lab24-fp-comparative-analysis.pdf">скачать</a></small>
        </div>
    </div>
    
    <div class="section">
        <h2>📊 Статистика курса</h2>
        <p><strong>Всего лекций:</strong> 16 (12 ООП + 4 ФП)</p>
        <p><strong>Всего лабораторных:</strong> 24 (18 ООП + 6 ФП)</p>
        <p><strong>Языки программирования:</strong> Java, Haskell</p>
        <p><strong>Проект:</strong> Пошаговая стратегия</p>
    </div>
    
    <div class="section">
        <h2>🔗 Полезные ссылки</h2>
        <p><a href="README.md">📖 README проекта</a></p>
        <p><a href="course-plan.md">📅 План курса</a></p>
        <p><a href="project-requirements.md">📋 Требования к проекту</a></p>
    </div>
</body>
</html>
EOF

echo "🎉 Сборка завершена!"
echo "📁 Файлы сохранены в директории output/"
echo "🌐 Откройте output/index.html для просмотра всех материалов"

# Подсчитываем количество созданных файлов
echo ""
echo "📊 Статистика:"
echo "   - Лекций по ООП: $(find output/pdf -name "*oop*.pdf" | wc -l | tr -d ' ')"
echo "   - Лекций по ФП: $(find output/pdf -name "*fp*.pdf" | wc -l | tr -d ' ')"
echo "   - Лабораторных по ООП: $(find output/pdf -name "*lab*oop*.pdf" | wc -l | tr -d ' ')"
echo "   - Лабораторных по ФП: $(find output/pdf -name "*lab*fp*.pdf" | wc -l | tr -d ' ')"
echo "   - Всего файлов: $(find output/pdf -name "*.pdf" | wc -l | tr -d ' ')"
