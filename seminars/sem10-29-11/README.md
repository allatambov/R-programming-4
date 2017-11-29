# Семинар 10. Графики в ggplot2.

*29 ноября 2017*

Все графики строятся с помощью ggplot2. 

1. Загрузите базу данных `demography.csv`. В ней содержатся данные по населению Белгородской и Калужской областей за 2015 год (источник -- Росстат).

2. Создайте переменную *young_share* -- процент населения возраста, моложе трудоспособного. Создайте переменную *trud_share* -- процент населения трудоспособного возраста и *old_share* -- процент населения возраста, старше трудоспособного.

3. Постройте гистограмму для доли трудоспособного населения в процентах. Измените цвет гистограммы, добавьте rugs. Добавьте вертикальную линию, которая отчерчивает медианное значение доли трудоспособного населения в процентах.

4. Постройте сглаженные графики плотности распределения для доли трудоспособного населения в процентах по регионам (два графика в одной плоскости). Настройте цвета и прозрачность заливки. По графикам плотности определите, имеет ли смысл для визуализации распределения доли трудоспособного населения строить скрипичные диаграммы (*violin plot*). Если да, постройте их (так же по группам). Если нет, постройте ящики с усами. 

5. Постройте диаграмму рассеяния для переменных *young_share* и *old_share*. Можно ли сказать, что чем больше процент молодого населения (моложе трудоспособного населения), тем меньше процент пожилых людей (старше трудоспособного возраста)? Поменяйте цвет и тип маркера для точек.

6. Создайте переменную *male_share* -- доля мужского населения в районе/городе (в процентах). Создайте переменную *male*, которая принимает значение 1, если доля мужчин в муниципальном районе/городе больше доли женщин, и значение 0 -- во всех остальных случаях.

7. Постройте пузырьковую диаграмму (*bubble plot*) для переменных *young_share* и *old_share*, учитывая информацию о доле мужчин в районе и о том, преобладают ли мужчины в районе или нет.

8. Постройте столбиковую диаграмму (*bar plot*), которая показывала бы, сколько в базе данных районов Белгородской области, а сколько -- Калужской.