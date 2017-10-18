Домашнее задание 3
================
Алла Тамбовцева

Формат сдачи
------------

*Срок сдачи:*

16 октября 2017, 22:00

*Формат сдачи:*

Студенту необходимо на выбор выполнить задачи базового или продвинутого блока. Результат выполнения домашнего задания 3: файл с расширением `.R`. Этот файл нужно загрузить по [ссылке](https://www.dropbox.com/request/ntmocOmeNr4YrSEhKq7o).

Базовый блок
------------

Скачайте с GitHub файл `hw3-rcode.R`, переименуйте его `hw3-rcode-surname.R`, где `surname` - Ваша фамилия латиницей. Откройте этот файл в R - в него Вы будете вписывать код для задач.

**Полезный факт:** загружать базу данных можно прямо по ссылке, не скачивая ее, если ссылка заканчивается расширением файла (например, `https://..../myfile.csv`). Для этого нужно вставить ссылку в качестве аргумента функции, используемой для загрузки файла.

### Задание 1

1.  Скачайте с сайта [British Election Study](http://www.britishelectionstudy.com/data-objects/cross-sectional-data/) базу данных Face-to-face Post-election Survey (2015) в формате `dta` (STATA File). Загрузите в R эту базу данных. Посмотрите на нее.
2.  Сохраните эту базу данных в формате `csv`, указав в качестве разделителя точку с запятой (возможно, потребуется запросить help для `write.csv()`).

*Для выполнения заданий 2-4 Вам необходимо загрузить [базу данных](https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/Titanic.csv), содержащую информацию по пассажирам "Титаника". Описание базы данных можно почитать [здесь](https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/doc/datasets/Titanic.html). Назовите загруженную базу данных `df`.*

### Задание 2

1.  Сколько в базе данных наблюдений? Сколько переменных? Какие это переменные? Какого типа?
2.  Сколько в базе данных строк, которые не содержат пропущенных значений? Сохраните строки, содержащие пропущенные значения, в отдельную базу данных `df_na`.
3.  Постройте график, который показывал бы частоту, с которой встречаются пропущенные значения в каждой из переменных базы данных. В какой переменной больше всего пропущенных значений?
4.  Постройте график, который позволит определить паттерны пропущенных значений. Можно ли по полученным результатам сделать вывод о том, что значения в базе пропущены “системно” (часто нет ответов на определенный вопрос или вопросы)? Может ли это быть связано со спецификой самих вопросов?
5.  Удалите в базе данных пропущенные значения.

### Задание 3

*Это задание выполняется без использования библиотеки dplyr*.

1.  Добавьте в базу данных бинарную переменную `female`, где значение 0 соответствует пассажирам мужского пола, а 1 - пассажирам женского пола. Не забудьте: бинарная переменная - всегда числовая (целочисленная). Готовую переменную `SexCode` использовать нельзя.

2.  Представьте, что в исследовании нас интересуют пассажиры старше 25 лет и не старше 45 лет, которые путешествовали вторым или третьим классом. Сохраните соответствующие строки в базу данных `df2`.

3.  Сколько на "Титанике" (согласно базе данных `df`) было пассажиров мужского пола? Женского пола?

4.  Сколько лет было самому молодому пассажиру среди выживших? А самому старому? Каков средний возраст пассажиров первого класса, которые выжили в катастрофе?

### Задание 4

*Для выполнения этого задания нужно обязательно использовать библиотеку dplyr, а также оператор %&gt;% из этой библиотеки.*

1.  Добавьте в базу данных `df` числовую переменную `ClassCode`, где значением 1 закодирован первый класс, 2 - второй, 3 - третий. Сделайте переменную `Age` целочисленной (integer).

2.  Оставьте в базе данных только тех пассажиров, которые не моложе 18 лет.

3.  Сгруппируйте пассажиров по классу, которым они путешествовали. Сколько пассажиров разных классов было на "Титанике"?

4.  В каком классе средний возраст выживших пассажиров выше?

5.  Кого больше: выживших мужчин из первого класса или выживших женщин из третьего класса?

Продвинутый блок
----------------

Для выполнения этого задания Вам нужно будет поработать с API ВКонтакте.

1.  Установите библиотеку `vkR` (на 4 курсе название этой библиотеки, наверное, немного напрягает). Ознакомьтесь с [документацией](https://cran.r-project.org/web/packages/vkR/vkR.pdf) по этой библиотеке и [инструкцией](https://cs.hse.ru/data/2017/04/04/1168473284/vkR_eng.pdf) по работе с API vk от разработчика этой библиотеки Дмитрия Сорокина. Авторизуйтесь согласно инструкции. **Альтернатива:** можете воспользоваться [библиотекой](https://github.com/denisStukal/Rvk) `Rvk`, написанной Денисом Стукалом. Думаю, не нужно объяснять, кто это :).

2.  Выберите 10 человек из списка друзей ВКонтакте, определите их id (можно вручную). Сохраните их id в вектор my\_friends. Желательно выбирать тех пользователей, у которых много разной информации на странице (какой - см. далее).
3.  Для 10 выбранных друзей с помощью библиотеки `vkR` соберите информацию (см. ниже) и сохраните ее в базу данных `df`. База данных должна включать следующие переменные:

-   **id**: id пользователя
-   **first\_name**: имя
-   **last\_name**: фамилия
-   **country**: страна пользователя
-   **home\_town**: родной город пользователя
-   **sex**: пол пользователя
-   **num\_friends**: число друзей пользователя
-   **polit\_views**: политические убеждения пользователя
-   **religion**: религиозные убеждения пользователя

Далее - все выполняется с помощью библиотеки`dplyr`.

1.  В API в контакте существует своя система кодирования пропущенных значений. Например, если пол пользователя не указан, выдается значение 0. Измените переменную `sex` таким образом, чтобы значение 1 соответствовало пользователю мужского пола, 0 - женского, а если значение пола не указано, то оно кодировалось как пропущенное (NA). Преобразуйте все текстовые переменные в базе (кроме имени и фамилии пользователя) в факторные.
2.  Посчитайте среднее число друзей у пользователей мужского и женского пола в Вашей базе. У кого среднее число друзей больше?

3.  Сгруппируйте пользователей по политическим убеждениям и определите, сколько приверженцев разных политических убеждений среди выбранных 10 друзей. Если у всех выбранных пользователей убеждения не указаны, проделайте аналогичные операции, сгруппировав наблюдения по другой переменной (при необходимости, можете добавить в базу переменную, значения которой точно будут у многих).