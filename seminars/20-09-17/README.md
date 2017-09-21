Семинар 1. Векторы, матрицы, списки.
================
Алла Тамбовцева
20 сентября 2017 г

Задание 1
---------

Дан вектор qq:

``` r
qq <- c(0, 72, 1, 8, 15, 22, 16, 4, 24)
```

-   Сохраните в вектор three элементы вектора qq, которые кратны 3.

``` r
three <- qq[qq %% 3 == 0]
three
```

    ## [1]  0 72 15 24

-   Сохраните в вектор four элементы вектора qq, которые кратны 4.

``` r
four <- qq[qq %% 4 == 0]
four
```

    ## [1]  0 72  8 16  4 24

-   Сохраните в вектор both элементы вектора qq, которые кратны и 3, и 4.

**Решение 1**

``` r
both <- qq[qq %% 3 == 0 & qq %% 4 == 0]
both
```

    ## [1]  0 72 24

**Решение 2**

``` r
three[three %in% four] # так
```

    ## [1]  0 72 24

``` r
four[four %in% three] # или так
```

    ## [1]  0 72 24

**Решение 3**

Решение подходит для данного случая, но оно не является общим: `intersect` - операция для работы с множествами, а множества не содержат повторяющихся элементов.

Если бы нас интересовало число элементов qq, кратных 3 и 4, и среди них были бы повторяющиеся значения, то функция `intersect` бы нам не подошла -- она бы убрала все повторяющиеся элементы.

``` r
both <- intersect(three, four)
both
```

    ## [1]  0 72 24

-   Создайте вектор all -- припишите в конец вектора three вектор four.

``` r
all <- c(three, four)
all
```

    ##  [1]  0 72 15 24  0 72  8 16  4 24

**Внимание:** функция `union` для множеств нам здесь не подойдет -- она уберет совпадающие значения.

Сравните:

``` r
c(three, four) # верно
```

    ##  [1]  0 72 15 24  0 72  8 16  4 24

``` r
union(three, four) # неверно
```

    ## [1]  0 72 15 24  8 16  4

-   Выведите на экран элементы вектора qq, которых нет в векторе all.

**Решение 1**

``` r
qq[!qq %in% all]
```

    ## [1]  1 22

**Решение 2**

Решение подходит для данного случая, но оно не является общим: `setdiff` - операция для работы с множествами, а множества не содержат повторяющихся элементов.

``` r
setdiff(qq, all)
```

    ## [1]  1 22

Задание 2
---------

1.  Поставьте библиотеку [randomNames](https://cran.r-project.org/web/packages/randomNames/randomNames.pdf). Обратитесь к ней через `library()`.

``` r
install.packages("randomNames")
```

``` r
library(randomNames)
```

1.  Создайте вектор из 100 испанских имен:

``` r
set.seed(1234) # чтобы у всех получались одинаковые результаты
names <- randomNames(100, which.names = "first", ethnicity = 4) 
```

1.  Будем считать, что эти 100 имен -- имена опрошенных респондентов. Создайте вектор со значениями возраста респондентов:

``` r
ages <- sample(16:75, 100, replace = TRUE) # replace = TRUE - с повторяющимися значениями
```

А также вектор polit -- политические взгляды респондентов:

``` r
views <- c("right", "left", "moderate", "indifferent")
polit <- sample(views, 100, replace = TRUE)
```

Теперь, когда у нас есть, с чем работать, можно выполнить задание.

-   Определите тип каждого вектора.

``` r
class(names)
```

    ## [1] "character"

``` r
class(ages)
```

    ## [1] "integer"

``` r
class(polit)
```

    ## [1] "character"

-   Создайте вектор id с номерами респондентов.

``` r
id <- 1:100
```

-   Определите, сколько среди респондентов людей в возрасте от 25 до 30 лет (включительно). Определите, какую долю респондентов в нашей сымпровизированной выборке составляют люди в возрасте от 25 до 30 лет. Выразите эту долю в процентах, округлите ее до 1 знака после запятой.

**Подробное решение**

``` r
resp <- ages[ages >= 25 & ages <= 30]
length(resp)
```

    ## [1] 6

``` r
share <- length(resp)/length(ages)
round(share * 100, 1)
```

    ## [1] 6

**Все сразу**

``` r
length(ages[ages >= 25 & ages <= 30])
```

    ## [1] 6

``` r
round(length(resp)/length(ages) * 100, 1)
```

    ## [1] 6

-   Создайте "факторный" вектор политических взглядов polit\_views. Сколько у полученного фактора уровней?

``` r
polit_views <- factor(polit)
str(polit_views) # 4 уровня
```

    ##  Factor w/ 4 levels "indifferent",..: 2 1 3 2 1 2 4 1 3 1 ...

-   Создайте матрицу M, столбцами которой являются векторы names, ages, polit\_views. Переименуйте столбцы как Name, Age и Polit. Назовите строки матрицы в соответствии с вектором id.

``` r
M <- cbind(names, ages, polit_views)
colnames(M) <- c("Name", "Age", "Polit")
rownames(M) <- id
head(M) # посмотрим на первые несколько строк матрицы
```

    ##   Name        Age  Polit
    ## 1 "Luis"      "55" "2"  
    ## 2 "Briana"    "47" "1"  
    ## 3 "Courtney"  "35" "3"  
    ## 4 "Mariah"    "62" "2"  
    ## 5 "Lateisha"  "47" "1"  
    ## 6 "Kassandra" "59" "2"

Задание 3
---------

В R есть функция `strsplit()`, которая позволяет разбивать строки (текстовые переменные) на части по определенным символам.

Пусть у нас есть строка s:

``` r
s <- "a,b,c,d"
```

Мы хотим получить из нее вектор из 6 букв. Применям функцию:

``` r
let <- strsplit(s, ",")
```

Получили почти то, что хотели. Почему почти? Потому что получили не вектор, а список!

``` r
class(let)
```

    ## [1] "list"

Превратим в вектор:

``` r
unlist(let)
```

    ## [1] "a" "b" "c" "d"

Теперь все в порядке, получили вектор из четырех элементов.

Теперь задание. Дана строка index:

``` r
index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
```

Получите из этой строки числовой вектор I.

**Подробное решение**

``` r
l <- strsplit(index, ";") # разбиваем по точке с запятой
v_comma <- unlist(l) # превращаем в вектор
v_dot <-gsub(",", ".", v_comma) # заменяем запятую на точку - иначе не превратим в числовой вектор
I <- as.numeric(v_dot)
I
```

    ## [1] 0.72 0.38 0.99 0.81 0.15 0.22 0.16 0.40 0.24

**Всё сразу**

``` r
I <- as.numeric(gsub(",", ".", unlist(strsplit(index, ";"))))
```

Задание 4
---------

Создайте из векторов из задания 2 список resp.

``` r
resp <- list(names, ages, polit)
```

-   Выведите на экран первый элемент списка resp.

``` r
resp[[1]]
```

    ##   [1] "Luis"       "Briana"     "Courtney"   "Mariah"     "Lateisha"  
    ##   [6] "Kassandra"  "Gerardo"    "Adrian"     "Marivi"     "Sovida"    
    ##  [11] "Rebecca"    "Karla"      "Brandon"    "Samantha"   "Michael"   
    ##  [16] "Yannely"    "John-Paul"  "Carlos"     "Michael"    "Kevin"     
    ##  [21] "Luis"       "Xavier"     "Jesus"      "Phillip"    "Michael"   
    ##  [26] "Rebecca"    "Jennifer"   "Kelsi"      "Kimberly"   "Jonathan"  
    ##  [31] "Corey"      "Alfonso"    "Adrian"     "Chloe"      "Michael"   
    ##  [36] "Shelby"     "Luiz"       "Damian"     "Ciara"      "Sierra"    
    ##  [41] "Kea"        "Sheila"     "Frankie"    "Briana"     "Miguel"    
    ##  [46] "Wendy"      "Alexis"     "Freddy"     "Ryan"       "Alicia"    
    ##  [51] "Adrian"     "Herber"     "Nayellie"   "Michelle"   "Diego"     
    ##  [56] "Courtney"   "Richardlee" "Jennifer"   "Antonio"    "Corina"    
    ##  [61] "Maria"      "Brendan"    "Coleman"    "Michael"    "Jordan"    
    ##  [66] "Liz"        "Leon"       "Genesis"    "Cristian"   "Crystal"   
    ##  [71] "Leroy"      "Anissa"     "Ruben"      "Silvia"     "Jason"     
    ##  [76] "Alondra"    "Avery"      "Diego"      "Rolando"    "Ana"       
    ##  [81] "Mckayla"    "Joell"      "Jesse"      "Heidi"      "Kaylan"    
    ##  [86] "Lidia"      "Adam"       "Devon"      "Gerardo"    "Domonique" 
    ##  [91] "Samuel"     "Magnolia"   "Joel"       "Kevin"      "Alejandro" 
    ##  [96] "Melina"     "Brycen"     "Darwin"     "Victor"     "Silvia"

-   Выведите на экран второй элемент третьего вектора в списке resp.

``` r
resp[[3]][2]
```

    ## [1] "indifferent"

-   Измените пятый элемент первого вектора в списке resp.

``` r
resp[[1]][5] <- "Maria"
resp[[1]]
```

    ##   [1] "Luis"       "Briana"     "Courtney"   "Mariah"     "Maria"     
    ##   [6] "Kassandra"  "Gerardo"    "Adrian"     "Marivi"     "Sovida"    
    ##  [11] "Rebecca"    "Karla"      "Brandon"    "Samantha"   "Michael"   
    ##  [16] "Yannely"    "John-Paul"  "Carlos"     "Michael"    "Kevin"     
    ##  [21] "Luis"       "Xavier"     "Jesus"      "Phillip"    "Michael"   
    ##  [26] "Rebecca"    "Jennifer"   "Kelsi"      "Kimberly"   "Jonathan"  
    ##  [31] "Corey"      "Alfonso"    "Adrian"     "Chloe"      "Michael"   
    ##  [36] "Shelby"     "Luiz"       "Damian"     "Ciara"      "Sierra"    
    ##  [41] "Kea"        "Sheila"     "Frankie"    "Briana"     "Miguel"    
    ##  [46] "Wendy"      "Alexis"     "Freddy"     "Ryan"       "Alicia"    
    ##  [51] "Adrian"     "Herber"     "Nayellie"   "Michelle"   "Diego"     
    ##  [56] "Courtney"   "Richardlee" "Jennifer"   "Antonio"    "Corina"    
    ##  [61] "Maria"      "Brendan"    "Coleman"    "Michael"    "Jordan"    
    ##  [66] "Liz"        "Leon"       "Genesis"    "Cristian"   "Crystal"   
    ##  [71] "Leroy"      "Anissa"     "Ruben"      "Silvia"     "Jason"     
    ##  [76] "Alondra"    "Avery"      "Diego"      "Rolando"    "Ana"       
    ##  [81] "Mckayla"    "Joell"      "Jesse"      "Heidi"      "Kaylan"    
    ##  [86] "Lidia"      "Adam"       "Devon"      "Gerardo"    "Domonique" 
    ##  [91] "Samuel"     "Magnolia"   "Joel"       "Kevin"      "Alejandro" 
    ##  [96] "Melina"     "Brycen"     "Darwin"     "Victor"     "Silvia"
