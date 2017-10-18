##### Подготовка базы данных #####

library(dplyr)

# загрузим базу данных 

el <- read.csv("47130-8314.csv", encoding = "UTF-8")

# выберем интересующие нас столбцы и дадим им содержательные названия

el <- el %>% select(kom1, kom2, kom3, X1, X9, X10, X19, X20, X21, X22, X23)
colnames(el) <- c("region", "tik", "uik", "total", "invalid", "valid",
                  "Zh", "Zug", "Mir", "Pro", "Put")

# создадим переменные для явки и уберем лишние строки (и NAs)

el <- el %>% mutate(turnout = invalid + valid)
el <- el %>% mutate(turnout_perc = turnout / total * 100)
el <- el %>% filter(region != "Территория за пределами РФ", 
                    region != "Город Байконур (Республика Казахстан)")
el <- na.omit(el)

# агрегируем результаты по регионам (суммируем) и добавим показатели в процентах

aggr_reg  <- el %>% group_by(region) %>%
  summarise(total = sum(total),
            turnout = sum(turnout),
            Zh = sum(Zh),
            Zug = sum(Zug),
            Mir = sum(Mir),
            Pro = sum(Pro),
            Put = sum(Put)) 

aggr_reg <- aggr_reg %>% mutate(turnout_perc = turnout / total * 100, 
                                Zh_perc = Zh / turnout * 100, 
                                Zug_perc = Zug / turnout * 100, 
                                Mir_perc = Mir / turnout * 100,
                                Pro_perc = Pro / turnout * 100,
                                Put_perc = Put / turnout * 100)

# создадим переменную reg_type (тип региона), 
# которая принимает значения "область", "республика", "край", "округ", "город"

# пример поиска паттерна в текстовом векторе

regs <- c("Белгородская область", "Владимирская область", 
          "Кабардино-Балкарская Республика", "Город Москва")
grepl(pattern = "область", regs)
as.integer(grepl(pattern = "область", regs))

# теперь проделаем эти операции для разных типов регионов в нашей базе

aggr_reg <- aggr_reg %>% mutate(resp = as.integer(grepl(pattern = "Республика", region)),
                                oblast = as.integer(grepl(pattern = "область", region)),
                                okrug = as.integer(grepl(pattern = "округ", region)),
                                krai = as.integer(grepl(pattern = "край", region)),
                                gorod = as.integer(grepl(pattern = "Город", region)))
View(aggr_reg)

# осталось скомбинировать полученные дамми-переменные - создать переменную reg_type

aggr_reg$reg_type <- names(aggr_reg[15:19])[max.col(aggr_reg[15:19])]

View(aggr_reg)

####################################################################################

##### Качественные данные #####

# столбиковая диаграмма (bar plot)

table(aggr_reg$reg_type)
barplot(table(aggr_reg$reg_type))

# добавим заголовок (main), и более внятные подписи к столбикам (names.arg)

barplot(table(aggr_reg$reg_type), 
        main = "Типы регионов", 
        names.arg = c("город фед. \n знач.", 
                      "край", "область", 
                      "округ", "республика"))

# поменяем цвет графика (col), добавим подпись к оси y (ylab)

barplot(table(aggr_reg$reg_type), 
        main = "Типы регионов", 
        names.arg = c("город фед. \n знач.", 
                      "край", "область", 
                      "округ", "республика"),
        col = "red",
        ylab = "число регионов")

# а теперь сделаем ось y более детальной -- добавим побольше делений

# строим график
# axes = FALSE

barplot(table(aggr_reg$reg_type), 
        main = "Типы регионов", 
        names.arg = c("город фед. \n знач.", 
                      "край", "область", 
                      "округ", "республика"),
        col = "red",
        ylab = "число регионов", axes = FALSE) 

# корректируем оси

axis(2, at = seq(from = 0, to = 50, by = 5))

# столбиковая диаграмма с процентами

perc <- table(aggr_reg$reg_type)/sum(table(aggr_reg$reg_type)) * 100 

perc

barplot(perc)

# строим график 
# ylim - границы значений по оси y

barplot(perc, 
        main = "Типы регионов", 
        names.arg = c("город фед. \n знач.", 
                      "край", "область", 
                      "округ", "республика"),
        col = "red",
        ylim = c(0, 65),
        ylab = "доля регионов (в %)", 
        axes = FALSE)

# корректируем оси

axis(2, at = seq(from = 0, to = 65, by = 5))

# чтобы было совсем здорово, добавим подписи с процентами на сам график

perc_labels <- round(perc, 2) # округляем
perc_labels <- paste(perc_labels, "%", sep = "") # приклеиваем знак %
perc_labels

# график

pl <- barplot(perc, 
              main = "Типы регионов", 
              names.arg = c("город фед. \n знач.", 
                            "край", "область", 
                            "округ", "республика"),
              col = "red",
              ylim = c(0, 65),
              ylab = "доля регионов (в %)", 
              axes = FALSE)

# корректируем оси

axis(2, at = seq(from = 0, to = 65, by = 5))

# добавляем подписи

text(x = pl, y = perc, labels = perc_labels, pos = 3)

# Круговая диаграмма (pie chart)

pie(perc)

# да, я специально подобрала нетипичные названия цветов
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

colors <- c('thistle2', 'plum2', 'palevioletred', 'orchid4', 'purple3')

pie(perc, col = colors)

labs = c("город федерального \n значения", "край", "область", "округ", "республика")

# в отличие от barplot, для надписей используем аргумент labels

pie(perc, col = colors, main = "Типы регионов", labels = labs)

##### Количественные данные #####

# гистограмма

hist(aggr_reg$turnout_perc)

hist(aggr_reg$turnout_perc,
     col = "lightgreen", 
     main = "Выборы президента 2012 года",
     xlab = "Явка (в %)", 
     ylab = "Частоты")

hist(aggr_reg$turnout_perc, breaks = 10, col = "lightgreen")
hist(aggr_reg$turnout_perc, breaks = 20, col = "lightgreen")

# ящик с усами (boxplot)

boxplot(aggr_reg$Zug_perc, col = "yellow", main = "Процент голосов",
        sub = 'Зюганов ("Единая Россия")') 
