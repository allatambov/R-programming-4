# размеченные по тональности твиты взяты отсюда (урезанные версии исходных баз):
# http://study.mokoron.com/

# загрузим базу данных и выберем из нее 50 положительно окрашенных твитов и 50 отрицательно окрашенных
df <- read.csv("tweets.csv")
View(df)
data <- rbind(head(df, 50), tail(df, 50))

# установим библиотеку RTextTools и обратимся к ней
# install.packages("RTextTools")
library(RTextTools)

# создадим матрицу слово-документ
dtm <- create_matrix(data$tweet, language = "russian", removeStopwords = TRUE, 
                       removeNumbers = TRUE, stemWords = TRUE, weighting = tm::weightTfIdf)
# превратим в обычную матрицу
mat <- as.matrix(dtm)

# разобьем данные на обучающую и тестовую выборки
# в обучающую положим твиты с четными индексами,
# а в тестовую - с нечетными
train <- which(1:nrow(data) %% 2 == 0)
test <- which(1:nrow(data) %% 2 != 0)

# создадим объект "контейнер", в котором будет хранится наша матрица, 
# информация об обучающей и тестовой выборках
container <- create_container(dtm, as.numeric(as.factor(data$sent)), trainSize = train, 
                             testSize = test, virgin = FALSE) 

# обучим модель с помощью метода опорных векторов (SVM)
model <- train_models(container, "SVM")

# получим результаты - предсказанные значения в том числе
results <- classify_models(container, model)

# получим оенку качества - проверяется на тестовой выборке из "контейнера"
analytics <- create_analytics(container, results)
summary(analytics)

# качество низкое, но на таких твитах это неудивительно (особенно, если учесть, что их всего 50)

# Дополнение: библиотека для разбивки данных на обучающую и тестовую выборки
# install.packages('caret')
library(caret)

# одно разбиение (times = 1), 80% данных уходят в обучающую выборку (p = 0.8)
trainRows <- createDataPartition(data$sent, 
                                 p = 0.8, list = FALSE, times = 1)
train <- data[trainRows,]
test <- data[-trainRows,]
