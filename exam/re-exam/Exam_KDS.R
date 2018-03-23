# Kudrevatykh Daniel, 142

# Задание 1

df <- read.csv('holidays.csv', sep = ",", encoding = "UTF-8")
str(df)
w <- df %>% select(weight.before)
re <- df %>% select(rtype)
# !!! Не работает
#1.1
w_lim <- w [w > 50 & round (w / 10, 1)]

w_lim


#1.2
table(df$rtype == 'active')
df$rtype <- as.factor(df$rtype)

# 502 человека предпочитают активный отдых, 498 - неактивный

# !!! Не работает
#1.3
active <- (re$rtype == 'active')

nre_act <- re(active)

# !!! Не работает
#1.4
re_num <- as.factor(re)
1 <- factor(active)


# Задание 2

#2.1

df <- df %>% mutate(weight.diff = weight.after - weight.before)
df <- df %>% mutate(status = replace(weight.diff, weight.diff > 0, 'not ok'),
                    status = replace(status, weight.diff <= 0, 'ok'))

#2.2
df_fem <- df %>% filter (df$gender == 'female')
score <- df %>% select(score)
females_23 <- df_fem %>% filter (sc >= 2 & sc <= 3)


#2.3

dim(females_23)
# 256 наблюдений и 7 переменных


#2.4
class('score')
class('gender')
class('weight.diff')
# все три переменных имеют класс character (строковая)

#2.5
sum(!complete.cases(df))
# 87 пропущенных значений

#2.6

library(mice)
library(VIM)

aggr(df)
# Пропущенные значения существуют в переменных веса до и после отдыха (не отобр. на графике), 
# и, как следствие, в дальнейшем теряются значения переменных разницы веса и статуса

matrixplot(df)

# То же самое показывает этот график в виде красных линий на переменных веса

#2.7
df <- na.omit(df)

# Задание 3

#3.1
# Возможно, дублирование вопроса. Но после удаления NA числа изменились, что говорит о "свершившемся отдыхе"

table(df$rtype == 'active')
df$rtype <- as.factor(df$rtype)

# 198 человека провели активный отдых, 201 - неактивный

#3.2
gender <- df %>% select(rtype)
df %>% group_by(gender) %>% summarise(median = median(score))
# женщины оценивали отдых чаще всего в три балла

#3.3
df %>% group_by(rtype) %>% summarise(mean = mean(weight.diff))
# в среднем больше всего веса набрали неактивные

#3.4
df %>% arrange(weight.before) %>% head(4)
df %>% arrange(weight.before) %>% tail(4)

# Задание 4

#4.1
