########################### HW3 solution (advanced) ###########################

library(vkR)
library(dplyr)

# authorization

my_id <- #######
vkOAuth(my_id, "friends")
my_token <- "###"
setAccessToken(access_token = my_token)

# vector of friends' ids

my_friends <- c(12374247, 54281447, 68892630, 50467050, 
                4161834, 171544496, 8761370, 3990163, 
                16260739, 20473269)
# get info

my_fields <- "uid, first_name, last_name, country, sex, home_town, personal"
df <- getUsersExecute(my_friends, fields = my_fields)

# get friends of friends

their_friends <- getFriendsBy25(my_friends)
df$num_friends <- lengths(their_friends) # lengths - length of each vector in a list

# choose only personal info that is required (omit other subfields)

country <- df$country$title
polit_views <- df$personal$political
religion <- df$personal$religion

df <- select(df, id, first_name, last_name, sex, home_town, num_friends)
final <- cbind(df, country, polit_views, religion)

# manage with dplyr

final <- final %>% mutate(sex = sex - 1, 
                          home_town = as.factor(home_town),
                          polit_views = as.factor(polit_views))

# average number of friends by groups

final %>% group_by(sex) %>% summarise(avg_friends = mean(num_friends))

# group by political views

final %>% group_by(polit_views) %>% tally()

# relatively numerous groups - 4 (liberal) and NA 


