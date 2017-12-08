### Code for the lecture on cluster analysis and visualization of results ###
### Full lecture will be posted later

library(dplyr)
library(ggplot2)

df <- read.csv("demography.csv")

# from seminar 10: add some columns

df <- df %>% mutate(young_share = young_total/popul_total * 100,
                    trud_share = wa_total/popul_total * 100,
                    old_share = ret_total/popul_total * 100)

df <- df %>% mutate(female = young_female + wa_female + ret_female,
                    male = young_male + wa_male + ret_male) %>% 
  mutate(fe_share = female / popul_total * 100,
         ma_share = male / popul_total * 100) %>%
  mutate(Male = factor(ifelse(ma_share > fe_share, 1, 0)))

# add columns urban_share and rural_share
df <- df %>% mutate(urban_share = urban_total / popul_total * 100,
                    rural_share = rural_total / popul_total * 100)

# select variables that ill be used for clustering of districts
dat <- df %>% select(district, urban_share, rural_share, fe_share, 
                     ma_share, young_share, trud_share, old_share)
View(dat)

# get rid of text variable with district names
# put this information into row names instead 
# use id instead of names - will be convenient in dendrograms
dat <- dat %>% select(-district)
rownames(dat) <- 1:dim(dat)[1]

# make a matrix of distances (Euclidean distance squared)
# do not forget to scale all the variables first
# scale: substract the mean and divide by standard deviation
distM <- dist(scale(dat))^2

# clustering using Ward method (algorithm performed on the distance matrix)
hc <- hclust(distM, method = "ward.D")

# dendrogram - visualization of clustering
plot(hc)

# decide to choose only two clusters
groups2 <- cutree(hc, k = 2)
groups2

# add cluster numbers into the dataset
dat$clust2 <- factor(groups2)

# plot a scatterplot with colored points
# points are in two colors - two clusters
ggplot(data = dat, aes(x = urban_share, y = old_share)) +
  geom_point(aes(color = clust2))

# now do the same, but for 3 clusters
groups3 <- cutree(hc, k = 3)
dat$clust3 <- factor(groups3)

ggplot(data = dat, aes(x = urban_share, y = trud_share)) +
  geom_point(aes(color = clust3))

# and what if we choose 7 clusters
groups7 <- cutree(hc, k = 7)
dat$clust7 <- factor(groups7)
ggplot(data = dat, aes(x = urban_share, y = trud_share)) +
  geom_point(aes(color = clust7))

# is it really necessary to choose so many clusters
# let's look whether mean values of seven clusters differ dramatically
dat %>% group_by(clust7) %>% 
  summarise(avg_young = mean(young_share))
