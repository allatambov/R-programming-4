# 1
name <- readline("Enter your name: ")
surname <- readline("Enter your surname: ")
print(sprintf("Hello, %s %s! Welcome to R!", name, surname))

#2
x <- readline("Enter a number: ")
if (as.numeric(x) == as.integer(x)){print("It is an integer.")} else{
  print("It is not an integer.")
}

#3
n <- readline("Enter number of elements: ")
n <- as.integer(n)
d <- rep(NA, n)

# 4
n <- readline("Enter number of elements: ")
n <- as.integer(n)
d <- rep(NA, n)
s <- 1:length(d)
# s <- 1:n

for (i in s){
  if (i %% 2 == 0){d[i] <- 1}
  else{d[i] <- 0}
}

#5
grades10 <- c(1, 4, 7, 8, 4, 3, 10, 9)
n <- length(grades10)
grades5 <- rep(NA, n)
indices <- 1:n

for (i in indices){
  if (grades10[i] == 0){grades5[i] <- 1}
  if (grades10[i] < 4){grades5[i] <- 2}
  if (grades10[i] >= 4 & grades10[i] <= 5){grades5[i]<-3}
  if (grades10[i] >= 6 & grades10[i] <=7){grades5[i] <-4}
  if (grades10[i] >= 8){grades5[i] <- 5}
}
