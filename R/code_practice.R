seq_len(10)
seq(from = 0, 
    to = 1, 
    length.out = 5)

n_participants <- 25
seq(0, 1, length.out = n_participants)

set.seed(2025-02-20)
strawberry <- 1:10
sample(strawberry, size = 3)

rep(5, times = 13)
rep('strawberry', 17)
rep('cake', 3)

names <- c('bicycle', 'tomato', 'mosquito')
rep(names, times = 4)

my_date <- as.Date('2025-02-20')
str(my_date)
class(my_date)

round(c(0.567, 4.1), digits = 1)
sum(5, 6)

obj <- c('bicycle', 'tomato', 'mosquito')
is.na(obj)
table(obj, useNA = 'always')

obj == 'tomato'
'tomato' %in% obj

5 %in% 1:5
1:5 %in% 5

plot(1, 2, pch = 22)
abline(v = 1.1, lty = 10, col = 'red')

library(tidyverse)
n_rep <- 3
t <- tibble(val1 = 1:3,
            val2 = rep(NA, n_rep))
t$val2[1]

for (agent in names) {
  if (agent == 'mosquito') print(agent)
  if (agent != 'mosquito') print('not mosquito')
}

# empty_vec <- rep(NA, times = n_rep)
# fill_me_in <- c('a', 'b', 'c')

for (agent in seq_len(n_rep)) {
  if (agent == 3) mynewval <- 'three'
  if (agent != 3) mynewval <- 'not_three'
  t$val2[agent] <- mynewval
}

hist(runif(100))
hist(rbinom(1000, size = 1, prob = 0.1))     
plot(density(rnbinom(1000, prob = 0.2, size = 1)))     
     
