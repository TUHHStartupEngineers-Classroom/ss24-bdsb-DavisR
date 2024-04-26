library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)


diamonds2_long <- pivot_longer(diamonds2,
                               cols      = c("2008", "2009"), 
                               names_to  = 'year', 
                               values_to = 'price')
head(diamonds2_long, n = 5)

model <- lm(price ~ ., data = diamonds2_long)
model

# New data
diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>% head(n = 5)

diamonds3_wide = pivot_wider(diamonds3,
                             names_from  = "dimension",
                             values_from = "measurement")
head(diamonds3_wide, n = 5)

# New data
diamonds4 <- readRDS("diamonds4.rds")

diamonds4


diamonds4_sep = separate(diamonds4,
                         col = dim,
                         into = c("x", "y", "z"),
                         sep = "/",
                         convert = T)

diamonds4_sep

# New Data
diamonds5 <- readRDS("diamonds5.rds")

diamonds5

diamonds5_united = unite(diamonds5,
                         clarity, clarity_prefix, clarity_suffix, 
                         sep = '')

diamonds5_united
