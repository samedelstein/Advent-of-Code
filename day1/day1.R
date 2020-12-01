library(tidyverse)
#Part 1 
inputs <- readLines('day1/day01.txt')
inputs <- as.numeric(inputs)
result <- NULL

for (i in 1:length(inputs)) {
  
  result[i] <-  2020 - inputs[i]

}

inputs[result %in% inputs] * (2020 - inputs[result %in% inputs])

#Part 2

df = read.delim('day1/day01.txt',
                header = F,col.names = 'input') %>%
  mutate(diff = 2020 - input)
trip = lapply(df$diff,
              function(x){df$input[(x - df$input)%in%df$input]}
)

prod(unique(unlist(trip))) 
