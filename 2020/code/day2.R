library(tidyverse)

#part 1

inputs <- readLines('2020/day2/day2.txt')
head(inputs)

data.frame(inputs) %>%
  extract(inputs, c('min', 'max', 'letter', 'password'), "(\\d+)-(\\d+) (.): *(.*)", convert = TRUE) %>%
  mutate(count_occurrences = str_count(password, letter),
         policies_passed = case_when(count_occurrences >= min & count_occurrences <= max ~ 1,
                   TRUE ~ 0)) %>%
  summarise(sum(policies_passed))


#part 2

data.frame(inputs) %>%
  extract(inputs, c('min', 'max', 'letter', 'password'), "(\\d+)-(\\d+) (.): *(.*)", convert = TRUE) %>%
  mutate(count = (str_sub(password, min,min)==letter) +
           (str_sub(password,max,max) == letter)) %>%
  filter(count == 1)
