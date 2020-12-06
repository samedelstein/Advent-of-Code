library(tidyverse)
inputs <- tibble(x = readLines('2020/data/day6.txt'))

head(inputs)
df <- inputs %>%
  mutate(group = cumsum(x == '')) %>%
  filter(x != '') %>%
  add_count(group, name = 'group_total') %>%
  separate_rows(x, sep = '') %>%
  filter( x != '')

#part 1
answer_1 <- df %>%
  distinct(group, x) %>%
  count()

#part 2
answer_2 <- df %>%
  count(group,x,group_total) %>%
  filter(n == group_total) %>%
  count()
