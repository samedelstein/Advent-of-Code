library(tidyverse)

inputs <- tibble(x = readLines('2020/data/day4.txt'))
required <- c("byr", 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')

fields <- inputs %>%
  mutate(passport = cumsum(x == "") )  %>%
  mutate(m = str_match_all(x, "(...)\\:([^ ]+)")) %>%
  mutate(f = map(m, ~ .[,2]),
         v = map(m, ~ .[,3])) %>%
  unnest(c(f,v)) %>%
  filter( f %in% required)

#part 1

fields %>% 
  count(passport) %>%
  summarise(answer = sum(n == 7))

#part 2

fields %>%
  extract(v, c('height', 'unit'), '(\\d+)(cm|in)', convert = TRUE, remove = FALSE) %>%
  mutate(valid = case_when(f == 'byr' ~ between(as.integer(v), 1920,2002),
                           f == 'iyr' ~ between(as.integer(v), 2010,2020),
                           f == 'eyr' ~ between(as.integer(v), 2020, 2030),
                           f == 'hgt' ~ ifelse(unit == 'cm', between(height, 150,193),
                                               between(height, 59, 76)),
                           f == 'hcl' ~ str_detect(v, '^#[0-9a-f]{6}$'),
                           f == 'ecl' ~ v %in% c('amb', 'blu', 'brn', 'gry', 'grn','hzl','oth'),
                           f == 'pid' ~ str_detect(v, '^[0-9]{9}$'))) %>%
  filter(valid) %>%
  count(passport) %>%
  summarize(answer = sum(n==7))
