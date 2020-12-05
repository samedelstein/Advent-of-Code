library(tidyverse)

inputs <- tibble(x = readLines('2020/data/day5.txt'))

#part 1

first <- inputs %>%
  mutate(start_lower = 0,
         start_upper = 127,
         step_1_lower = case_when(substr(x, 1,1) == 'F' ~ start_lower,
                                  substr(x, 1,1) == 'B' ~ round((start_upper - start_lower)/2) + start_lower),
         step_1_upper = case_when(substr(x, 1,1) == 'F' ~ floor((start_upper - start_lower)/2) + start_lower,
                                  substr(x, 1,1) == 'B' ~ start_upper),
         
         step_2_lower = case_when(substr(x, 2,2) == 'F' ~ step_1_lower,
                                  substr(x, 2,2) == 'B' ~ round((step_1_upper - step_1_lower)/2) + step_1_lower),
         step_2_upper = case_when(substr(x, 2,2) == 'F' ~ floor((step_1_upper - step_1_lower)/2) + step_1_lower,
                                  substr(x, 2,2) == 'B' ~ step_1_upper),
         
         step_3_lower = case_when(substr(x, 3,3) == 'F' ~ step_2_lower,
                                  substr(x, 3,3) == 'B' ~ round((step_2_upper - step_2_lower)/2) + step_2_lower),
         step_3_upper = case_when(substr(x, 3,3) == 'F' ~ floor((step_2_upper - step_2_lower)/2) + step_2_lower,
                                  substr(x, 3,3) == 'B' ~ step_2_upper),
         
         step_4_lower = case_when(substr(x, 4,4) == 'F' ~ step_3_lower,
                                  substr(x, 4,4) == 'B' ~ round((step_3_upper - step_3_lower)/2) + step_3_lower),
         step_4_upper = case_when(substr(x, 4,4) == 'F' ~ floor((step_3_upper - step_3_lower)/2) + step_3_lower,
                                  substr(x, 4,4) == 'B' ~ step_3_upper),
         
         step_5_lower = case_when(substr(x, 5,5) == 'F' ~ step_4_lower,
                                  substr(x, 5,5) == 'B' ~ round((step_4_upper - step_4_lower)/2) + step_4_lower),
         step_5_upper = case_when(substr(x, 5,5) == 'F' ~ floor((step_4_upper - step_4_lower)/2) + step_4_lower,
                                  substr(x, 5,5) == 'B' ~ step_4_upper),
         
         step_6_lower = case_when(substr(x, 6,6) == 'F' ~ step_5_lower,
                                  substr(x, 6,6) == 'B' ~ round((step_5_upper - step_5_lower)/2) + step_5_lower),
         step_6_upper = case_when(substr(x, 6,6) == 'F' ~ floor((step_5_upper - step_5_lower)/2) + step_5_lower,
                                  substr(x, 6,6) == 'B' ~ step_5_upper),
         
         step_7 = case_when(substr(x, 7,7) == 'F' ~ step_6_lower,
                                  substr(x, 7,7) == 'B' ~ step_6_upper))


second <- inputs %>%
  mutate(start_side_lower = 0,
         start_side_upper = 7,
         step_1_side_lower = case_when(substr(x, 8,8) == 'L' ~ start_side_lower,
                                  substr(x, 8,8) == 'R' ~ round((start_side_upper - start_side_lower)/2) + start_side_lower),
         step_1_side_upper = case_when(substr(x, 8,8) == 'L' ~ floor((start_side_upper - start_side_lower)/2) + start_side_lower,
                                  substr(x, 8,8) == 'R' ~ start_side_upper),
         
         step_2_side_lower = case_when(substr(x, 9,9) == 'L' ~ step_1_side_lower,
                                  substr(x, 9,9) == 'R' ~ round((step_1_side_upper - step_1_side_lower)/2) + step_1_side_lower),
         step_2_side_upper = case_when(substr(x, 9,9) == 'L' ~ floor((step_1_side_upper - step_1_side_lower)/2) + step_1_side_lower,
                                  substr(x, 9,9) == 'R' ~ step_1_side_upper),
         
         step_side_3 = case_when(substr(x, 10,10) == 'L' ~ step_2_side_lower,
                            substr(x, 10,10) == 'R' ~ step_2_side_upper))

first_answer <- merge(first, second, by = 'x') %>%
  mutate(total = step_7 * 8 + step_side_3)

max(first_answer$total)



# part 2

first_answer %>%
  arrange(total) %>%
  mutate(same = lag(total,1) + 1) %>%
  filter(total != same)
