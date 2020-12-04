library(stringr)

inputs <- readLines('2020/day3/day3.txt')

#part 1
length_of_string <- str_length(inputs[1])
number_of_strings <- length(inputs)


repeat_string <- c()
trees_hit <- c()




for (i in 1:length(repeat_string)) {
  string_location <- ((i-1)*3)+1
  trees_hit[i] <- str_sub(repeat_string[i], string_location,string_location)
}
sum(str_count(trees_hit, "#"))

#part 2

input_df <- data.frame(inputs)
input_df$row_num <- as.numeric(row.names(input_df))

odd_input <- input_df[input_df$row_num %% 2 == 1,]

trees_hit1 <- c()
trees_hit3 <- c()
trees_hit5 <- c()
trees_hit7 <- c()
trees_hitodd <- c()

for (i in 1:length(repeat_string)) {
  string_location1 <- ((i-1)*1)+1
  trees_hit1[i] <- str_sub(repeat_string[i], string_location1,string_location1)
  
  string_location3 <- ((i-1)*3)+1
  trees_hit3[i] <- str_sub(repeat_string[i], string_location3,string_location3)
  
  string_location5 <- ((i-1)*5)+1
  trees_hit5[i] <- str_sub(repeat_string[i], string_location5,string_location5)
  
  string_location7 <- ((i-1)*7)+1
  trees_hit7[i] <- str_sub(repeat_string[i], string_location7,string_location7)
}

number_of_strings_odd <- length(odd_input$inputs)

repeat_stringodd <- c()
for (i in 1:length(odd_input$inputs)) {
  repeat_stringodd[i] <- paste(replicate(number_of_strings, odd_input$inputs[i]), collapse = "")
}

for (i in 1:length(repeat_stringodd)) {
  string_locationodd <- ((i-1)*1)+1
  trees_hitodd[i] <- str_sub(repeat_stringodd[i], string_locationodd,string_locationodd)
}

sum(str_count(trees_hit1, "#"))
sum(str_count(trees_hit3, "#"))
sum(str_count(trees_hit5, "#"))
sum(str_count(trees_hit7, "#"))
sum(str_count(trees_hitodd, "#"))
#94 * 214 * 99 * 91 * 46
