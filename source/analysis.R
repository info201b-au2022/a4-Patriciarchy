library(tidyverse)
library(ggplot2)
library(dplyr)

# The functions might be useful for A4
source("../source/a4-helpers.R")


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration <- get_data()
View(incarceration)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Examining black population in jail
#----------------------------------------------------------------------------#
#In 2018, what is the state average population of black prisoners and white prisoners 
average_black_in_each_state_2018 <- incarceration %>% 
  filter(year == 2018) %>% select(year, state, county_name, black_jail_pop) %>% 
  group_by(state) %>% summarize(state_black_pop = sum(black_jail_pop, na.rm = TRUE))
average_black_in_each_state_2018

state_black_avg_2018 <- mean(average_in_each_state_2018$state_black_pop)
state_black_avg_2018

average_white_in_each_state_2018 <- incarceration %>% 
  filter(year == 2018) %>% select(year, state, county_name, white_jail_pop) %>% 
  group_by(state) %>% summarize(state_white_pop = sum(white_jail_pop, na.rm = TRUE))

average_white_in_each_state_2018

state_avg_white_2018 <- mean(average_white_in_each_state_2018$state_white_pop)
state_avg_white_2018

#In 2018, which state had the highest number of black and white prisoners from 15-64

state_most_black_prisoners <- average_black_in_each_state_2018 %>% filter(state_black_pop == max(state_black_pop)) %>% pull(state)
state_most_black_prisoners_count <- average_black_in_each_state_2018 %>% filter(state_black_pop == max(state_black_pop)) %>% pull(state_black_pop)

state_most_white_prisoners <- average_white_in_each_state_2018 %>% filter(state_white_pop == max(state_white_pop)) %>% pull(state)
state_most_white_prisoners_count <- average_white_in_each_state_2018 %>% filter(state_white_pop == max(state_white_pop)) %>% pull(state_white_pop)

state_most_black_prisoners
state_most_black_prisoners_count
state_most_white_prisoners
state_most_white_prisoners_count

#How much has the average number of black prisoners in each state change from 2008 to 2018

average_black_in_each_state_2008 <- incarceration %>% 
  filter(year == 2008) %>% select(year, state, county_name, black_jail_pop) %>% 
  group_by(state) %>% summarize(state_black_pop = sum(black_jail_pop, na.rm = TRUE))

state_black_avg_2008 <- mean(average_black_in_each_state_2008$state_black_pop)
state_black_avg_2018
state_black_avg_2008

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function returns a data frame of each year's total jail population 
get_year_jail_pop <- function() {
  result <- incarceration %>% select(year, total_jail_pop)
  return(result)   
}

# This function graphs the total jail population data frame 
plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970-2018)",
    caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).  This chart shows the 
    overall trend of jail population in the US since the year 1970. There has been a steep increase in jail
    population starting from the year 1980, and the number peaked around the year 2008. We see a slight decrease since,
    however, the overall incarcerated population is concerning."
  )
  
  chart <- ggplot(data) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop),
    ) + labels + scale_y_continuous(labels = scales::comma)
  return(chart)
} 

plot_jail_pop_for_us()


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

#This function creates a data frame that shows the selected states total jail population each year 
get_jail_pop_by_states <- function(states) {
  data <- incarceration %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% states) %>% group_by(year, state) %>%
    summarize(state_total = sum(total_jail_pop, na.rm = TRUE))
  return(data)
}

#This function graphs the data frame and returns a line graph 
plot_jail_pop_by_states <- function(states) {
  data <- get_jail_pop_by_states(states)
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Jail population in selected states from 1970 to 2018",
    caption = "Figure 2. This line graph indicates the growth trend of jail population 
    in selected states from 1970 to 2018."
  )
  
  chart <- ggplot(data) +
    geom_line(mapping = aes(x = year, y = state_total, color = state)) + labels
  return(chart)
}

plot_jail_pop_by_states(c("WA", "OR", "CA"))


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_comparison_pop <- function() {
  data <- incarceration %>% select(year, white_jail_pop, black_jail_pop) %>% 
                            group_by(year) %>% 
    summarise(white_sum = sum(white_jail_pop, na.rm = TRUE),
              black_sum = sum(black_jail_pop, na.rm = TRUE)) %>% drop_na()
  return(data)
}

View(get_comparison_pop())

plot_minority_pop <- function() {
  data <- get_comparison_pop()
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Jail population of black and white people from 1970 to 2018",
    caption = "Figure 3. This chart is a comparison visualization for the jail population of black and white people. 
    The blue line indicates the incarceration trend for black people. The red line
    indicates the incarceration trend for white people."
  )
  
  chart <- ggplot(data) +
    geom_line(mapping = aes(x = year, y = black_sum), color = 'blue') +
    geom_line(mapping = aes(x = year, y = white_sum), color = "red") + labels
  return(chart)
}

plot_minority_pop()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


