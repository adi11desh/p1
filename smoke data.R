library(dplyr)
library(tidyverse)
library(openintro)
census_data |>
  select(highest_qualification, amt_weekdays) |>
  rename(hq = highest_qualification, smokdays = amt_weekdays) |>
  arrange(census_data, hq, smokdays )

census_data |>
  group_by(highest_qualification) |>
  summarise(total_cig = sum(amt_weekdays + amt_weekends, na.rm =T)
  )

census_data |>
  filter(smoke == "Yes", age < 30) 
census_data |>
  filter(age<30) |>
  arrange(-age)

census_data |>
  select(gross_income,amt_weekdays,amt_weekends) |>
  group_by(gross_income) |>
  summarise(tot = sum(amt_weekends + amt_weekdays, na.rm = T))
census_data |>
  group_by(gender) |>
  summarise(avg = mean(age))

View(census_data)
library(dplyr)
library(tidyverse)
library(openintro)

census_data |>
  mutate(
    age = case_when(
      age >= 15 & age<=25 ~ "small",
      age > 25 & age<=40 ~ "med",
      age > 40 & age<=59 ~ "larg",
      age > 59 ~ "huge")
  )
