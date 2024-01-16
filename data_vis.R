library(tidyverse)
library(ggplot2)
library(openintro)
library(dplyr)
ggplot(data = yrbss_samp) +
  geom_point(
    aes(x = height,y = weight, shape=gender),
    aplha=0.2
  )
ggplot(data = yrbss_samp) +
  geom_point(
    aes(x = height,y = weight, shape=gender),
    alpha=0.2
  )


yrbss_samp |>
  group_by(gender) |>
  summarise(
    mean_h = mean(height)
  )|>
  ggplot()+
  geom_col(
    aes(gender, mean_h)
  )

yrbss_samp |>
  group_by(gender) |>
  summarise(
    mean_days = mean(strength_training_7d)
  )|>
  ggplot()+
  geom_col(
    aes(gender,mean_days)
  )
view(yrbss_samp)
yrbss_samp |>
  filter(gender == "male") |>
  group_by(hispanic) |>
  summarise(
    mean_h = mean(height)
  )|>
  ggplot()+
  geom_col(
    aes(hispanic,mean_h)
  )


yrbss_samp |>
  mutate(
    stv_cat = case_when(
      strength_training_7d == 0 ~ "no train",
      strength_training_7d >=1 & strength_training_7d <=3 ~ "low train",
      strength_training_7d >3 & strength_training_7d <=5 ~ "med train",
      strength_training_7d >5 ~ "high train")
  )|>
  group_by(stv_cat) |>
  summarise(mean_w=mean(weight))|>
  ggplot()+
  geom_col( width=0.65,
    aes(x=reorder(stv_cat,mean_w),y=mean_w )
      )+
  coord_flip()
      
    