library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggsci)

senspr <- read_excel("sen.xlsx", sheet = 2)

senspr$Code <- as.factor(senspr$Code)
senspr %>% 
  select(2:6) %>%  
  pivot_longer(cols = 3:5, names_to = "range", values_to = "value") %>% 
  ggplot(aes(Code, value, fill = Par))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~Par, scales = "free_y")+
  scale_color_npg()+
  theme_bw()


# LBB
senlbb <- read_excel("sen.xlsx", sheet = 3)

senlbb$Code <- as.factor(senlbb$Code)
senlbb %>% 
  select(2:6) %>%  
  pivot_longer(cols = 3:5, names_to = "range", values_to = "value") %>% 
  ggplot(aes(Code, value, fill = Par))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~Par, scales = "free_y")+
  scale_color_npg()+
  theme_bw()
