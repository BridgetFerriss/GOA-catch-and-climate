#create heatmap of catch over time by species

library(tidyverse)

dat <- read.csv("./Data/current.goa.catch.csv")
salmon.dat<-read.csv("./Data/ADFG_salmon_1985_2020.csv")

head(dat)
head(salmon.dat)

#tidy salmon data to summarize catch (lbs) by species, region, year; and remove 2021 prelim data
salmon.dat<- salmon.dat %>%
  filter(Region_Desc=="Southeastern Region"| Region_Desc=="Central Region" | Region_Desc=="Westward Region") %>%
  filter(Management_Area!="M" | Management_Area!="T" | Management_Area!="F") %>%  #T=Bristol Bay, M = both sides AK peninsula, F =Atka-Amlia in Westward REgion
  mutate(catch_mt=Landed_Weight_lbs*0.000453592) %>% #convert catch_lbs to catch_mt
  select(year, catch_mt, Species_Name) %>%
  group_by(year, Species_Name) %>%
  summarise(catch_mt_sum=sum(catch_mt)) %>%
  spread(Species_Name,catch_mt_sum)  

#join salmon data to other species data

salmon.dat2<-salmon.dat %>%
  filter(year %in% 1992:2020)    #years will match other catch data

dat2 <- dat %>%
  select(year:TannerCrab_mt)  %>% #select certain columns
  filter(year %in% 1992:2020) 

dat2 <- dat %>%
  select(year:TannerCrab_mt)  %>% #select certain columns
  filter(year %in% 1992:2020) %>% #remove 1991 - some catch data much lower
  full_join(salmon.dat2, by="year") #




#convert to long format (1 obs per row) for heatmap
datplot <- dat2 %>%
  pivot_longer(cols = -year, names_to = "species", values_to = "catch") 


library(ggplot2)
library(viridis) #foro color palette

ggplot(datplot, aes(x = year, y = species, fill = catch)) +
  geom_tile()+
  scale_fill_viridis(discrete=FALSE) 

ggsave("./Figs/heatmap_current_catch.png", width = 5.5, height = 3, units = 'in')

