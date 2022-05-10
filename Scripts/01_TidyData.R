# Join legacy (1956-2004) and current catch (1991-2020) datasets with correction factors where needed

library(tidyverse)
library(ggplot2)

#setwd("C:/Users/bridget.ferriss/work/Project/GOA Catch and Climate 2022/DataJoin")

dataFullLeg=read.csv("../Data/legacy.goa.catch.csv")
dataFullCur=read.csv("../Data/current.goa.catch.csv")
dataFullSalCur<-read.csv("../Data/ADFG_salmon_1985_2020.csv")


head(dataFullLeg)
head(dataFullCur)
head(dataFullSalCur)

### Tidy datasets
#tidy salmon data to join to 'current catch data' (lbs - convert to mt) by species, region, year; and remove 2021 prelim data
dataSalCur<- dataFullSalCur %>%
  filter(Region_Desc=="Southeastern Region"| Region_Desc=="Central Region" | Region_Desc=="Westward Region") %>%
  filter(Management_Area!="M" | Management_Area!="T" | Management_Area!="F") %>%  #T=Bristol Bay, M = both sides AK peninsula, F =Atka-Amlia in Westward REgion
  mutate(catch_mt=Landed_Weight_lbs*0.000453592) %>% #convert catch_lbs to catch_mt
  mutate(dataset="current") %>% #label dataset as 'current'catch
  mutate(color="red") %>%
  select(year, catch_mt, Species_Name, dataset, year) %>%
  group_by(year, Species_Name) %>%
  summarise(catch_mt_sum=sum(catch_mt)) %>%
  spread(Species_Name,catch_mt_sum)   %>%
  rename(Chinook_mt=chinook_salmon, Chum_mt=chum_salmon, Coho_mt=coho_salmon, Pink_mt=pink_salmon, Sockeye_mt=sockeye_salmon) 

 
dataSalCur=dataSalCur %>%
mutate (dataset="current")%>%
  mutate (color="red")
 head(dataSalCur)

#tidy 'current' catch data and join catch salmon data 

dataCur <- dataFullCur%>%
  select(year:TannerCrab_mt)  %>% #select certain columns in 'dat' dataset
  filter(year %in% 1992:2020) %>% #remove 1991 - some catch data much lower
  mutate(dataset="current") %>% #label dataset as 'current'catch
  mutate(color="red") 
  
head(dataCur)

#tidy 'legacy' catch data (rename to match species column headers, remove or summarize columns that don't match 'current'catch dataset)
head(dataFullLeg)
dataLeg<- dataFullLeg %>%
  mutate(AllRockfish=slope.rockfish+pelagic.shelf.rockfish+demersal.shelf.rockfish+thornyheads+rockfish) %>%
  mutate (AllFlatfish=other.flatfish +arrowtooth.flounder) %>%
  select(-c(slope.rockfish,pelagic.shelf.rockfish,demersal.shelf.rockfish,thornyheads,rockfish)) %>% #remove columns that were summarized above
  select(-c(other.flatfish, arrowtooth.flounder)) %>%
  select(-c(atka.mackerel,pacific.herring.se,pacific.halibut.se, other.species, dungeness))%>% #not in current catch dataset
  rename(Wpollock_mt=walleye.pollock, Flatfish_mt=AllFlatfish, Pcod_mt=pacific.cod, Rockfish_mt=AllRockfish, Sablefish_mt=sablefish, Phalibut_mt=pacific.halibut, Pherring_mt=pacific.herring, KingCrab_mt=king.crab,Shrimp_mt=shrimp, TannerCrab_mt=tanner.crab)  %>% 
  rename (Chinook_mt=chinook, Chum_mt=chum, Pink_mt=pink, Sockeye_mt=sockeye, Coho_mt=coho) %>%
   mutate(dataset="legacy") %>% #label dataset as 'current'catch
  mutate(color="blue")

head(dataLeg)

### Get correction factors for joining legacy and current catch data; calculated in 'CatchDataJoin.r'
# No differences in slope & intercept: Flatfish, cod, pollock, herring
# Differences in intercept only: rockfish, sablefish, halibut, Tanner crab
# Differences in intercept and slope: King crab, Shrimp



