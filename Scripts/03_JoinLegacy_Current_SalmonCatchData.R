##Join Legacy, Current, and Salmon datasets
#Step1 - adjust legacy catch data where appropriate to align with current dataset using overlapping period (1992-2004)) (02_CalcLegacyCorrectionFactor.R)
#step2 - remove overlpaping years and join all 3 datasets
#STEP 3 - final output joint dataset is "DataAll"

#source(./Scripts/02_CalcLegacyCorrectionFactor.R") # (this caluclates correction factor (if there is one) between legacy and current datasets 

#table of difference in legacy and current data intercepts by species group calculated in 02_CalcLegacyCorrectionFactor.R
intTable

#these are from 01_TidyData.R, sourced in 02_CalcLegacyCorrectionFactor.R
head(dataCur) #1991-2020
head(dataLeg) # 1956-2004 #includes salmon
head(dataSalCur) #only salmon 1985-2020

#######
#STEP 1: adjust legacy data to align with current data based on CatchDataJoin2 (adjust intercet)
#no adjusments needed
#herring & shrimp have diff interc and slopes so can't join. none other have diff intercepts
#king crab has no legacy (all zeros) so can't join
#######


#########
#STEP 2: join 3 datasets
##########
# current dataset use 1992-2020 (remove 1991)
#legacy dataset use 1956-1991 (remove 1992-2004 -overlaps with current); use values adjusted for difference in intercept (calculated CatchDataJoin2.R)
#salmon -use legacy salmon through 1985 then current salmon 1985-2020)

#CURRENT dataset
dataCur2= dataCur%>%
filter (year!=1991)

#LEGACY with no salmon and remove overlapping years with current data
dataLeg2=dataLeg %>%
   select(-c(Chinook_mt,Pink_mt,Sockeye_mt,Chum_mt,Coho_mt)) %>%
   filter(year<1992) #use 'current' data from 1992 on

#SALMON
#salmon 1) separate salmon legacy from full legacy
dataSalLeg=dataLeg %>%
  select(year,Chinook_mt,Pink_mt,Sockeye_mt,Chum_mt,Coho_mt, dataset, color)%>%
  filter(year<1985)

#salmon 2) joint legacy salmon: 1956-1984 & current salmon 1985-2020
dataSalAll= dataSalLeg %>%
  full_join(dataSalCur)

#JOIN ALL legacy and current and salmon
DatAll= dataLeg2  %>%
  full_join(dataCur) %>%
  full_join(dataSalAll, by="year")

DatAll