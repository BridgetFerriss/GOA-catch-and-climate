#### compare "legacy" Litzow et al catch (1956-2005) data with "Current" AKFIN 1991-2020 catch data (GOA catch) 
# by analyzing log transformed catch_mt by species in overlapping years (1991-2005) to determine if differences
# in slope and/or intercept


library(ggplot2)

#setwd("C:/Users/bridget.ferriss/work/Project/GOA Catch and Climate 2022/DataJoin")

dataFullLeg=read.csv("LitzowDataClean.csv")
dataFullCur=read.csv("AKFINDataClean.csv")

head(dataFullLeg)
head(dataFullCur)

###########
### Organize data
#1. reduce columns to year, species, catch_mt, data source)
#2. plot full time series of all species
#3. Reduce time series to overlapping years (1991-2005) for analyses 
###########

### reduce datasets to main catch timeseries (mt) (remove other columns of data)
#assign color blue to 'Legacy' data and color red to 'Current' data

dataLeg=dataFullLeg[,1:14]
dataLeg$Col="blue"
dataLeg$Source="legacy"
head(dataLeg)

dataCur=dataFullCur[,1:11]
dataCur$Col="red"
dataCur$Source="current"
head(dataCur)

### create new combined dataset of catch of all species individual and together; full time series and plot
 
PLcomb=rbind(dataCur[,c(1,4,12,13)],dataLeg[,c(1,2,15,16)]) #pollock
PLcomb$Species="WPollock"
colnames(PLcomb)[2]="Catch_mt"
CDcomb=rbind(dataCur[,c(1,3,12,13)],dataLeg[,c(1,3,15,16)]) #cod
CDcomb$Species="PCod"
colnames(CDcomb)[2]="Catch_mt"
FFcomb=rbind(dataCur[,c(1,2,12,13)],dataLeg[,c(1,4,15,16)])#flatfish
FFcomb$Species="Flatfish"
colnames(FFcomb)[2]="Catch_mt"
SBcomb=rbind(dataCur[,c(1,6,12,13)],dataLeg[,c(1,5,15,16)])#sablefish
SBcomb$Species="Sablefish"
colnames(SBcomb)[2]="Catch_mt"
RKcomb=rbind(dataCur[,c(1,5,12,13)],dataLeg[,c(1,6,15,16)])#rockfish
RKcomb$Species="Rockfish"
colnames(RKcomb)[2]="Catch_mt"
HRcomb=rbind(dataCur[,c(1,8,12,13)],dataLeg[,c(1,7,15,16)])#herring
HRcomb$Species="PHerring"
colnames(HRcomb)[2]="Catch_mt"
HLcomb=rbind(dataCur[,c(1,7,12,13)],dataLeg[,c(1,8,15,16)])#halibut
HLcomb$Species="PHalibut"
colnames(HLcomb)[2]="Catch_mt"
KCcomb=rbind(dataCur[,c(1,9,12,13)],dataLeg[,c(1,9,15,16)])#kingcrab  
KCcomb$Species="KingCrab"
colnames(KCcomb)[2]="Catch_mt"
TCcomb=rbind(dataCur[,c(1,11,12,13)],dataLeg[,c(1,10,15,16)])#tannercrab
TCcomb$Species="TannerCrab"
colnames(TCcomb)[2]="Catch_mt"
SHcomb=rbind(dataCur[,c(1,10,12,13)],dataLeg[,c(1,11,15,16)])#shrimp
SHcomb$Species="Shrimp"
colnames(SHcomb)[2]="Catch_mt"

datComb=rbind(PLcomb,CDcomb,FFcomb,SBcomb,RKcomb,HRcomb,HLcomb,KCcomb,TCcomb,SHcomb)

### plot all species together full timeseries
p_regr=ggplot(data = datComb, group=Source)+
geom_point(aes(x = Year, y = Catch_mt, color = Source), size=1, show.legend = TRUE) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

p_regr+facet_grid(vars(Species),scales = "free_y") 


### Reduce full datasets to only overlapping years of catch data 1991-2005 for analyses
CurMaxRow=which(dataCur$Year==2005)
LegMinRow=which(dataLeg$Year==1991)
LegMaxRow=length(dataLeg[,1])

CurOvlap=dataCur[c(1:CurMaxRow),]
LegOvlap=dataLeg[c(LegMinRow:LegMaxRow),]

CurOvlap
LegOvlap

PLov=rbind(CurOvlap[,c(1,4,12,13)],LegOvlap[,c(1,2,15,16)]) #pollock
PLov$Species="WPollock"
colnames(PLov)[2]="Catch_mt"
CDov=rbind(CurOvlap[,c(1,3,12,13)],LegOvlap[,c(1,3,15,16)]) #cod
CDov$Species="PCod"
colnames(CDov)[2]="Catch_mt"
FFov=rbind(CurOvlap[,c(1,2,12,13)],LegOvlap[,c(1,4,15,16)])#flatfish
FFov$Species="Flatfish"
colnames(FFov)[2]="Catch_mt"
SBov=rbind(CurOvlap[,c(1,6,12,13)],LegOvlap[,c(1,5,15,16)])#sablefish
SBov$Species="Sablefish"
colnames(SBov)[2]="Catch_mt"
RKov=rbind(CurOvlap[,c(1,5,12,13)],LegOvlap[,c(1,6,15,16)])#rockfish
RKov$Species="Rockfish"
colnames(RKov)[2]="Catch_mt"
HRov=rbind(CurOvlap[,c(1,8,12,13)],LegOvlap[,c(1,7,15,16)])#herring
HRov$Species="PHerring"
colnames(HRov)[2]="Catch_mt"
HLov=rbind(CurOvlap[,c(1,7,12,13)],LegOvlap[,c(1,8,15,16)])#halibut
HLov$Species="PHalibut"
colnames(HLov)[2]="Catch_mt"
KCov=rbind(CurOvlap[,c(1,9,12,13)],LegOvlap[,c(1,9,15,16)])#kingcrab
KCov$Species="KingCrab"
colnames(KCov)[2]="Catch_mt"
TCov=rbind(CurOvlap[,c(1,11,12,13)],LegOvlap[,c(1,10,15,16)])#tannercrab
TCov$Species="TannerCrab"
colnames(TCov)[2]="Catch_mt"
SHov=rbind(CurOvlap[,c(1,10,12,13)],LegOvlap[,c(1,11,15,16)])#shrimp
SHov$Species="Shrimp"
colnames(SHov)[2]="Catch_mt"

datOv=rbind(PLov,CDov,FFov,SBov,RKov,HRov,HLov,KCov,TCov,SHov)
datOv$logCatch_mt=log(as.numeric(datOv$Catch_mt)) #log transform to be able to regress


### Plot all species together OVERLAPPING timeseries
p_regr=ggplot(data = datOv, group=)+
  geom_point(aes(x = Year, y = logCatch_mt, color = Source), size=1, show.legend = TRUE) +
  #scale_y_continuous(limits = c(0, 1000))
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

p_regr+facet_grid(vars(Species),scales = "free_y") 

########################
### Test an dplot potential differences between 2 datasets in overlapping time period (1991-2005); test different slope, intercept by species
#removed years when didn't have data point for each "current" and "legacy" dataset
#######################

### pollock - no difference if remove 1991 (big difference in that year)
PLov
PLov2=PLov[-15,] #remove any rows that have NA (missing values in legacy or current dataset)
PLov3=PLov2[-which(PLov2$Year==1991),] #remove 1991 only year where values very differet (exploratory), 2005 - no data for one set
PLov4=PLov3[-which(PLov3$Year==2005),] #remove 2015 only one year has values

#plot
ggplot(data=PLov4, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
PLlm1= lm(log(Catch_mt) ~ Year, data = PLov4 )
PLlm2= lm(log(Catch_mt) ~ Year + Source, data = PLov4 )
PLlm3= lm(log(Catch_mt) ~ Year*Source, data = PLov4 )
AIC(PLlm1,PLlm2,PLlm3) 

PLlm4=aov(log(Catch_mt) ~ Year*Source, data = PLov3)
summary(PLlm4) # No difference slope (Year*Source) or intercept (Source)

#Pacific cod no difference if remove 1991, 2005 (no difference slope or intercept)
CDov
CDov2=CDov[-which(CDov$Year==2005),] #remove 2015 only one year has values
CDov3=CDov2[-which(CDov2$Year==1991),] #remove 1991 only year where values very differet (exploratory), 

#plot
ggplot(data=CDov3, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
CDlm1= lm(log(Catch_mt) ~ Year, data = CDov3 )
CDlm2= lm(log(Catch_mt) ~ Year + Source, data = CDov3 )
CDlm3= lm(log(Catch_mt) ~ Year*Source, data = CDov3 )
AIC(CDlm1,CDlm2,CDlm3) #CDlm1

CDlm4=aov(log(Catch_mt) ~ Year*Source, data = CDov3)
summary(CDlm4) # No difference slope (Year*Source) or intercept (Source)


#Flatfish no difference if remove 1991
FFov
FFov2=FFov[-which(FFov$Year==2005),] #remove 2015 only one year has values
FFov3=FFov2[-which(FFov2$Year==1991),] #remove 1991 only year where values very differet (exploratory), 

#plot
ggplot(data=FFov3, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
FFlm1= lm(log(Catch_mt) ~ Year, data = FFov3 )
FFlm2= lm(log(Catch_mt) ~ Year + Source, data = FFov3 )
FFlm3= lm(log(Catch_mt) ~ Year*Source, data = FFov3 )
AIC(FFlm1,FFlm2,FFlm3) 

FFlm4=aov(log(Catch_mt) ~ Year*Source, data = FFov3)
summary(FFlm4) #No different intercepts no different slopes


### Sablefish - remove 1991, diff intercepts sim slopes
SBov
SBov2=SBov[-which(SBov$Year==2005),] #remove 2015 only one year has values
SBov3=SBov2[-which(SBov2$Year==1991),] #remove 1991 only year where values very differet (exploratory), 

#plot
ggplot(data=SBov3, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")


#analyze for differences in slope and intercept by AIC and aov
SBlm1= lm(log(Catch_mt) ~ Year, data = SBov3 )
SBlm2= lm(log(Catch_mt) ~ Year + Source, data = SBov3 )
SBlm3= lm(log(Catch_mt) ~ Year*Source, data = SBov3 )
AIC(SBlm1,SBlm2,SBlm3)

SBlm4=aov(log(Catch_mt) ~ Year*Source, data = SBov3)
summary(SBlm4) #different intercepts not slopes


#Rockfish - diff intercepts but not slopes
RKov
RKov2=RKov[-which(RKov$Year==2005),] #remove 2015 only one year has values

ggplot(data=RKov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
RKlm1= lm(log(Catch_mt) ~ Year, data = RKov2)
RKlm2= lm(log(Catch_mt) ~ Year + Source, data = RKov2)
RKlm3= lm(log(Catch_mt) ~ Year*Source, data = RKov2)
AIC(RKlm1,RKlm2,RKlm3) 

RKlm4=aov(log(Catch_mt) ~ Year*Source, data = RKov2)
summary(RKlm4) #different intercepts not slopes

#Pacific Halibut  #different intercept (source) not different slope (year:source); remove 1991 and 2005
HLov
HLov2=HLov[-which(HLov$Year==2005),] #remove 2015 only one year has values

#plot
ggplot(data=HLov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
HLlm1= lm(log(Catch_mt) ~ Year, data = HLov2)
HLlm2= lm(log(Catch_mt) ~ Year + Source, data = HLov2)
HLlm3= lm(log(Catch_mt) ~ Year*Source, data = HLov2)
AIC(HLlm1,HLlm2,HLlm3) 

HLlm4=aov(log(Catch_mt) ~ Year*Source, data = HLov2)
summary(HLlm4) #different intercept (source) not different slope (year:source)

### Pacific Herring  #no different intercept (source) not different slope (year:source); keep 1991 ; remove 1994,1995,1996,1997 (missing from Legacy)
HRov
HRyearsRM=which(HRov$Year==1994 | HRov$Year==1995 | HRov$Year==1996 | HRov$Year==1997)
HRov2=HRov[-HRyearsRM,] #no values in Legacy dataset

#plot
ggplot(data=HRov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
HRlm1= lm(log(Catch_mt) ~ Year, data = HRov2)
HRlm2= lm(log(Catch_mt) ~ Year + Source, data = HRov2)
HRlm3= lm(log(Catch_mt) ~ Year*Source, data = HRov2)
AIC(HRlm1,HRlm2,HRlm3) 

HRlm4=aov(log(Catch_mt) ~ Year*Source, data = HRov2)
summary(HRlm4) #no different intercept (source) not different slope (year:source)



### King Crab  #different intercept (source)  different slope (year:source); Legacy data all zeros
KCov
KCov2=KCov[-which(KCov$Year==2005),] #no values in Legacy dataset

#plot
ggplot(data=KCov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
KClm1= lm(log(Catch_mt+1) ~ Year, data = KCov2)
KClm2= lm(log(Catch_mt+1) ~ Year + Source, data = KCov2)
KClm3= lm(log(Catch_mt+1) ~ Year*Source, data = KCov2)
AIC(KClm1,KClm2,KClm3) 


KClm4=aov(log(Catch_mt+1) ~ Year*Source, data = KCov2)
summary(KClm4) #no different intercept (source) not different slope (year:source)

###Tanner Crab  # Legacy data  zeros 1995-2000; #different intercept, not different slope
TCov
TCov2=TCov[-which(TCov$Year==2005),] #no values in Legacy dataset

#plot
ggplot(data=TCov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
TClm1= lm(log(Catch_mt+1) ~ Year, data = TCov2)
TClm2= lm(log(Catch_mt+1) ~ Year + Source, data = TCov2)
TClm3= lm(log(Catch_mt+1) ~ Year*Source, data = TCov2)
AIC(TClm1,TClm2,TClm3) 

TClm4=aov(log(Catch_mt+1) ~ Year*Source, data = TCov2)
summary(TClm4) #no different intercept (source) not different slope (year:source)

#### Shrimp #different slope and intercept
SHov
SHov2=SHov[-which(SHov$Year==2005),] #no values in Legacy dataset

#plot
ggplot(data=SHov2, aes(x=Year, y=log(Catch_mt), color=Source), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by AIC and aov
SHlm1= lm(log(Catch_mt+1) ~ Year, data = SHov2)
SHlm2= lm(log(Catch_mt+1) ~ Year + Source, data = SHov2)
SHlm3= lm(log(Catch_mt+1) ~ Year*Source, data = SHov2)
AIC(SHlm1,SHlm2,SHlm3) 

SHlm4=aov(log(Catch_mt+1) ~ Year*Source, data = SHov2)
summary(SHlm4) #no different intercept (source) not different slope (year:source)


