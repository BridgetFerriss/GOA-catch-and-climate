### Join legacy (1956-2004) and current catch (1991-2020) datasets with correction factors where needed

library(ggplot2)

### read in tidied catch datasets for 'legacy', 'current', 'salmon'
#source("../Scripts/01_TidyData.R")

head(dataCur)
head(dataLeg)
head(dataSalCur)

#Reduce datasets to overlapping time periods
#Current data
dataCurov= dataCur  %>%
  filter(year %in% 1992:2004)

#legacy data
dataLegov= dataLeg  %>%
  filter(year %in% 1992:2004)

#Current salmon data
dataSalCurOv=dataSalCur %>%
  filter(year %in% 1985:2004)

#legacy salmon data
dataSalLegOv= dataLeg  %>%
  filter(year %in% 1985:2004) %>%
  select(year, Chinook_mt, Chum_mt, Sockeye_mt, Pink_mt, Coho_mt, dataset, color)           #, Chum_mt, Sockeye_mt, Pink_mt, Coho_mt)) %>%

#combine Current, legacy (no salmon); and combine salmon current&legacy (different time period of overlap
dataOv=bind_rows(dataCurov,dataLegov)
dataOvSal=bind_rows(dataSalCurOv,dataSalLegOv)

########################
### Test an dplot potential differences between 2 datasets in overlapping time period (1991-2005); test different slope, intercept by species
#removed years when didn't have data point for each "current" and "legacy" dataset
#######################

#create empty table to populate with intercept correction factors
sp=colnames(dataOv[,c(2:11,14:18)]) #remove year and dataset and color columns
intTable=matrix(NA, nrow=length(sp), ncol=3)
intTable[,1]=sp
colnames(intTable)=c("Species", "Intercept", "Slope")

##species specific analyses to determin if intercept and slope are differnet between overlapping legacy and current datasets

#Walleye pollock
PLov= dataOv %>%
  select (year, dataset, Wpollock_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs
  
#plot
ggplot(data=PLov, aes(x=year, y=log(Wpollock_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#ANOVA for differences in slope & intercept
PLlm=aov(log(Wpollock_mt) ~ year*dataset, data = PLov)
summary.lm(PLlm) # No difference slope (Year*dataset) or intercept (dataset)

PL.int=summary.lm(PLlm)$coefficients[3,1]#estimate datasetlegacy
PL.int.pval=summary.lm(PLlm)$coefficients[3,4] #p value datasetlegacy
PL.sl=summary.lm(PLlm)$coefficients[4,1]#estimate year:datasetlegacy
PL.sl.pval=summary.lm(PLlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(PL.int.pval<0.05) {
  intTable[which(intTable[,1]=="Wpollock_mt"),2]= PL.int } else {
    intTable[which(intTable[,1]=="Wpollock_mt"),2]= 0
  }

if(PL.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Wpollock_mt"),3]= PL.sl} else {
    intTable[which(intTable[,1]=="Wpollock_mt"),3]= 0
  }

##### Pacific Cod
#Pacific cod no difference if remove 1991, 2005 (no difference slope or intercept)

CDov= dataOv %>%
  select (year, dataset, Pcod_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=CDov, aes(x=year, y=log( Pcod_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept ANOVA (aov)
CDlm=aov(log(Pcod_mt) ~ year*dataset, data = CDov)
summary.lm(CDlm) # No difference slope (Year*Source) or intercept (Source)

CD.int=summary.lm(CDlm)$coefficients[3,1]#estimate datasetlegacy
CD.int.pval=summary.lm(CDlm)$coefficients[3,4] #p value datasetlegacy
CD.sl=summary.lm(CDlm)$coefficients[4,1]#estimate year:datasetlegacy
CD.sl.pval=summary.lm(CDlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(CD.int.pval<0.05) {
  intTable[which(intTable[,1]=="Pcod_mt"),2]= CD.int } else {
    intTable[which(intTable[,1]=="Pcod_mt"),2]= 0
  }

if(CD.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Pcod_mt"),3]= CD.sl} else {
    intTable[which(intTable[,1]=="Pcod_mt"),3]= 0
  }

###### F;atfish
## Flatfish no difference if remove 1991

FFov= dataOv %>%
  select (year, dataset, Flatfish_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=FFov, aes(x=year, y=log(Flatfish_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for differences in slope and intercept by ANOVA (aov)
FFlm=aov(log(Flatfish_mt) ~ year*dataset, data = FFov)
summary.lm(FFlm) # No difference slope (Year*Source) or intercept (Source)

FF.int=summary.lm(FFlm)$coefficients[3,1]#estimate datasetlegacy
FF.int.pval=summary.lm(FFlm)$coefficients[3,4] #p value datasetlegacy
FF.sl=summary.lm(FFlm)$coefficients[4,1]#estimate year:datasetlegacy
FF.sl.pval=summary.lm(FFlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(FF.int.pval<0.05) {
  intTable[which(intTable[,1]=="Flatfish_mt"),2]= FF.int } else {
    intTable[which(intTable[,1]=="Flatfish_mt"),2]= 0
  }

if(FF.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Flatfish_mt"),3]= FF.sl} else {
    intTable[which(intTable[,1]=="Flatfish_mt"),3]= 0
  }

### Sablefish 
##- remove 1991, diff intercepts sim slopes

SBov= dataOv %>%
  select (year, dataset, Sablefish_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=SBov, aes(x=year, y=log(Sablefish_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
SBlm=aov(log(Sablefish_mt) ~ year*dataset, data = SBov)
summary.lm(SBlm) # No diSBerence slope (Year*dataset) or intercept (dataset)

SB.int=summary.lm(SBlm)$coefficients[3,1]#estimate datasetlegacy
SB.int.pval=summary.lm(SBlm)$coefficients[3,4] #p value datasetlegacy
SB.sl=summary.lm(SBlm)$coefficients[4,1]#estimate year:datasetlegacy
SB.sl.pval=summary.lm(SBlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(SB.int.pval<0.05) {
  intTable[which(intTable[,1]=="Sablefish_mt"),2]= SB.int } else {
    intTable[which(intTable[,1]=="Sablefish_mt"),2]= 0
  }

if(SB.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Sablefish_mt"),3]= SB.sl} else {
    intTable[which(intTable[,1]=="Sablefish_mt"),3]= 0
  }

###Rockfish 
#- diff intercepts but not slopes

RKov= dataOv %>%
  select (year, dataset, Rockfish_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=RKov, aes(x=year, y=log(Rockfish_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
RKlm=aov(log(Rockfish_mt) ~ year*dataset, data = RKov)
summary.lm(RKlm) # No difference slope (Year*Source) or intercept (Source)

RK.int=summary.lm(RKlm)$coefficients[3,1]#estimate datasetlegacy
RK.int.pval=summary.lm(RKlm)$coefficients[3,4] #p value datasetlegacy
RK.sl=summary.lm(RKlm)$coefficients[4,1]#estimate year:datasetlegacy
RK.sl.pval=summary.lm(RKlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(RK.int.pval<0.05) {
  intTable[which(intTable[,1]=="Rockfish_mt"),2]= RK.int } else {
    intTable[which(intTable[,1]=="Rockfish_mt"),2]= 0
  }

if(RK.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Rockfish_mt"),3]= RK.sl} else {
    intTable[which(intTable[,1]=="Rockfish_mt"),3]= 0
  }


#Pacific Halibut  #different intercept (source) not different slope (year:source); remove 1991 and 2005
HLov= dataOv %>%
  select (year, dataset, Phalibut_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=HLov, aes(x=year, y=log(Phalibut_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
HLlm=aov(log(Phalibut_mt) ~ year*dataset, data = HLov)
summary.lm(HLlm) # No difference slope (Year*dataset) or intercept (dataset)

HL.int=summary.lm(HLlm)$coefficients[3,1]#estimate datasetlegacy
HL.int.pval=summary.lm(HLlm)$coefficients[3,4] #p value datasetlegacy
HL.sl=summary.lm(HLlm)$coefficients[4,1]#estimate year:datasetlegacy
HL.sl.pval=summary.lm(HLlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(HL.int.pval<0.05) {
  intTable[which(intTable[,1]=="Phalibut_mt"),2]= HL.int } else {
    intTable[which(intTable[,1]=="Phalibut_mt"),2]= 0
  }

if(HL.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Phalibut_mt"),3]= HL.sl} else {
    intTable[which(intTable[,1]=="Phalibut_mt"),3]= 0
  }

  
### Pacific Herring  #no different intercept (source) not different slope (year:source); keep 1991 ; remove 1994,1995,1996,1997 (missing from Legacy)
HRov= dataOv %>%
  select (year, dataset, Pherring_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=HRov, aes(x=year, y=log(Pherring_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
HRlm=aov(log(Pherring_mt) ~ year*dataset, data = HRov)
summary.lm(HRlm) # No difference slope (Year*Source) or intercept (Source)

HR.int=summary.lm(HRlm)$coefficients[3,1]#estimate datasetlegacy
HR.int.pval=summary.lm(HRlm)$coefficients[3,4] #p value datasetlegacy
HR.sl=summary.lm(HRlm)$coefficients[4,1]#estimate year:datasetlegacy
HR.sl.pval=summary.lm(HRlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(HR.int.pval<0.05) {
  intTable[which(intTable[,1]=="Pherring_mt"),2]= HR.int } else {
    intTable[which(intTable[,1]=="Pherring_mt"),2]= 0
  }

if(HR.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Pherring_mt"),3]= HR.sl} else {
    intTable[which(intTable[,1]=="Pherring_mt"),3]= 0
  }

### King Crab  #different intercept (source)  different slope (year:source); Legacy data all zeros
KCov= dataOv %>%
  select (year, dataset, KingCrab_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=KCov, aes(x=year, y=log(KingCrab_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#no legacy data, only current data (1992-2020) - so no analyses of overlap
KC.int=NA
KC.sign=NA

intTable[which(intTable[,1]=="KingCrab_mt"),2]= NA #no legacy data so invalid intercept
intTable[which(intTable[,1]=="KingCrab_mt"),3]= NA #no legacy data so invalid slope

###Tanner Crab  # Legacy data  zeros 1995-2000; #different intercept, not different slope; legacy has lots zeros (1995-2000)
TCov= dataOv %>%
  select (year, dataset, TannerCrab_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.))# %>% #remove rows with any NAs
  #filter(TannerCrab_mt>0)

#plot
ggplot(data=TCov, aes(x=year, y=log(TannerCrab_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by AIC and aov
TClm1= lm(log(TannerCrab_mt) ~ year, data = TCov )
TClm2= lm(log(TannerCrab_mt) ~ year + dataset, data = TCov )
TClm3= lm(log(TannerCrab_mt) ~ year*dataset, data = TCov )
AIC(TClm1,TClm2,TClm3) 

TClm=aov(log(TannerCrab_mt) ~ year*dataset, data = TCov)
summary.lm(TClm) # No difference slope (Year*Source) or intercept (Source)

TC.int=summary.lm(TClm)$coefficients[3,1]#estimate datasetlegacy
TC.pval=summary.lm(TClm)$coefficients[3,4] #p value datasetlegacy
TC.int=NA
TC.sign=NA

intTable[which(intTable[,1]=="TannerCrab_mt"),2]= NA #zeros legacy data (1995-2000) so invalid intercept
intTable[which(intTable[,1]=="TannerCrab_mt"),3]= NA #zeros legacy data so invalid slope

#### Shrimp #different slope and intercept
SHov= dataOv %>%
  select (year, dataset, Shrimp_mt)%>%
  filter(year!=1991) %>%
  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=SHov, aes(x=year, y=log(Shrimp_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept ANOVA (aov)
SHlm=aov(log(Shrimp_mt) ~ year*dataset, data = SHov)
summary.lm(SHlm) # No difference slope (Year*Source) or intercept (Source)

SH.int=summary.lm(SHlm)$coefficients[3,1]#estimate datasetlegacy
SH.int.pval=summary.lm(SHlm)$coefficients[3,4] #p value datasetlegacy
SH.sl=summary.lm(SHlm)$coefficients[4,1]#estimate year:datasetlegacy
SH.sl.pval=summary.lm(SHlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(SH.int.pval<0.05) {
  intTable[which(intTable[,1]=="Shrimp_mt"),2]= SH.int } else {
    intTable[which(intTable[,1]=="Shrimp_mt"),2]= 0
  }

if(SH.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Shrimp_mt"),3]= SH.sl} else {
    intTable[which(intTable[,1]=="Shrimp_mt"),3]= 0
  }


#### Chinook salmon #different slope and intercept
CHov= dataOvSal %>%
  select (year, dataset, Chinook_mt)
 # filter(year!=1991) %>%
#  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=CHov, aes(x=year, y=log(Chinook_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept ANOVA (aov)
CHlm=aov(log(Chinook_mt) ~ year*dataset, data = CHov)
summary.lm(CHlm) # No difference slope (Year*Source) or intercept (Source)

CH.int=summary.lm(CHlm)$coefficients[3,1]#estimate datasetlegacy
CH.int.pval=summary.lm(CHlm)$coefficients[3,4] #p value datasetlegacy
CH.sl=summary.lm(CHlm)$coefficients[4,1]#estimate year:datasetlegacy
CH.sl.pval=summary.lm(CHlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(CH.int.pval<0.05) {
  intTable[which(intTable[,1]=="Chinook_mt"),2]= CH.int } else {
    intTable[which(intTable[,1]=="Chinook_mt"),2]= 0
  }

if(CH.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Chinook_mt"),3]= CH.sl} else {
    intTable[which(intTable[,1]=="Chinook_mt"),3]= 0
  }

#### Pink salmon #different slope and intercept
PKov= dataOvSal %>%
  select (year, dataset, Pink_mt)
# filter(year!=1991) %>%
#  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=PKov, aes(x=year, y=log(Pink_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by AIC and aov
PKlm1= lm(log(Pink_mt) ~ year, data = PKov )
PKlm2= lm(log(Pink_mt) ~ year + dataset, data = PKov )
PKlm3= lm(log(Pink_mt) ~ year*dataset, data = PKov )
AIC(PKlm1,PKlm2,PKlm3) 

PKlm=aov(log(Pink_mt) ~ year*dataset, data = PKov)
summary.lm(PKlm) # No difference slope (Year*Source) or intercept (Source)

PK.int=summary.lm(PKlm)$coefficients[3,1]#estimate datasetlegacy
PK.int.pval=summary.lm(PKlm)$coefficients[3,4] #p value datasetlegacy
PK.sl=summary.lm(PKlm)$coefficients[4,1]#estimate year:datasetlegacy
PK.sl.pval=summary.lm(PKlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(PK.int.pval<0.05) {
  intTable[which(intTable[,1]=="Pink_mt"),2]= PK.int } else {
    intTable[which(intTable[,1]=="Pink_mt"),2]= 0
  }

if(PK.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Pink_mt"),3]= PK.sl} else {
    intTable[which(intTable[,1]=="Pink_mt"),3]= 0
  }

#### Chum salmon #different slope and intercept
CHMov= dataOvSal %>%
  select (year, dataset, Chum_mt)
# filter(year!=1991) %>%
#  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=CHMov, aes(x=year, y=log(Chum_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
CHMlm=aov(log(Chum_mt) ~ year*dataset, data = CHMov)
summary.lm(CHMlm) # No difference slope (Year*Source) or intercept (Source)

CHM.int=summary.lm(CHMlm)$coefficients[3,1]#estimate datasetlegacy
CHM.int.pval=summary.lm(CHMlm)$coefficients[3,4] #p value datasetlegacy
CHM.sl=summary.lm(CHMlm)$coefficients[4,1]#estimate year:datasetlegacy
CHM.sl.pval=summary.lm(CHMlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(CHM.int.pval<0.05) {
  intTable[which(intTable[,1]=="Chum_mt"),2]= CHM.int } else {
    intTable[which(intTable[,1]=="Chum_mt"),2]= 0
  }

if(CHM.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Chum_mt"),3]= CHM.sl} else {
    intTable[which(intTable[,1]=="Chum_mt"),3]= 0
  }

#### Sockeye salmon #different slope and intercept
SKov= dataOvSal %>%
  select (year, dataset, Sockeye_mt)
# filter(year!=1991) %>%
#  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=SKov, aes(x=year, y=log(Sockeye_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept by ANOVA (aov)
SKlm=aov(log(Sockeye_mt) ~ year*dataset, data = SKov)
summary.lm(SKlm) # No difference slope (Year*Source) or intercept (Source)

SK.int=summary.lm(SKlm)$coefficients[3,1]#estimate datasetlegacy
SK.int.pval=summary.lm(SKlm)$coefficients[3,4] #p value datasetlegacy
SK.sl=summary.lm(SKlm)$coefficients[4,1]#estimate year:datasetlegacy
SK.sl.pval=summary.lm(SKlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(SK.int.pval<0.05) {
  intTable[which(intTable[,1]=="Sockeye_mt"),2]= SK.int } else {
    intTable[which(intTable[,1]=="Sockeye_mt"),2]= 0
  }

if(SK.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Sockeye_mt"),3]= SK.sl} else {
    intTable[which(intTable[,1]=="Sockeye_mt"),3]= 0
  }

#### Coho salmon #different slope and intercept
COHov= dataOvSal %>%
  select (year, dataset, Coho_mt)
# filter(year!=1991) %>%
#  filter(complete.cases(.)) #remove rows with any NAs

#plot
ggplot(data=COHov, aes(x=year, y=log(Coho_mt), color=dataset), size=1, show.legend = TRUE) +
  geom_point()+
  geom_smooth(method="lm") +#, SE=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")

#analyze for diSBerences in slope and intercept ANOVA (aov)
COHlm=aov(log(Coho_mt) ~ year*dataset, data = COHov)
summary.lm(COHlm) # No difference slope (Year*Source) or intercept (Source)

COH.int=summary.lm(COHlm)$coefficients[3,1]#estimate datasetlegacy
COH.int.pval=summary.lm(COHlm)$coefficients[3,4] #p value datasetlegacy
COH.sl=summary.lm(COHlm)$coefficients[4,1]#estimate year:datasetlegacy
COH.sl.pval=summary.lm(COHlm)$coefficients[4,4] #p value year:datasetlegacy

#populate summary table

if(COH.int.pval<0.05) {
  intTable[which(intTable[,1]=="Coho_mt"),2]= COH.int } else {
    intTable[which(intTable[,1]=="Coho_mt"),2]= 0
  }

if(COH.sl.pval<0.05) {
  intTable[which(intTable[,1]=="Coho_mt"),3]= COH.sl} else {
    intTable[which(intTable[,1]=="Coho_mt"),3]= 0
  }


###########
intTable
