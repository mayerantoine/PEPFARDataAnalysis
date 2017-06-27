library(tidyverse)
setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis/fy2017q2_htc")
#load  Site X IM Haiti data
### !!! This dataset doesn't include community data
fy2017q1 <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20170324_v2_2.txt")
fy2017q1
str(fy2017q1)

#cleaning factview Site X IM Haiti  and select HTS data
fy2017q1 <- select(fy2017q1,SNU1:FY2017Q1)
fy2017q1_htc <- fy2017q1 %>% filter(indicator == "HTS_TST" | indicator == "HTS_TST_POS" ) %>% 
                             filter(SNU1 != "_Military Haiti") %>%
                             filter(indicatorType == "DSD" && typeFacility =="Y") %>%
                             select(SNU1,PSNU,PSNUuid,FY17SNUPrioritization,MechanismID,mechanismUID,PrimePartner,+
                                + ImplementingMechanismName,FacilityUID,Facility,indicator,+
                               + numeratorDenom,disaggregate,standardizedDisaggregate,categoryOptionComboName,Age,Sex,+
                               + resultStatus,otherDisaggregate,coarseDisaggregate,modality,isMCAD,FY2015Q2,FY2015Q3,FY2015Q4,+
                               + FY2016Q1,FY2016Q2,FY2016Q3,FY2016Q4,FY2017Q1) %>%
                               gather("quarter","NbTested",FY2015Q2:FY2017Q1)
# Total number of sites 
N_SITE <- fy2017q1_htc %>% summarise(NbSite = n_distinct(Facility))

N_SITE

# Nb sites and total test by quarter,indicator and disaggregation
htc_by_quarter_disaggregate <- fy2017q1_htc %>% 
                              group_by(quarter,indicator,standardizedDisaggregate) %>%
                               filter(NbTested > 0 ) %>% 
                              summarise(nb_tested = sum(NbTested,na.rm=TRUE),nb_site = n_distinct(Facility))

# Total HTC_TST by quarter and Nb sites reported 
Total_HTC_By_Quarter <- htc_by_quarter_disaggregate %>% filter(standardizedDisaggregate == "Total Numerator")
Total_HTC_By_Quarter
Modality/MostCompleteAgeDisagg

# Disaggregate HTC_TST  by quarter and Nb sites reported
Disagg_HTC_By_Quarter <- htc_by_quarter_disaggregate %>% filter(standardizedDisaggregate == "Modality/MostCompleteAgeDisagg" 
                                    | standardizedDisaggregate == "MostCompleteAgeDisagg")
Disagg_HTC_By_Quarter



# Proportion of Male,Female tested by quarter
HTC_proportion_By_Sex <- fy2017q1_htc %>%
                          filter(indicator== "HTS_TST" & (standardizedDisaggregate == "Modality/MostCompleteAgeDisagg" 
                                 | standardizedDisaggregate == "MostCompleteAgeDisagg")) %>%
                          group_by(quarter,Sex) %>%
                          summarise(nb_tested = sum(NbTested, na.rm=TRUE))

HTC_proportion_By_Sex

## Average Test Male ,Female by quarter
HTC_AVG_By_Sex <- HTC_proportion_By_Sex %>%
  group_by(Sex) %>%
  summarise(avg_tested = mean(nb_tested, na.rm=TRUE))
HTC_AVG_By_Sex

## Proportion
HTC_proportion_By_Sex %>% 
  ggplot(aes(quarter,nb_tested,fill=Sex)) +
   geom_col(position = "fill")

## Trend Male  vs Female
HTC_proportion_By_Sex %>% 
  ggplot(aes(Sex,nb_tested,fill=quarter)) +
  geom_col(position = "dodge")

# Proportion of Male,Female tested by quarter by SNU Categorization
HTC_proportion_By_SNUPriotiy_Sex <- fy2017q1_htc %>%
  filter(indicator== "HTS_TST" & (standardizedDisaggregate == "Modality/MostCompleteAgeDisagg" 
                                  | standardizedDisaggregate == "MostCompleteAgeDisagg")) %>%
  group_by(FY17SNUPrioritization,quarter,Sex) %>%
  summarise(nb_tested = sum(NbTested, na.rm=TRUE))

#### NbTest
HTC_proportion_By_SNUPriotiy_Sex %>% ggplot(aes(quarter,nb_tested,fill=Sex))+
   geom_col()+facet_wrap(~FY17SNUPrioritization)

#### Proportion
HTC_proportion_By_SNUPriotiy_Sex %>% ggplot(aes(quarter,nb_tested,fill=Sex))+
  geom_col(position="fill")+facet_wrap(~FY17SNUPrioritization)

# Proportion of Male,Female tested by year by PSNU
HTC_proportion_By_PSNU_Sex <- fy2017q1_htc %>%
  filter(indicator == "HTS_TST" & (standardizedDisaggregate == "Modality/MostCompleteAgeDisagg" 
                                  | standardizedDisaggregate == "MostCompleteAgeDisagg")
                              & (FY17SNUPrioritization == "1 - Scale-Up: Saturation" 
                                | FY17SNUPrioritization == "2 - Scale-Up: Aggressive")) %>%
  group_by(FY17SNUPrioritization,PSNU,quarter,Sex) %>%
  summarise(nb_tested = sum(NbTested, na.rm=TRUE))

HTC_proportion_By_PSNU_Sex

HTC_proportion_By_PSNU_Sex %>% filter(FY17SNUPrioritization == "1 - Scale-Up: Saturation") %>% ggplot(aes(PSNU,nb_tested,fill=Sex))+
  geom_col(position = "fill")+facet_wrap(~quarter)

HTC_proportion_By_PSNU_Sex %>% filter(FY17SNUPrioritization == "2 - Scale-Up: Aggressive") %>% ggplot(aes(PSNU,nb_tested,fill=Sex))+
  geom_col(position = "fill")+facet_wrap(~quarter)

# Proportion of sites testing more Male than Female by quarter
HTC_proportion_By_Facility_Sex <- fy2017q1_htc %>%
  filter(indicator == "HTS_TST" & (standardizedDisaggregate == "Modality/MostCompleteAgeDisagg" 
                                   | standardizedDisaggregate == "MostCompleteAgeDisagg")
         & (FY17SNUPrioritization == "1 - Scale-Up: Saturation" 
            | FY17SNUPrioritization == "2 - Scale-Up: Aggressive")) %>%
  group_by(Facility,quarter,Sex) %>%
  summarise(nb_tested = sum(NbTested, na.rm=TRUE))

HTC_proportion_By_Facility_Sex
# Top sites testing more Male
# Nb Test and Yield by age, sex and by fiscal year
# Key pop resutls Q1
