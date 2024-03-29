setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis/BasicGraphics")
library(tidyverse)
library(stringi)
#load PSNI X IM 
rawdata <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20170515_v1_1.txt")

# -- must be UTF8 or update 
# -- lower all col names

names(rawdata) <- tolower(names(rawdata))
# filter only needed indicators and cols

rm_indicators<- c("LAB_PT_TBAFB","CARE_COMM","VMMC_CIRC","INVS_COMD","LAB_PTCQI_LAB","LAB_PT_TBCULTURE",
                  "LAB_PT_EID","LAB_PT_CD4","LAB_CAP","KP_MAT","SC_STOCK","BS_SCREEN","BS_COLL","LAB_ACC",
                  "INVS_COMD","LAB_PTCQI","TB_OUTCOME")

rm_col <- c("otherdisaggregate","region","countryname","regionuid","operatingunit",
            "operatingunituid","typemilitary","community","typecommunity","fy16communityprioritization",
            "fy17communityprioritization")

fy2017q2 <- rawdata %>% filter(snu1 != "_Military Haiti") %>%
    filter(!(indicator %in% rm_indicators)) %>%
    select(-one_of(rm_col))

fy2017_net_new <- fy2017q2 %>%
                 filter(indicator %in% c("TX_CURR")) %>%
                 filter(disaggregate == "Total Numerator") %>%
                 filter(indicatortype == "DSD") %>% 
                 filter(typefacility == "Y") %>%
                 filter(numeratordenom == "N") %>%
                 select(facility,fy2015apr,fy2016apr,fy2017q2,fy2017_targets) %>%
                 group_by(facility) %>%
                 summarise(fy2015apr = sum(fy2015apr, na.rm= TRUE),
                           fy2016apr = sum(fy2016apr, na.rm = TRUE),
                           fy2017q2 = sum(fy2017q2, na.rm = TRUE),
                           fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
                 mutate(fy16_net_new = fy2016apr - fy2015apr,
                        fy17q2_net_new = fy2017q2 - fy2016apr ,
                        fy17_net_new_target = fy2017targets - fy2016apr)


tx_new <-  fy2017q2 %>%
            filter(indicator %in% c("TX_NEW")) %>%
            filter(disaggregate == "Total Numerator") %>%
            filter(indicatortype == "DSD") %>% 
            filter(typefacility == "Y") %>%
            filter(numeratordenom == "N") %>%
            select(facility,fy2017q1,fy2017q2) %>%
            group_by(facility) %>%
            summarise(fy2017q1 = sum(fy2017q1, na.rm = TRUE),
                      fy2017q2 = sum(fy2017q2, na.rm = TRUE)) %>%
            mutate(fy2017_tx_new = fy2017q1 + fy2017q1 ) %>%
            select(facility,fy2017_tx_new)
                
fy2017_net_new <- left_join(fy2017_net_new,tx_new, by="facility")

fy2017_net_new <- mutate(fy2017_net_new,
                       fy17q2_attrition = fy2017q2 -(fy2016apr + fy2017_tx_new))

# net new by SNU
fy2017_net_new_SNU  <- fy2017q2 %>%
    filter(indicator %in% c("TX_CURR")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(psnu,fy17snuprioritization,fy2015apr,fy2016apr,fy2017q2,fy2017_targets) %>%
    group_by(psnu,fy17snuprioritization) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm= TRUE),
              fy2016apr = sum(fy2016apr, na.rm = TRUE),
              fy2017q2 = sum(fy2017q2, na.rm = TRUE),
              fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
    mutate(fy16_net_new = fy2016apr - fy2015apr,
           fy17q2_net_new = fy2017q2 - fy2016apr ,
           fy17_net_new_target = fy2017targets -  fy2016apr)

fy2017_net_new_SNU$gap_to_target <- ifelse(fy2017_net_new_SNU$fy17_net_new_target>0 & fy2017_net_new_SNU$fy17_net_new_target > fy2017_net_new_SNU$fy17q2_net_new,
                                           fy2017_net_new_SNU$fy17_net_new_target - fy2017_net_new_SNU$fy17q2_net_new,0)

#gap <- ifelse(fy2017_net_new_SNU$fy17_net_new_target>0 & fy2017_net_new_SNU$fy17_net_new_target > fy2017_net_new_SNU$fy17q2_net_new,
 #                    fy2017_net_new_SNU$fy17_net_new_target - fy2017_net_new_SNU$fy17q2_net_new,0)

#fy2017_net_new_SNU <- fy2017_net_new_SNU %>% mutate(gap_to_target = gap )

write.csv(fy2017_net_new_SNU,"fy2017_net_new_SNU.csv")

fy2017_IM_Q2  <- fy2017q2 %>%
    filter(indicator %in% c("TX_CURR")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fy2017q2) %>%
    group_by(implementingmechanismname) %>%
    summarise(fy2017q2 = sum(fy2017q2, na.rm = TRUE))


# net new by Agency

fy2017_net_new_Agency <- fy2017q2 %>%
    filter(indicator %in% c("TX_CURR")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(fundingagency,fy2015apr,fy2016apr,fy2017q2,fy2017_targets) %>%
    group_by(fundingagency) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm= TRUE),
              fy2016apr = sum(fy2016apr, na.rm = TRUE),
              fy2017q2 = sum(fy2017q2, na.rm = TRUE),
              fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
    mutate(fy16_net_new = fy2016apr - fy2015apr,
           fy17q2_net_new = fy2017q2 - fy2016apr ,
           fy17_net_new_target = fy2017targets -  fy2016apr)

fy2017_net_new_Agency$gap_to_target <- ifelse(fy2017_net_new_Agency$fy17_net_new_target>0 & fy2017_net_new_Agency$fy17_net_new_target > fy2017_net_new_Agency$fy17q2_net_new,
                                              fy2017_net_new_Agency$fy17_net_new_target - fy2017_net_new_Agency$fy17q2_net_new,0)         

write.csv(fy2017_net_new_Agency,"fy2017_net_new_Agency.csv")

# net new by OU

fy2017_net_new_OU <- fy2017q2 %>%
    filter(indicator %in% c("TX_CURR")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(fy2015apr,fy2016apr,fy2017q2,fy2017_targets) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm= TRUE),
              fy2016apr = sum(fy2016apr, na.rm = TRUE),
              fy2017q2 = sum(fy2017q2, na.rm = TRUE),
              fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
    mutate(fy16_net_new = fy2016apr - fy2015apr,
           fy17q2_net_new = fy2017q2 - fy2016apr ,
           fy17_net_new_target = fy2017targets -  fy2016apr)

fy2017_net_new_OU$gap_to_target <- ifelse(fy2017_net_new_OU$fy17_net_new_target>0 & fy2017_net_new_OU$fy17_net_new_target > fy2017_net_new_OU$fy17q2_net_new,
                                          fy2017_net_new_OU$fy17_net_new_target - fy2017_net_new_OU$fy17q2_net_new,0)         
write.csv(fy2017_net_new_OU,"fy2017_net_new_OU.csv")
