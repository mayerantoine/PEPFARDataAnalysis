setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis/BasicGraphics")
library(tidyverse)
library(stringi)
#load PSNI X IM 
rawdata <- read_tsv("data/ICPI_FactView_PSNU_IM_20170515_v1_1_Haiti.txt")

#-- must be UTF8 or update 

# filter only needed indicators and cols
#short partner names

rm_indicators<- c("LAB_PT_TBAFB","CARE_COMM","VMMC_CIRC","INVS_COMD","LAB_PTCQI_LAB","LAB_PT_TBCULTURE",
                  "LAB_PT_EID","LAB_PT_CD4","LAB_CAP","KP_MAT","SC_STOCK","BS_SCREEN","BS_COLL","LAB_ACC",
                  "INVS_COMD","LAB_PTCQI","TB_OUTCOME")

rm_col <- c("otherdisaggregate","ïregion","countryname","regionuid","operatingunit",
            "operatingunituid","typemilitary")

fy2017q2 <- rawdata %>% filter(snu1 != "_Military Haiti") %>%
                        filter(!(indicator %in% rm_indicators)) %>%
                        select(-one_of(rm_col))

# dirty fix for missing prioritization -- vallieres "5 - Centrally Supported"
# should review and update from update from other file
fy2017q2 <- mutate(fy2017q2,
       fy17snuprioritization = ifelse(is.na(fy17snuprioritization),"5 - Centrally Supported",fy17snuprioritization)
)



fy2017q2 <- mutate(fy2017q2,fy16snuprioritization = fy17snuprioritization)

## TX_NEW Trend
tx_new_by_q <- fy2017q2 %>%
                filter(indicator == "TX_NEW") %>%
                filter(indicatortype == "DSD") %>% 
                filter(numeratordenom == "N") %>%  
                filter(disaggregate == "Total Numerator") %>%
                summarise(fy2015q3 = sum(fy2015q3,na.rm=TRUE),
                          fy2015q4 = sum(fy2015q4,na.rm=TRUE),
                          fy2016q1 = sum(fy2016q1,na.rm=TRUE),
                          fy2016q2 = sum(fy2016q2,na.rm=TRUE),
                          fy2016q3 = sum(fy2016q3,na.rm=TRUE),
                          fy2016q4 = sum(fy2016q4,na.rm=TRUE),
                          fy2017q1 = sum(fy2017q1,na.rm=TRUE),
                          fy2017q2 = sum(fy2017q2,na.rm=TRUE)
                          ) %>%
                gather("quarter","TX_NEW",fy2015q3:fy2017q2)
tx_new_by_q

ggplot(tx_new_by_q)+
    geom_bar(aes(quarter,TX_NEW), stat="identity", width = 0.6,fill = "#5b9bd5") +
    geom_text(aes(x =quarter, y=TX_NEW, label = TX_NEW),vjust = 0)+
    labs(title = "TX_NEW Trend")+
    ylab(" ")


## HTS Trend
hts_tst_by_q <- fy2017q2 %>%
    filter(indicator == "HTS_TST") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%  
    filter(disaggregate == "Total Numerator") %>%
    summarise(fy2015q3 = sum(fy2015q3,na.rm=TRUE),
              fy2015q4 = sum(fy2015q4,na.rm=TRUE),
              fy2016q1 = sum(fy2016q1,na.rm=TRUE),
              fy2016q2 = sum(fy2016q2,na.rm=TRUE),
              fy2016q3 = sum(fy2016q3,na.rm=TRUE),
              fy2016q4 = sum(fy2016q4,na.rm=TRUE),
              fy2017q1 = sum(fy2017q1,na.rm=TRUE),
              fy2017q2 = sum(fy2017q2,na.rm=TRUE)
    ) %>%
    gather("quarter","HTS_TST",fy2015q3:fy2017q2)
hts_tst_by_q

ggplot(hts_tst_by_q)+
    geom_bar(aes(quarter,HTS_TST), stat="identity", width = 0.6,fill = "#5b9bd5") +
    geom_text(aes(x =quarter, y=HTS_TST, label = HTS_TST),vjust = 0)+
    labs(title = "HTS_TST Trend")+
    ylab(" ")


# Indicator By Partner for Q2, partner score card
q2_ind <- c("HTS_TST","TX_NEW","TX_CURR","HTS_TST_POS",
            "PMTCT_ART","PMTCT_STAT","PMTCT_EID_POS","PMTCT_EID",
            "TB_STAT","TB_STAT_POS","TB_ART","TX_TB","OVC_SERV","PMTCT_STAT_POS","KP_PREV","PP_PREV")

ind_sum <- c("HTS_TST","TX_NEW","HTS_TST_POS", "PMTCT_ART","PMTCT_STAT","PMTCT_EID_POS","PMTCT_EID",
             "TB_STAT","TB_STAT_POS","PMTCT_STAT_POS")
ind_cum <- c("TX_CURR","TB_ART","TX_TB","OVC_SERV")

indicator_by_partner <- fy2017q2 %>%
                        filter(implementingmechanismname != "Dedup") %>%
                        filter(indicator %in% q2_ind) %>%
                        filter(indicatortype == "DSD") %>% 
                        filter(numeratordenom == "N") %>%  
                        filter(disaggregate == "Total Numerator") %>%
                        select(implementingmechanismname,indicator,fy2017q1, fy2017q2,fy2017_targets) %>%
                        group_by(implementingmechanismname, indicator) %>%
                        summarise(fy2017q1 = sum(fy2017q1, na.rm= TRUE), fy2017q2 = sum(fy2017q2, na.rm= TRUE),
                                  fy17targets = sum(fy2017_targets, na.rm= TRUE)) %>%
                        mutate( fy2017sapr = ifelse( indicator %in% ind_sum,fy2017q1+fy2017q2,fy2017q2)) %>%
                        mutate(fy2017_performance = ifelse(is.na(fy17targets)|fy17targets ==0 ,0,fy2017sapr/fy17targets)) %>%
                        select(implementingmechanismname, indicator,fy2017_performance) %>%
                        spread(indicator,fy2017_performance)
                        
indicator_by_partner
write_csv(indicator_by_partner,"indicator_by_partnerv2.csv")       



# HTC by Sex

htc_by_sex <- fy2017q2 %>%
    filter(indicator %in% c("HTS_TST","HTS_TST_POS")) %>%
    filter(indicatortype == "DSD") %>% 
    filter(standardizeddisaggregate == "Modality/MostCompleteAgeDisagg" 
                                    | standardizeddisaggregate == "MostCompleteAgeDisagg") %>%
    select(sex,indicator,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2) %>%
    group_by(sex,indicator) %>%
    summarise(fy2015q3 = sum(fy2015q3,na.rm=TRUE),
              fy2015q4 = sum(fy2015q4,na.rm=TRUE),
              fy2016q1 = sum(fy2016q1,na.rm=TRUE),
              fy2016q2 = sum(fy2016q2,na.rm=TRUE),
              fy2016q3 = sum(fy2016q3,na.rm=TRUE),
              fy2016q4 = sum(fy2016q4,na.rm=TRUE),
              fy2017q1 = sum(fy2017q1,na.rm=TRUE),
              fy2017q2 = sum(fy2017q2,na.rm=TRUE)
    ) %>%
    select(sex,indicator,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2)
htc_by_sex

write.csv(htc_by_sex,"htc_by_sex2.csv")



# HTC by SNU
htc_by_snu <- fy2017q2 %>%
    filter(indicator %in% c("HTS_TST","HTS_TST_POS")) %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%  
    filter(disaggregate == "Total Numerator") %>%
    select(psnu,indicator,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2) %>%
    group_by(psnu,indicator) %>%
    summarise(fy2015q3 = sum(fy2015q3,na.rm=TRUE),
              fy2015q4 = sum(fy2015q4,na.rm=TRUE),
              fy2016q1 = sum(fy2016q1,na.rm=TRUE),
              fy2016q2 = sum(fy2016q2,na.rm=TRUE),
              fy2016q3 = sum(fy2016q3,na.rm=TRUE),
              fy2016q4 = sum(fy2016q4,na.rm=TRUE),
              fy2017q1 = sum(fy2017q1,na.rm=TRUE),
              fy2017q2 = sum(fy2017q2,na.rm=TRUE)
    ) %>%
    select(psnu,indicator,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2)

htc_by_snu

#write.csv(htc_by_snu, "htc_by_snu.csv")

# OVC_SER by SNU

ovc_serv_by_snu <- fy2017q2 %>%
    filter(indicator %in% c("OVC_SERV")) %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%  
    filter(disaggregate == "Total Numerator") %>%
    select(psnu,indicator,fy17snuprioritization,fy2015q4,fy2016q4,fy2017q2) %>%
    group_by(psnu,indicator,fy17snuprioritization) %>%
    summarise(fy2015q4 = sum(fy2015q4,na.rm=TRUE),
              fy2016q4 = sum(fy2016q4,na.rm=TRUE),
              fy2017q2 = sum(fy2017q2,na.rm=TRUE)
    ) %>%
    select(psnu,indicator,fy17snuprioritization,fy2015q4,fy2016q4,fy2017q2)
ovc_serv_by_snu

#write.csv(ovc_serv_by_snu,"ovc_serv_by_snu.csv")

# filter implemeting mechanim data

by_im <- fy2017q2 %>%
            filter(indicator %in% q2_ind) %>%
            filter(implementingmechanismname != "Dedup") %>%
            filter(indicatortype == "DSD") %>% 
            filter(numeratordenom == "N" | numeratordenom == "D") %>%  
            filter(disaggregate == "Total Numerator" | disaggregate == "Total Denominator") %>%
            select(implementingmechanismname,indicator,disaggregate,fy2017q1,fy2017q2) %>%
            group_by(implementingmechanismname,indicator,disaggregate) %>%
            summarise(fy2017q1 = sum(fy2017q1, na.rm = T),
                      fy2017q2 = sum(fy2017q2, na.rm = T)) %>%
            mutate( fy2017sapr = ifelse( indicator %in% ind_sum,fy2017q1+fy2017q2,fy2017q2)) %>%
            select(implementingmechanismname,indicator,disaggregate,fy2017q1,fy2017q2,fy2017sapr)

by_im
write.csv(by_im,"by_im.csv")
