setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis/BasicGraphics")
library(tidyverse)
library(stringi)
#load PSNI X IM 
rawdata <- read_tsv("data/ICPI_FactView_PSNU_IM_20170515_v1_1_Haiti.txt")

#-- must be UTF8 or update 

# filter only needed indicators and cols

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

