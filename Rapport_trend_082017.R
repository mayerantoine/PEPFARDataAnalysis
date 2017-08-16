setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis")

library(tidyverse)
library(knitr)
library(formattable)

rm(list=ls())

rawdata <- read_tsv("BasicGraphics/data/ICPI_FactView_Site_IM_Haiti_20170515_v1_1.txt")
names(rawdata) <- tolower(names(rawdata))

fy2017_results <- rawdata %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("TX_NEW","HTS_TST","HTS_TST_POS","TX_CURR","PMTCT_STAT","PMTCT_STAT_POS","PMTCT_ART","OVC_SERV")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015q2,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2) %>%
    group_by(indicator) %>%
    summarise(fy2015q2 = sum(fy2015q2,na.rm=T),
              fy2015q3 = sum(fy2015q3,na.rm=T),
              fy2015q4 = sum(fy2015q4,na.rm=T),
              fy2016q1 = sum(fy2016q1,na.rm=T),
              fy2016q2 = sum(fy2016q2,na.rm=T),
              fy2016q3 = sum(fy2016q3,na.rm=T),
              fy2016q4 = sum(fy2016q4,na.rm=T),
              fy2017q1 = sum(fy2017q1,na.rm=T),
              fy2017q2 = sum(fy2017q2,na.rm=T))

write_csv(fy2017_results,"fy2017_results.csv")

fy2017_apr <- rawdata %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("TX_NEW","HTS_TST","HTS_TST_POS","TX_CURR","PMTCT_STAT","PMTCT_STAT_POS","PMTCT_ART","OVC_SERV")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015apr,fy2016apr,fy2017q1,fy2017q2) %>%
    group_by(indicator) %>%
    summarise(fy2015apr = sum(fy2015apr,na.rm=T),
              fy2016apr = sum(fy2016apr,na.rm=T),
              fy2017q1 = sum(fy2017q1,na.rm=T),
              fy2017q2 = sum(fy2017q2,na.rm=T))

write_csv(fy2017_apr,"fy2017_apr.csv")
