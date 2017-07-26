
##  Monthly Indicator Progress Analysis

##  Source MESI and ICPI FatcView

##  Mayer Antoine

##  Purpuse : Montoring our progress for key indicators to achieve 

##  COP FY17

##  Date : July 25, 2017

##  Updated

## NOTES ########################

# Site mapping : External file with DATIM-MESI mapping of Facility Name
# Partner mapping  : External file to map partner and site for COP17

# Other inputs - must be UTF8 after downloadindg from MESI : 

#HTS_TST : mesi_report_cdv
#HTS_TST_POS : mesi_report_positifs
#TX_CURR : mesi_report_actifs_juin.csv
#TX_NEW : mesi_report_nouveau_enrolles_juin.csv
#


####################################################################################

setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis")

library(tidyverse)

rm(list=ls())

##  import ICPI FactView to extract target FY17 and APR16 Results
    rawdata <- read_tsv("BasicGraphics/data/ICPI_FactView_Site_IM_Haiti_20170515_v1_1.txt")
    names(rawdata) <- tolower(names(rawdata))
    old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery",
                       "GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")
    old_site <- c("Centre de Santé de Rousseau OSAPO",
                  "Centre de Santé Baie-de-Henne","Prison Civile de Pétion-Ville","Prison Civile des Cayes")

## select fy2017 targets for key indicators    
    fy2017_target <- rawdata %>%
        filter(snu1 != "_Military Haiti") %>%
        filter(indicator %in% c("TX_NEW","HTS_TST","HTS_TST_POS","TX_CURR")) %>%
        filter(!(implementingmechanismname %in% old_mechanism)) %>%
        filter(disaggregate == "Total Numerator") %>%
        filter(indicatortype == "DSD") %>% 
        filter(typefacility == "Y") %>%
        filter(numeratordenom == "N") %>%
        select(facility,implementingmechanismname,indicator,fy2017_targets) %>%
        group_by(facility,implementingmechanismname,indicator) %>%
        summarise( FY17TARGET = sum(fy2017_targets, na.rm= TRUE)) %>%
        filter(FY17TARGET > 0)
    
##  reshape target dataset and rename columns    
        fy2017_target <- spread(fy2017_target,indicator,FY17TARGET)
        names(fy2017_target) <- c("facility","mechanism","FY17TARGET.HTS_TST","FY17TARGET.HTS_TST_POS",
                                  "FY17TARGET.TX_CURR","FY17TARGET.TX_NEW")
 
## import MESI data
       
         #  TESTING DATA
        patient_cdv <-  read_csv("mesi_report_cdv.csv", skip =1)
        names(patient_cdv)  <-  make.names(names(patient_cdv))
        patient_cdv <- patient_cdv %>% select(RESEAU, INSTITUTION,TOTAL)
        patient_cdv <- inner_join(patient_cdv,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        patient_cdv <- filter(patient_cdv,!is.na(INSTITUTION))
       
         # HTS_TST_POS
        patient_positifs <- read_csv("mesi_report_positifs.csv",skip =1)
        names(patient_positifs) <-  make.names(names(patient_positifs))
        patient_positifs <-  patient_positifs %>% select(RESEAU,INSTITUTION, TOTAL)
        patient_positifs <- filter(patient_positifs,!is.na(INSTITUTION))
        patient_positifs <- inner_join(patient_positifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        
        # TX_CURR
        patient_actifs <- read_csv("mesi_report_actifs.csv", skip = 1)
        names(patient_actifs) <- make.names(names(patient_actifs))
        patient_actifs <-  patient_actifs %>% select(RESEAU,INSTITUTION,TOTAL)
        patient_actifs <- filter(patient_actifs,!is.na(INSTITUTION))
        patient_actifs <- inner_join(patient_actifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        
        #TX_NEW
        patient_enrolles <- read_csv("mesi_report_nouveau_enrolles.csv", skip = 1)
        names(patient_enrolles) <- make.names(names(patient_enrolles))
        names(patient_enrolles)[11] <- "TOTAL"
        patient_enrolles <- patient_enrolles %>% select(RESEAU, INSTITUTION,TOTAL)
        patient_enrolles <- filter(patient_enrolles,!is.na(INSTITUTION))
    
## load site_mapping
    mapping <- read_csv("site_mapping.csv")
