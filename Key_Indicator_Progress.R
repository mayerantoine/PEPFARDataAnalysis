
##  Monthly Indicator Progress Analysis

##  Source MESI and ICPI FatcView

##  Mayer Antoine

##  Purpuse : Montoring our progress for key indicators to reach targets 

##  COP FY17

##  Date : July 26, 2017

##  Updated

## NOTES ########################

# Site mapping : External file with DATIM-MESI mapping of Facility Name
# Partner mapping  : External file to map partner and site for COP17

# Other inputs - must be UTF8 after downloadindg from MESI : 

#HTS_TST : mesi_report_cdv
#HTS_TST_POS : mesi_report_positifs
#TX_CURR : mesi_report_actifs.csv
#TX_NEW : mesi_report_nouveau_enrolles.csv
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
        select(facility,snu1,psnu,fy17snuprioritization,implementingmechanismname,indicator,fy2017_targets) %>%
        group_by(facility,snu1,psnu,fy17snuprioritization,implementingmechanismname,indicator) %>%
        summarise( FY17TARGET = sum(fy2017_targets, na.rm= TRUE)) %>%
        filter(FY17TARGET > 0)
    
   
## load site_mapping
        mapping <- read_csv("site_mapping.csv")
        
 
## import MESI data  and merge with results
         #  TESTING DATA
        patient_cdv <-  read_csv("mesi_report_cdv_juin2.csv", skip =1)
        names(patient_cdv)  <-  make.names(names(patient_cdv))
        patient_cdv <- patient_cdv %>% 
                        select(RESEAU, INSTITUTION, HTS_TST= TOTAL) %>%
                        filter(!is.na(INSTITUTION)) %>%
                        filter(!is.na(RESEAU))
        patient_cdv <- inner_join(patient_cdv,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        
        hts_target <- filter(fy2017_target, indicator == "HTS_TST" & FY17TARGET > 0)
        hts_target <- left_join(hts_target,patient_cdv,by = c("facility"="Facility.DATIM"))
        
        hts_target[which(hts_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
                   c("implementingmechanismname")] <-  "FOSREF 1925"
        hts_target[which(hts_target$facility == "Centre Lakay de Ouanaminthe"),
                   c("implementingmechanismname")] <-  "Linkages"
        hts_target <- hts_target %>% select(facility,snu1,psnu,fy17snuprioritization,mechanism = implementingmechanismname,
                                            indicator,FY17Results = HTS_TST, FY17TARGET)
         # HTS_TST_POS
        patient_positifs <- read_csv("mesi_report_positifs_juin2.csv",skip =1)
        names(patient_positifs) <-  make.names(names(patient_positifs))
        patient_positifs <- patient_positifs %>% 
                            select(RESEAU,INSTITUTION, HTS_TST_POS=TOTAL) %>%
                            filter(!is.na(INSTITUTION)) %>%
                            filter(!is.na(RESEAU))
        patient_positifs <- inner_join(patient_positifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        
        hts_pos_target <- filter(fy2017_target, indicator == "HTS_TST_POS")
        hts_pos_target <- left_join(hts_pos_target,patient_positifs,by = c("facility"="Facility.DATIM"))
        hts_pos_target[which(hts_pos_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
                       c("implementingmechanismname")] <-  "FOSREF 1925"
        hts_pos_target <- hts_pos_target %>% select(facility,snu1,psnu,fy17snuprioritization,mechanism = implementingmechanismname,
                                                    indicator,FY17Results = HTS_TST_POS, FY17TARGET)
        
        # TX_CURR
        patient_actifs <- read_csv("mesi_report_actifs_juin2.csv", skip = 1)
        names(patient_actifs) <- make.names(names(patient_actifs))
        patient_actifs <-  patient_actifs %>% 
                           select(RESEAU,INSTITUTION,TX_CURR = TOTAL) %>%
                           filter(!is.na(INSTITUTION)) %>%
                           filter(!is.na(RESEAU))
        patient_actifs <- inner_join(patient_actifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        
        tx_curr_target <- filter(fy2017_target, indicator == "TX_CURR" & FY17TARGET > 0)
        tx_curr_target <- left_join(tx_curr_target,patient_actifs,by = c("facility"="Facility.DATIM"))
        
        tx_curr_target[which(tx_curr_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
                       c("implementingmechanismname")] <-  "FOSREF 1925"
        tx_curr_target <- tx_curr_target %>% select(facility,snu1,psnu,fy17snuprioritization,mechanism = implementingmechanismname,
                                                    indicator,FY17Results = TX_CURR, FY17TARGET)
        
        #TX_NEW
        patient_enrolles <- read_csv("mesi_report_nouveau_enrolles_juin2.csv", skip = 1)
        names(patient_enrolles) <- make.names(names(patient_enrolles))
        names(patient_enrolles)[9] <- "TOTAL"
        patient_enrolles <- patient_enrolles %>% 
                            select(RESEAU, INSTITUTION,TX_NEW = TOTAL) %>%
                            filter(!is.na(INSTITUTION)) %>%
                            filter(!is.na(RESEAU))
        patient_enrolles <- inner_join(patient_enrolles,mapping, by = c("INSTITUTION" = "Facility.MESI"))
        tx_new_target <- filter(fy2017_target, indicator == "TX_NEW" & FY17TARGET > 0)
        
        tx_new_target <- left_join(tx_new_target,patient_enrolles,by = c("facility"="Facility.DATIM"))
        
        tx_new_target[which(tx_new_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
                      c("implementingmechanismname")] <-  "FOSREF 1925"
        tx_new_target <- tx_new_target %>% select(facility,snu1,psnu,fy17snuprioritization,mechanism = implementingmechanismname,
                                                  indicator,FY17Results = TX_NEW, FY17TARGET)

## merge MESI and Target data
        fy2017_results <- rbind(hts_target,hts_pos_target,tx_new_target,tx_curr_target)
        
#export data set in as csv file
        write.csv(fy2017_results,"fy2017_results.csv")
    
    
