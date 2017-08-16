
##  Monthly TX_Net_New Analysis

##  Source MESI and ICPI FatcView

##  Mayer Antoine

##  Purpuse : Montoring Site Tx_Net_New  and Attrition monthly, create a site level dataset

##  COP FY17

##  Date : July 25, 2017

##  Updated

## NOTES ########################

    # Site mapping : External file with DATIM-MESI mapping of Facility Name
    # Partner mapping  : External file to map partner and site for COP17

    # Other inputs - must be UTF8 after downloadindg from MESI : 
            #TX_CURR : mesi_report_actifs_juin.csv
            #TX_NEW : mesi_report_nouveau_enrolles_juin.csv


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
    
# load site_mapping
    mapping <- read_csv("site_mapping.csv")


## load_prepare_net_new_data
# TX_CURR
    patient_actifs <- read_csv("mesi_report_actifs_july.csv", skip = 1)
    names(patient_actifs) <- make.names(names(patient_actifs))
    patient_actifs <-  patient_actifs %>% select(RESEAU,INSTITUTION,fy2017Cum =TOTAL)
    patient_actifs <- filter(patient_actifs,!is.na(INSTITUTION))
    patient_actifs <- inner_join(patient_actifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))

#load_prepare_tx_new_data
# TX_NEW 
    patient_enrolles <- read_csv("mesi_report_nouveau_enrolles_july.csv", skip = 1)
    names(patient_enrolles) <- make.names(names(patient_enrolles))
    names(patient_enrolles)[9] <- "TOTAL"
    patient_enrolles <- patient_enrolles %>% select(RESEAU, INSTITUTION,TOTAL)
    patient_enrolles <- filter(patient_enrolles,!is.na(INSTITUTION) , !is.na(RESEAU))
    patient_enrolles <- inner_join(patient_enrolles,mapping, by = c("INSTITUTION" = "Facility.MESI"))

## TX_NET_NEW TARGET
    fy2017_net_new <- rawdata %>%
        filter(indicator %in% c("TX_CURR")) %>%
        filter(snu1 != "_Military Haiti") %>%
        filter(disaggregate == "Total Numerator") %>%
        filter(indicatortype == "DSD") %>% 
        filter(typefacility == "Y") %>%
        filter(numeratordenom == "N") %>%
        select(facility,snu1,psnu,fy2016apr,fy17snuprioritization,fy2017_targets) %>%
        group_by(facility,snu1,psnu,fy17snuprioritization) %>%
         summarise( fy2016apr = sum(fy2016apr, na.rm = TRUE),
                   fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
        mutate(fy17_net_new_target = fy2017targets - fy2016apr)


## quick fix bon repos issue    
    aprbonrepos <- fy2017_net_new[fy2017_net_new$facility == "Hôpital Communautaire De Bon Repos","fy2016apr"]
    fy2017_net_new[fy2017_net_new$facility == "Hopital de Bon Repos","fy2016apr"] <- aprbonrepos
    fy2017_net_new <- filter(fy2017_net_new, !(facility == "Hôpital Communautaire De Bon Repos"))
    


## Load  FY17 partner mapping

    partner_mapping <- read_csv("fy17_partner_mapping.csv")
    names(partner_mapping) <- c("partner_facility","mechanism")


#merge dataset
    fy2017_net_new <- left_join(fy2017_net_new,partner_mapping,by = c("facility" = "partner_facility"))
    fy2017_net_new <- left_join(fy2017_net_new,patient_actifs,by = c("facility" = "Facility.DATIM"))
    fy2017_net_new <- left_join(fy2017_net_new,patient_enrolles, by = c("facility" = "Facility.DATIM"))
    fy2017_net_new <- mutate(fy2017_net_new,
                             fy2017Cum_net_new = fy2017Cum-fy2016apr,
                             fy2017Cum_tx_new = TOTAL,
                             fy2017Cum_attrition = (fy2017Cum - (fy2016apr+fy2017Cum_tx_new)) )%>%
        select(facility,snu1,psnu,fy17snuprioritization,mechanism,fy2016apr,fy2017Cum,fy2017Cum_net_new,fy2017Cum_tx_new,fy2017Cum_attrition,fy17_net_new_target) %>%
        filter(!(is.na(mechanism)))
    
# remove old site
    fy2017_net_new <- filter(fy2017_net_new, !(facility %in% old_site))


#export data set in as csv file
    write.csv(fy2017_net_new,"fy2017_net_new_july.csv")
    
    
### Prison civile des femmes de cabaret not included
