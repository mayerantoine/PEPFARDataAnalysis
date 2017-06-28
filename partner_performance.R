setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis")

library(tidyverse)

rawdata <- read_tsv("BasicGraphics/data/ICPI_FactView_Site_IM_Haiti_20170515_v1_1.txt")

# -- must be UTF8 or update 
# -- lower all col names

names(rawdata) <- tolower(names(rawdata))

old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery",
                   "GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")

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
                summarise( FY17TARGET = sum(fy2017_targets, na.rm= TRUE))
               

#fy2017_target %>% group_by(indicator) %>% summarise(sum(FY17TARGET))
#fy2017_target %>% filter(implementingmechanismname %in% old_mechanism) %>% filter(FY17TARGET > 0) 

## load site_mapping

mapping <- read_csv("site_mapping.csv")

# load mesi cdv data
patient_cdv <-  read_csv("mesi_report_cdv.csv", skip =1)
names(patient_cdv)  <-  make.names(names(patient_cdv))
patient_cdv <- patient_cdv %>% select(RESEAU, INSTITUTION,TOTAL)

patient_cdv <- inner_join(patient_cdv,mapping, by = c("INSTITUTION" = "Facility.MESI"))
patient_cdv <- filter(patient_cdv,!is.na(INSTITUTION))

hts_target <- filter(fy2017_target, indicator == "HTS_TST" & FY17TARGET > 0)

hts_target <- left_join(hts_target,patient_cdv,by = c("facility"="Facility.DATIM"))

#move cul-de-sac to FOSREF 1925  -- Ouanaminthe to Linkages

hts_target[which(hts_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
           c("implementingmechanismname")] <-  "FOSREF 1925"

hts_target[which(hts_target$facility == "Centre Lakay de Ouanaminthe"),
           c("implementingmechanismname")] <-  "Linkages"

by_mechanism <- hts_target %>% 
    group_by(implementingmechanismname) %>% 
    summarise( HTS = sum(TOTAL,na.rm = T), FY17TARGET = sum(FY17TARGET, na.rm = T)) %>%
    mutate(Performance = ifelse( FY17TARGET == 0, 0 , HTS/FY17TARGET ))

by_site <- filter(hts_target,implementingmechanismname  == "FOSREF 1925")


#patient enrolle
patient_enrolles <- read_csv("mesi_report_nouveau_enrolles.csv", skip = 1)
names(patient_enrolles) <- make.names(names(patient_enrolles))
patient_enrolles <- patient_enrolles %>% select(RESEAU, INSTITUTION,TOTAL.ENROLÉS)
names(patient_enrolles)[3] <- "TOTAL"
patient_enrolles <- filter(patient_enrolles,!is.na(INSTITUTION))

patient_enrolles <- inner_join(patient_enrolles,mapping, by = c("INSTITUTION" = "Facility.MESI"))
tx_new_target <- filter(fy2017_target, indicator == "TX_NEW" & FY17TARGET > 0)

tx_new_target <- left_join(tx_new_target,patient_enrolles,by = c("facility"="Facility.DATIM"))

tx_new_target[which(tx_new_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
           c("implementingmechanismname")] <-  "FOSREF 1925"

tx_new_by_mechanism <- tx_new_target %>% 
    group_by(implementingmechanismname) %>% 
    summarise( TX_NEW = sum(TOTAL,na.rm = T), FY17TARGET = sum(FY17TARGET, na.rm = T)) %>%
    mutate(Performance = ifelse( FY17TARGET == 0, 0 , TX_NEW/FY17TARGET ))

tx_new_by_site <- filter(tx_new_target,implementingmechanismname  == "FOSREF 1925")

# patient actifs load mesi data
patient_actifs <- read_csv("mesi_report_actifs.csv", skip = 1)
names(patient_actifs) <- make.names(names(patient_actifs))
patient_actifs <-  patient_actifs %>% select(RESEAU,INSTITUTION,TOTAL)
patient_actifs <- filter(patient_actifs,!is.na(INSTITUTION))

patient_actifs <- inner_join(patient_actifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
tx_curr_target <- filter(fy2017_target, indicator == "TX_CURR" & FY17TARGET > 0)
tx_curr_target <- left_join(tx_curr_target,patient_actifs,by = c("facility"="Facility.DATIM"))


tx_curr_target[which(tx_curr_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
              c("implementingmechanismname")] <-  "FOSREF 1925"

tx_curr_by_mechanism <- tx_curr_target %>% 
    group_by(implementingmechanismname) %>% 
    summarise( TX_CURR = sum(TOTAL,na.rm = T), FY17TARGET = sum(FY17TARGET, na.rm = T)) %>%
    mutate(Performance = ifelse( FY17TARGET == 0, 0 , TX_CURR/FY17TARGET ))


#load data positif patient
patient_positifs <- read_csv("mesi_report_positifs.csv",skip =1)
names(patient_positifs) <-  make.names(names(patient_positifs))
patient_positifs <-  patient_positifs %>% select(RESEAU,INSTITUTION, TOTAL)
patient_positifs <- filter(patient_positifs,!is.na(INSTITUTION))

patient_positifs <- inner_join(patient_positifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))
hts_pos_target <- filter(fy2017_target, indicator == "HTS_TST_POS")
hts_pos_target <- left_join(hts_pos_target,patient_positifs,by = c("facility"="Facility.DATIM"))

hts_pos_target[which(hts_pos_target$facility == "Centre Jeunes Plaine du Cul-de-Sac"),
               c("implementingmechanismname")] <-  "FOSREF 1925"

hts_pos_by_mechanism <- hts_pos_target %>% 
    group_by(implementingmechanismname) %>% 
    summarise( HTS_TST_POS = sum(TOTAL,na.rm = T), FY17TARGET = sum(FY17TARGET, na.rm = T)) %>%
    mutate(Performance = ifelse( FY17TARGET == 0, 0 , HTS_TST_POS/FY17TARGET ))

hts_pos_by_site <- filter(hts_pos_target,implementingmechanismname=="FOSREF 1925")


#yield calculation
positifs <- read_csv("mesi_report_positifs.csv",skip =1)
names(positifs) <-  make.names(names(patient_positifs))
positifs <- filter(positifs,!is.na(INSTITUTION))
positifs <- filter(positifs,!is.na(RESEAU))
positifs <-  positifs %>% select(RESEAU,INSTITUTION, HTS_TST_POS = TOTAL)

cdv <-  read_csv("mesi_report_cdv.csv", skip =1)
names(cdv)  <-  make.names(names(cdv))
cdv <- filter(cdv,!is.na(INSTITUTION))
cdv <- filter(cdv,!is.na(RESEAU))
cdv <- cdv %>% select(RESEAU, INSTITUTION,HTS_TST = TOTAL)

htc_yield <- inner_join(cdv,positifs, by = "INSTITUTION")
htc_yield <-  mutate(htc_yield,HTC_YIELD =ifelse(
    HTS_TST == 0, 0, HTS_TST_POS / HTS_TST
))


htc_yield_by_mechanism <- htc_yield %>% 
    group_by(RESEAU.x) %>% 
    summarise( HTS_TST_POS = sum(HTS_TST_POS,na.rm = T), HTS_TST = sum(HTS_TST, na.rm = T))


# net new 
patient_actifs <- read_csv("mesi_report_actifs.csv", skip = 1)
names(patient_actifs) <- make.names(names(patient_actifs))
patient_actifs <-  patient_actifs %>% select(RESEAU,INSTITUTION,fy2017May =TOTAL)
patient_actifs <- filter(patient_actifs,!is.na(INSTITUTION))

patient_actifs <- inner_join(patient_actifs,mapping, by = c("INSTITUTION" = "Facility.MESI"))


fy2017_net_new <- rawdata %>%
    filter(indicator %in% c("TX_CURR")) %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(facility,fy2016apr,fy2017_targets) %>%
    group_by(facility) %>%
    summarise( fy2016apr = sum(fy2016apr, na.rm = TRUE),
              fy2017targets = sum(fy2017_targets, na.rm = TRUE)) %>%
    mutate(fy17_net_new_target = fy2017targets - fy2016apr)

fy2017_net_new <- left_join(fy2017_net_new,patient_actifs,by = c("facility" = "Facility.DATIM"))
fy2017_net_new <- mutate(fy2017_net_new, fy2017May_net_new = fy2017May-fy2016apr)
