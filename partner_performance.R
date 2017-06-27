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
               

fy2017_target %>% group_by(indicator) %>% summarise(sum(FY17TARGET))
fy2017_target %>% filter(implementingmechanismname %in% old_mechanism) %>% filter(FY17TARGET > 0) 

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

hts_target[is.na(hts_target$TOTAL),]

# load mesi data
patient_actifs <- read_csv("mesi_report_actifs.csv", skip = 1)
names(actis) <- make.names(names(patient_actifs))
patient_actifs <-  patient_actifs %>% select(RESEAU,INSTITUTION,TOTAL)


patient_enrolles <- read_csv("mesi_report_nouveau_enrolles.csv", skip = 1)
names(patient_enrolles) <- make.names(names(patient_enrolles))
patient_enrolles <- patient_enrolles %>% select(RESEAU, INSTITUTION,TOTAL.ENROLÉS)

patient_positifs <- read_csv("mesi_report_positifs.csv",skip =1)
names(patient_positifs) <-  make.names(names(patient_positifs))
patient_positifs <-  patient_positifs %>% select(RESEAU,INSTITUTION, TOTAL)