library(tidyverse)
library(lubridate)
setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis")

# in the script convert to UTF-8

#load
patient_demo <- read_tsv("patient_demo.txt")
mesi_cdv <- read_csv("mesi.csv")
mesi_arv <- read_csv("mesi_arv.csv")
isante <- read_csv("isante_dashboard.csv")


institutionCode <- read_csv("InstitutionCode.csv")

getSALVHDataBySite <- function(){
    
    SiteByLastReportDate <- patient_demo %>% 
        group_by(Institution) %>% 
        summarise(NbDiag =n(),LastReportDate = max(DateRemplissage, na.rm= TRUE)) %>%
        arrange(desc(LastReportDate)) %>%
        mutate(lastReportYear = year(LastReportDate))
    SiteByLastReportDate

}


#clean isante dashboard

drop_cols <-c("Nouveaux","Nouveaux_1","Risque","Risque_1","Autre *","Totaux")
isante <- isante %>% select(-one_of(drop_cols))
names(isante)[5] <- "DatedeSaisie"
names(isante) <- sub("_1","SouTAR",names(isante))

