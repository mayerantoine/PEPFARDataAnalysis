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
names(institutionCode)[2] <- "Code"

getSALVHDataBySite <- function(){
    
    salvh_data <- patient_demo %>% 
        group_by(Institution) %>% 
        summarise(NbDiag =n(),LastReportDate = max(DateRemplissage, na.rm= TRUE)) %>%
        arrange(desc(LastReportDate)) %>%
        mutate(lastReportYear = year(LastReportDate))
    
    salvh_data <- salvh_data[!is.na(salvh_data$Institution),]

}

#salvh_data <- getSALVHDataBySite()
#clean isante dashboard


drop_cols <-c("Nouveaux","Nouveaux_1","Risque","Risque_1","Autre *","Totaux")
isante <- isante %>% select(-one_of(drop_cols))
names(isante)[5] <- "DatedeSaisie"
names(isante)[1] <- "Institution"
names(isante) <- sub("_1","SouTAR",names(isante))

isante <- isante[!is.na(isante$Institution),]
isante$Serveur <- ifelse(!is.na(isante$Serveur),c("Oui"),c("Non"))
isante$DatedeSaisie <- as.Date(isante$DatedeSaisie,"%m/%d/%Y")
isante_data <- isante[6:13]
isante_data <- as.data.frame(apply(isante_data,2,function(x)gsub('\\s+', '',x)))

isante_data <-as.data.frame( apply(isante_data,2,function(x){
    data.frame(do.call('rbind', strsplit(as.character(x),'/',fixed=TRUE)))
}))

isante_data <- as.data.frame(lapply(isante_data, function(x){as.numeric(as.character(x))}))

names(isante_data) <- gsub("X1","Adultes",names(isante_data))
names(isante_data) <- gsub("X2","Enfants",names(isante_data))
isante_clean <- cbind(isante[1:5],isante_data)


## merge isante and salvh

m1 <- left_join(salvh_data,institutionCode, by = "Institution")
m2 <- left_join(m1,isante_clean, by="Code")

