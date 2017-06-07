library(tidyverse)
library(lubridate)
patient_demo <- read_tsv("data/patient_demo.txt")
patient_cd4 <- read_tsv("data/patient_cd4.txt")
patient_who_stage <- read_tsv("data/patient_who_stage.txt")
patient_art_ctx_event <- read_tsv("data/patient_art_ctx_event.txt")

patient_demo_2006  <- patient_demo %>% filter(year(DateDiagnostic) > 2005)
patientID <- patient_demo_2006$ID

patient_cd4_2006 <- patient_cd4 %>% filter(ID_Patient %in% patientID)
patient_who_stage_2006 <- patient_who_stage %>% filter(ID_Patient %in% patientID)
patient_art_ctx_event_2006 <- patient_art_ctx_event %>% filter(ID_Patient %in% patientID)

cd4_by_year <- patient_cd4_2006 %>% group_by(yeartest =year(Clean_DateTest)) %>% summarise(count= n())

write_tsv(patient_demo_2006, path="patient_demo_2006.txt")
write_tsv(patient_cd4_2006, path="patient_cd4_2006.txt")
write_tsv(patient_who_stage_2006, path="patient_who_stage_2006.txt")
write_tsv(patient_art_ctx_event_2006, path="patient_art_ctx_event_2006.txt")

## NA Fields
##  Review site with last report date less than 2016
Institution_by <- patient_demo %>% 
  group_by(Institution) %>% 
  summarise(NbDiag =n(),DateMaxDx = max(DateDiagnostic,na.rm= TRUE), DateMaxRemp = max(DateRemplissage, na.rm= TRUE))


Institution_byMaxDx <-  arrange(Institution_by,desc(DateMaxDx))
Institution_byMaxDx  <- mutate(Institution_byMaxDx,yearDx = year(DateMaxDx))

Institution_byMaxRemp <-  arrange(Institution_by,desc(DateMaxRemp))
Institution_byMaxRemp  <- mutate(Institution_byMaxRemp,yearRemp = year(DateMaxRemp))
head(Institution_byMaxRemp,20)

table(Institution_byMaxRemp$yearRemp)
filter(Institution_byMaxRemp,NbDiag < 25)
filter(Institution_byMaxRemp,yearRemp > 2012 & yearRemp <=2015)
