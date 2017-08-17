library(tidyverse)
library(lubridate)


setwd("C:/Users/wsn8/Documents/PEPFARDataAnalysis")


rm(list=ls())
patient_demo <-  read_tsv("FileBox/patient_demo.txt")
patient_cd4 <-  read_tsv("FileBox/patient_cd4.txt")
patient_art_ctx_event <-  read_tsv("FileBox/patient_art_ctx_event.txt")
patient_ptme <-  read_tsv("FileBox/patient_ptme.txt")
patient_regime <-  read_tsv("FileBox/patient_regime.txt")
patient_who_stage <-  read_tsv("FileBox/patient_who_Stage.txt")

## NA Fields Patient demo -- OK
colSums(is.na(patient_demo))

## NA Fields patient_cd4
colSums(is.na(patient_cd4))

patient_cd4_u <- patient_cd4 %>% select(Id,Numero,DateTest,Valeur,Source,ID_Patient)

## NA Fields patient_art_ctx_event
colSums(is.na(patient_art_ctx_event_u))

patient_art_ctx_event_u <- patient_art_ctx_event %>% select(Id,EventType,EventDescription,Numero,EventDate,
                                                           ID_Patient,Source)

## NA Fields patient_ptme
colSums(is.na(patient_ptme))

patient_ptme_u <- patient_ptme %>% select(Id,DateEnrollement,DPA,Prophylaxie,Accouchement,
                                          DateAccouchement,Allaitement,Statut,Source,ID_Patient)

## NA Fields patient_regime
colSums(is.na(patient_regime))

patient_regime_u <- patient_regime %>% select(Id,TypeRegime,NomMedicament,DateDebut,DateFin,Dosage,Source,
                                              ID_Patient)

## NA Fields patient_who_stage
colSums(is.na(patient_who_stage))

patient_who_stage_u <- patient_who_stage %>% select(Id,Numero,ValeurStade,StartStade,EndStade,Source,ID_Patient)


write_tsv(patient_demo, path="FileBox/ucsf/patient_demo.txt")
write_tsv(patient_cd4, path="FileBox/ucsf/patient_cd4.txt")
write_tsv(patient_art_ctx_event, path="FileBox/ucsf/patient_art_ctx_event.txt")
write_tsv(patient_ptme, path="FileBox/ucsf/patient_ptme.txt")
write_tsv(patient_who_stage, path="FileBox/ucsf/patient_who_stage.txt")
