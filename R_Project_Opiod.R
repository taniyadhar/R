library(readr)
Humana <- read_csv("All Data Text File.txt")
###LOAD THE DATASET USING  "IMPORT DATASET FUNCTION" in the window  --->>> because if you use read.csv... your data types becomes 
###all factors

##Humana$Days <- as.numeric(as.character(Humana$Days))
##Humana <- Humana[!is.na(Humana$Days), ]           
##str(Humana)

patientIDs <- unique(Humana$id)
finalData <- data.frame(patientIDs)
finalData$numOfEvents <- NULL
finalData$DailyMED <- NULL
finalData$LTOT <- NULL
finalData$LTOT_V2 <- NULL
finalData$LTOT_V3 <- NULL
finalData$Supplycount <- NULL
finalData$FullyPaidClaim <- NULL
finalData$New_diagnosis_Top5 <- NULL
finalData$Surgery <- NULL
finalData$Newprovider <- NULL
finalData$Newdiagnosis_CAD <- NULL
finalData$Newdiagnosis_Diabetes <- NULL
finalData$Newdiagnosis_Hypertension <- NULL
finalData$Newdiagnosis_CPD <- NULL
finalData$Newdiagnosis_CHF <- NULL
finalData$RXClaim_Paid <- NULL
finalData$RXClaim_Rejected <- NULL
finalData$RXClaim_NewDrug <- NULL
finalData$RXClaim_FirstTimeMailOrder <- NULL
finalData$InboundCallbyMbr <- NULL
finalData$InboundCallbyOther <- NULL
finalData$InboundCallbyProv <- NULL
finalData$NumberofOpioids <- NULL
finalData$CosttoMember <- NULL
finalData$SurgeryPast30Days <- NULL
finalData$PastLowBackPain <- NULL
finalData$DrugType_IR <- NULL
finalData$DrugType_ER <- NULL
finalData$Diagnosis_OpioidDependence <- NULL
finalData$Diagnosis_Hepatitis <- NULL
finalData$Diagnosis_AlcoholDependence <- NULL
finalData$MedicalSpecialistPain <- NULL
finalData$Diagnosis_ChronicPain <- NULL
finalData$Diagnosis_Neuropathic <- NULL
finalData$DrugClass_ACE_INHIBITORS <- NULL
finalData$DrugClass_OPIOIDAGONISTS <- NULL
finalData$DrugClass_OPIOIDANTAGONISTS <- NULL
finalData$DrugClass_OPIOIDCOMBINATIONS <- NULL
finalData$DrugClass_OPIOIDPARTIALAGONISTS <- NULL
finalData$DrugClass_PERIPHERALOPIOIDRECEPTORANTAGONISTS <- NULL
finalData$DrugClass_Location_OpioidCrisis <- NULL
finalData$DrugGroupDescription_Psych <- NULL
finalData$Generic_Oxycodone <- NULL
finalData$Specialty_GeneralPractice <- NULL
finalData$Specialty_PhysicalMedicine <- NULL
finalData$Specialty_OrthopaedicSurgery <- NULL
finalData$Generic_MORPHINE <- NULL
finalData$Diagnosis_ARTHRITIS <- NULL
finalData$Diagnosis_MALIGNANTNEOPLASM <- NULL
finalData$Diagnosis_ROTATORCUFFTEAR <- NULL
finalData$BrandName_FENTANYL <- NULL
finalData$BrandName_OXYCONTIN <- NULL
finalData$BrandName_DEMEROL <- NULL

###replace 10 with length(patientIDs) in the final version
for(i in 1:200){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  tempanchordate <- temp[temp$Days==0,]
  tempAfter <- temp[temp$Days>=0 & temp$Days<=180,]
  tempbetween <- temp[temp$Days>=-30 & temp$Days<=0,]
  ###you can add tempAfter <- temp[temp$Days>0,]
  finalData$numOfEvents[i] <-  nrow(tempBefore)
  ifelse(length(unique(tempanchordate$`GPI Drug Group8 ID`[!is.na(tempanchordate$MME)]))>1,finalData$DailyMED[i] <-sum(tempanchordate$MME, na.rm=TRUE), finalData$DailyMED[i] <-mean(tempanchordate$MME, na.rm=TRUE))
  Count <- tempanchordate[!duplicated(tempanchordate[25:27]),]
  finalData$Supplycount[i] <-  max(Count$PAY_DAY_SUPPLY_CNT [!is.na(Count$PAY_DAY_SUPPLY_CNT)])
  finalData$FullyPaidClaim[i] <-  length(which(tempBefore$event_descr == "Fully Paid Claim"))
  finalData$New_diagnosis_Top5[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Top 5"))
  finalData$Surgery[i] <-  length(which(tempBefore$event_descr == "Surgery"))
  finalData$Newprovider[i] <-  length(which(tempBefore$event_descr == "New provider"))
  finalData$Newdiagnosis_CAD[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CAD"))
  finalData$Newdiagnosis_Diabetes[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Diabetes"))
  finalData$Newdiagnosis_Hypertension[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Hypertension"))
  finalData$Newdiagnosis_CPD[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CPD"))
  finalData$Newdiagnosis_CHF[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CHF"))
  finalData$RXClaim_Paid[i] <-  length(which(tempBefore$event_descr == "RX Claim - Paid"))
  finalData$RXClaim_Rejected[i] <-  length(which(tempBefore$event_descr == "RX Claim - Rejected"))
  finalData$RXClaim_NewDrug[i] <-  length(which(tempBefore$event_descr == "RX Claim - New Drug"))
  finalData$RXClaim_FirstTimeMailOrder[i] <-  length(which(tempBefore$event_descr == "RX Claim - First Time Mail Order"))
  finalData$InboundCallbyMbr[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Mbr"))
  finalData$InboundCallbyOther[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Other"))
  finalData$InboundCallbyProv[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Prov"))
  finalData$NumberofOpioids[i] <-  length(Count$PAY_DAY_SUPPLY_CNT [!is.na(Count$PAY_DAY_SUPPLY_CNT)])
  ifelse(length(unique(tempanchordate$`GPI Drug Group8 ID`[!is.na(tempanchordate$MME)]))>1,finalData$CosttoMember[i] <-sum(tempanchordate$`Member Responsible Amount`[!is.na(tempanchordate$MME)]), finalData$CosttoMember[i] <-max(tempanchordate$`Member Responsible Amount`[!is.na(tempanchordate$MME)]))
  r <- grepl("LOW BACK PAIN|RADICULOPATHY|SPONDYLOSIS|SPINE", tempBefore$Diagnosis)
  finalData$PastLowBackPain[i] <- ifelse(sum(r[r==TRUE])>=1, 1, 0)
  ifelse(nrow(tempbetween[tempbetween$event_descr == "Surgery",])>=1, finalData$SurgeryPast30Days[i] <- 1, finalData$SurgeryPast30Days[i] <- 0)
  b <- grepl("IR", tempanchordate$DRUG_TYPE)
  finalData$DrugType_IR[i] <- ifelse(sum(b[b==TRUE])>=1, 1, 0)
  a <- grepl("ER", tempanchordate$DRUG_TYPE)
  finalData$DrugType_ER[i] <- ifelse(sum(a[a==TRUE])>=1, 1, 0)
  c <- grepl("OPIOID DEPENDENCE|OPIOID ABUSE|OPIOID TYPE DEPENDENCE|OPIOID USE|POISONING BY OTHER OPIOIDS|COMBINATIONS OF OPIOID TYPE DRUG|ADVERSE EFFECT OF OTHER OPIOIDS|NONDEPENDENT OPIOID ABUSE", tempBefore$Diagnosis)
  finalData$Diagnosis_OpioidDependence[i] <- ifelse(sum(c[c==TRUE])>=1, 1, 0)
  d <- grepl("HEPATITIS", tempBefore$Diagnosis)
  finalData$Diagnosis_Hepatitis[i] <- ifelse(sum(d[d==TRUE])>=1, 1, 0)
  e <- grepl("ACUTE ALCOHOLIC INTOXICATION|ALCOHOL ABUSE|ALCOHOL DEPENDENCE|ALCOHOL USE|ALCOHOL WITHDRAWAL|ALCOHOLIC CIRRHOSIS|ALCOHOLIC FATTY LIVER|ALCOHOLIC GASTRITIS|ALCOHOLIC HEPATIC FAILURE|ALCOHOLIC LIVER DISEASE|ALCOHOLIC POLYNEUROPATHY|ALCOHOL-INDUCED|DEGENERATION OF NERVOUS SYSTEM DUE TO ALCOHOL|ENCOUNTER FOR BLOOD-ALCOHOL AND BLOOD-DRUG TEST|NONDEPENDENT ALCOHOL ABUSE|OTHER AND UNSPECIFIED ALCOHOL DEPENDENCE|TOXIC EFFECT OF ETHYL ALCOHOL|TOXIC EFFECT OF UNSPECIFIED ALCOHOL|UNSPECIFIED ALCOHOLIC LIVER DAMAGE|ALCOHOLIC FIBROSIS|ALCOHOLIC HEPATITIS|UNSPECIFIED ALCOHOL-INDUCED MENTAL DISORDERS|ALCOHOLIC CARDIOMYOPATHY|ACUTE ALCOHOLIC HEPATITIS|PERSONAL HISTORY OF ALCOHOLISM", tempBefore$Diagnosis)
  finalData$Diagnosis_AlcoholDependence[i] <- ifelse(sum(e[e==TRUE])>=1, 1, 0)
  f <- grepl("Anesthesiology Pain Medicine|Pain Medicine Interventional Pain Med|Pain Medicine Pain Management|Physical Medicine & Rehab Pain Medicine|Psychiatry & Neurology Pain Medicine", tempanchordate$Specialty)
  finalData$MedicalSpecialistPain[i] <- ifelse(sum(f[f==TRUE])>=1, 1, 0)
  g <- grepl("CHRONIC PAIN", tempBefore$Diagnosis)
  finalData$Diagnosis_ChronicPain[i] <- ifelse(sum(g[g==TRUE])>=1, 1, 0)
  h <- grepl("TYPE 2 DIABETES MELLITUS WITH DIABETIC NEUROPATHIC ARTHROPATHY", tempBefore$Diagnosis)
  finalData$Diagnosis_Neuropathic[i] <- ifelse(sum(h[h==TRUE])>=1, 1, 0)
  j <- grepl("ACE INHIBITORS", tempBefore$`GPI Drug Class Description`)
  finalData$DrugClass_ACE_INHIBITORS[i] <- ifelse(sum(j[j==TRUE])>=1, 1, 0)
  k <- grepl("OPIOID AGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDAGONISTS[i] <- ifelse(sum(k[k==TRUE])>=1, 1, 0)
  l <- grepl("OPIOID ANTAGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDANTAGONISTS[i] <- ifelse(sum(l[l==TRUE])>=1, 1, 0)
  m <- grepl("OPIOID COMBINATIONS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDCOMBINATIONS[i] <- ifelse(sum(m[m==TRUE])>=1, 1, 0)
  n <- grepl("OPIOID PARTIAL AGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDPARTIALAGONISTS[i] <- ifelse(sum(n[n==TRUE])>=1, 1, 0)
  o <- grepl("PERIPHERAL OPIOID RECEPTOR ANTAGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_PERIPHERALOPIOIDRECEPTORANTAGONISTS[i] <- ifelse(sum(o[o==TRUE])>=1, 1, 0)
  p <- grepl("OH|KY", tempBefore$Location)
  finalData$DrugClass_Location_OpioidCrisis[i] <- ifelse(sum(p[p==TRUE])>=1, 1, 0)
  q <- grepl("PSYCH-DEP|PSYCH-ANX|PSYCH", tempBefore$`Drug Group Description`)
  finalData$DrugGroupDescription_Psych[i] <- ifelse(sum(q[q==TRUE])>=1, 1, 0)
  s <- grepl("OXYCODONE", tempanchordate$`Generic Name`)
  finalData$Generic_Oxycodone[i] <- ifelse(sum(s[s==TRUE])>=1, 1, 0)
  t <- grepl("General Practice", tempanchordate$Specialty)
  finalData$Specialty_GeneralPractice[i] <- ifelse(sum(t[t==TRUE])>=1, 1, 0)
  u <- grepl("Physical Medicine", tempanchordate$Specialty)
  finalData$Specialty_PhysicalMedicine[i] <- ifelse(sum(u[u==TRUE])>=1, 1, 0)
  v <- grepl("Orthopaedic Surgery", tempanchordate$Specialty)
  finalData$Specialty_OrthopaedicSurgery[i] <- ifelse(sum(v[v==TRUE])>=1, 1, 0)
  w <- grepl("MORPHINE", tempanchordate$`Generic Name`)
  finalData$Generic_MORPHINE[i] <- ifelse(sum(w[w==TRUE])>=1, 1, 0)
  x <- grepl("ARTHRITIS", tempBefore$Diagnosis)
  finalData$Diagnosis_ARTHRITIS[i] <- ifelse(sum(x[x==TRUE])>=1, 1, 0)
  y <- grepl("MALIGNANT NEOPLASM", tempBefore$Diagnosis)
  finalData$Diagnosis_MALIGNANTNEOPLASM[i] <- ifelse(sum(y[y==TRUE])>=1, 1, 0)
  z <- grepl("ROTATOR CUFF TEAR", tempBefore$Diagnosis)
  finalData$Diagnosis_ROTATORCUFFTEAR[i] <- ifelse(sum(z[z==TRUE])>=1, 1, 0)
  aa <- grepl("FENTANYL", tempanchordate$`Brand Name`)
  finalData$BrandName_FENTANYL[i] <- ifelse(sum(aa[aa==TRUE])>=1, 1, 0)
  bb <- grepl("OXYCONTIN", tempanchordate$`Brand Name`)
  finalData$BrandName_OXYCONTIN[i] <- ifelse(sum(bb[bb==TRUE])>=1, 1, 0)
  cc <- grepl("DEMEROL", tempanchordate$`Brand Name`)
  finalData$BrandName_DEMEROL[i] <- ifelse(sum(cc[cc==TRUE])>=1, 1, 0)
  ###Target Variable 
  Pay <- tempAfter[!duplicated(tempAfter[25:27]),]
  Supply <- sum(Pay$PAY_DAY_SUPPLY_CNT[!is.na(Pay$PAY_DAY_SUPPLY_CNT)])
  ifelse(Supply >= 162, finalData$LTOT[i] <- 1, finalData$LTOT[i] <- 0)
  ifelse(Supply >= 172, finalData$LTOT_V2[i] <- 1, finalData$LTOT_V2[i] <- 0)
  ifelse(Supply >= 175, finalData$LTOT_V3[i] <- 1, finalData$LTOT_V3[i] <- 0)
}

###replace 10 with length(patientIDs) in the final version
for(i in 1:length(patientIDs)){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  ###you can add tempAfter <- temp[temp$Days>0,]
  finalData$numOfEvents[i] <-  nrow(tempBefore)
}

##DONE

for(i in 1:27){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  ifelse(length(unique(tempanchordate$`GPI Drug Group8 ID`[!is.na(tempanchordate$MME)]))>1,finalData$DailyMED[i] <-sum(tempanchordate$MME, na.rm=TRUE), finalData$DailyMED[i] <-mean(tempanchordate$MME, na.rm=TRUE))
}
###Get the MME value, add if more than one opioid is perscribed - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempAfter <- temp[temp$Days>=0 & temp$Days<=180,]
  Pay <- tempAfter[!duplicated(tempAfter[25:27]),]
  Supply <- sum(Pay$PAY_DAY_SUPPLY_CNT[!is.na(Pay$PAY_DAY_SUPPLY_CNT)])
  ifelse(Supply >= 162, finalData$LTOT[i] <- 1, finalData$LTOT[i] <- 0)
}   
###Creating target varaible LTOT - DONE

for(i in 1:27){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  Count <- tempanchordate[!duplicated(tempanchordate[25:27]),]
  finalData$Supplycount[i] <-  max(Count$PAY_DAY_SUPPLY_CNT [!is.na(Count$PAY_DAY_SUPPLY_CNT)])
}
###Creating number of days inital supply will last - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$FullyPaidClaim[i] <-  length(which(tempBefore$event_descr == "Fully Paid Claim"))
}
###Number of Fully Paid Claims - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$New_diagnosis_Top5[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Top 5"))
}
###Number of New diagnosis - Top 5 - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Surgery[i] <-  length(which(tempBefore$event_descr == "Surgery"))
}
####Surgery - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newprovider[i] <-  length(which(tempBefore$event_descr == "New provider"))
}
###New Provider - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newdiagnosis_CAD[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CAD"))
}
###New diagnosis - CAD - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newdiagnosis_Diabetes[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Diabetes"))
}
###New diagnosis - Diabetes - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newdiagnosis_Hypertension[i] <-  length(which(tempBefore$event_descr == "New diagnosis - Hypertension"))
}
###New diagnosis - Hypertension - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newdiagnosis_CPD[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CPD"))
}
###New diagnosis - CPD - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$Newdiagnosis_CHF[i] <-  length(which(tempBefore$event_descr == "New diagnosis - CHF"))
}
###New diagnosis - CHF - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$RXClaim_Paid[i] <-  length(which(tempBefore$event_descr == "RX Claim - Paid"))
}
###RX Claim - Paid - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$RXClaim_Rejected[i] <-  length(which(tempBefore$event_descr == "RX Claim - Rejected"))
}
###RX Claim - Rejected - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$RXClaim_NewDrug[i] <-  length(which(tempBefore$event_descr == "RX Claim - New Drug"))
}
###RX Claim - New Drug - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$RXClaim_FirstTimeMailOrder[i] <-  length(which(tempBefore$event_descr == "RX Claim - First Time Mail Order"))
}
###RX Claim - First Time Mail Order - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$InboundCallbyMbr[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Mbr"))
}
###Inbound Call by Mbr - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$InboundCallbyOther[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Other"))
}
###Inbound Call by Other - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  finalData$InboundCallbyProv[i] <-  length(which(tempBefore$event_descr == "Inbound Call by Prov"))
}
###Inbound Call by Prov - DONE

for(i in 1:27){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  Count <- tempanchordate[!duplicated(tempanchordate[25:27]),]
  finalData$NumberofOpioids[i] <-  length(Count$PAY_DAY_SUPPLY_CNT [!is.na(Count$PAY_DAY_SUPPLY_CNT)])
}
###Number of opioids a person was initially prescribed - DONE

for(i in 1:27){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  ifelse(length(unique(tempanchordate$`GPI Drug Group8 ID`[!is.na(tempanchordate$MME)]))>1,finalData$CosttoMember[i] <-sum(tempanchordate$`Member Responsible Amount`[!is.na(tempanchordate$MME)]), finalData$CosttoMember[i] <-max(tempanchordate$`Member Responsible Amount`[!is.na(tempanchordate$MME)]))
}
###Total dollar amount patient pays for opioid(s) - DONE

for(i in 1:27){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  r <- grepl("LOW BACK PAIN|RADICULOPATHY|SPONDYLOSIS", tempBefore$Diagnosis)
  finalData$PastLowBackPain[i] <- ifelse(sum(r[r==TRUE])>=1, 1, 0)
}
###Number of lower back pain diagnosis - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempbetween <- temp[temp$Days>=-30 & temp$Days<=0,]
  ifelse(nrow(tempbetween[tempbetween$event_descr == "Surgery",])>=1, finalData$SurgeryPast30Days[i] <- 1, finalData$SurgeryPast30Days[i] <- 0)
}
####Had surgery within 30 days of day zero - DONE

for(i in 1:110){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  b <- grepl("IR", tempanchordate$DRUG_TYPE)
  finalData$DrugType_IR[i] <- ifelse(sum(b[b==TRUE])>=1, 1, 0)
}
####Prescribed an IR (Immediate Release) Opioid - DONE 

for(i in 1:85){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  a <- grepl("ER", tempanchordate$DRUG_TYPE)
  finalData$DrugType_ER[i] <- ifelse(sum(a[a==TRUE])>=1, 1, 0)
}
####Prescribed an ER (Extended Release) Opioid - DONE

for(i in 1:40){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  c <- grepl("OPIOID DEPENDENCE|OPIOID ABUSE|OPIOID TYPE DEPENDENCE|OPIOID USE|POISONING BY OTHER OPIOIDS|COMBINATIONS OF OPIOID TYPE DRUG|ADVERSE EFFECT OF OTHER OPIOIDS|NONDEPENDENT OPIOID ABUSE", tempBefore$Diagnosis)
  finalData$Diagnosis_OpioidDependence[i] <- ifelse(sum(c[c==TRUE])>=1, 1, 0)
}
###Diagnosis of Opioid dependence before Day Zero - DONE

for(i in 1:200){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  d <- grepl("HEPATITIS", tempBefore$Diagnosis)
  finalData$Diagnosis_Hepatitis[i] <- ifelse(sum(d[d==TRUE])>=1, 1, 0)
}
###Diagnosis of Hepatitis before or at Day Zero - DONE

for(i in 1:40){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore <- temp[temp$Days<=0,]
  e <- grepl("ACUTE ALCOHOLIC INTOXICATION|ALCOHOL ABUSE|ALCOHOL DEPENDENCE|ALCOHOL USE|ALCOHOL WITHDRAWAL|ALCOHOLIC CIRRHOSIS|ALCOHOLIC FATTY LIVER|ALCOHOLIC GASTRITIS|ALCOHOLIC HEPATIC FAILURE|ALCOHOLIC LIVER DISEASE|ALCOHOLIC POLYNEUROPATHY|ALCOHOL-INDUCED|DEGENERATION OF NERVOUS SYSTEM DUE TO ALCOHOL|ENCOUNTER FOR BLOOD-ALCOHOL AND BLOOD-DRUG TEST|NONDEPENDENT ALCOHOL ABUSE|OTHER AND UNSPECIFIED ALCOHOL DEPENDENCE|TOXIC EFFECT OF ETHYL ALCOHOL|TOXIC EFFECT OF UNSPECIFIED ALCOHOL|UNSPECIFIED ALCOHOLIC LIVER DAMAGE|ALCOHOLIC FIBROSIS|ALCOHOLIC HEPATITIS|UNSPECIFIED ALCOHOL-INDUCED MENTAL DISORDERS|ALCOHOLIC CARDIOMYOPATHY|ACUTE ALCOHOLIC HEPATITIS|PERSONAL HISTORY OF ALCOHOLISM", tempBefore$Diagnosis)
  finalData$Diagnosis_AlcoholDependence[i] <- ifelse(sum(e[e==TRUE])>=1, 1, 0)
}
###Diagnosis of alcohol dependence before or at Day Zero - DONE

for(i in 1:26){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  f <- grepl("Anesthesiology Pain Medicine|Pain Medicine Interventional Pain Med|Pain Medicine Pain Management|Physical Medicine & Rehab Pain Medicine|Psychiatry & Neurology Pain Medicine", tempanchordate$Specialty)
  finalData$MedicalSpecialistPain[i] <- ifelse(sum(f[f==TRUE])>=1, 1, 0)
}
###Prescribing Specialist _ Specialist Medical Specialist Pain - DONE

for(i in 1:200){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  g <- grepl("CHRONIC PAIN", tempBefore$Diagnosis)
  finalData$Diagnosis_ChronicPain[i] <- ifelse(sum(g[g==TRUE])>=1, 1, 0)
}
###Diagnosis of Chronic Pain - DONE

for(i in 1:682){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  h <- grepl("TYPE 2 DIABETES MELLITUS WITH DIABETIC NEUROPATHIC ARTHROPATHY", tempBefore$Diagnosis)
  finalData$Diagnosis_Neuropathic[i] <- ifelse(sum(h[h==TRUE])>=1, 1, 0)
}
###Diagnosis of Neuropathic before or at Day Zero - DONE

for(i in 1:85){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  j <- grepl("ACE INHIBITORS", tempBefore$`GPI Drug Class Description`)
  finalData$DrugClass_ACE_INHIBITORS[i] <- ifelse(sum(j[j==TRUE])>=1, 1, 0)
}
####Prescribed an ACE INHIBITOR - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  k <- grepl("OPIOID AGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDAGONISTS[i] <- ifelse(sum(k[k==TRUE])>=1, 1, 0)
}
####Prescribed an OPIOID AGONISTS at day zero -DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  l <- grepl("OPIOID ANTAGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDANTAGONISTS[i] <- ifelse(sum(l[l==TRUE])>=1, 1, 0)
}
####Prescribed an OPIOID ANTAGONISTS at day zero - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  m <- grepl("OPIOID COMBINATIONS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDCOMBINATIONS[i] <- ifelse(sum(m[m==TRUE])>=1, 1, 0)
}
####Prescribed an OPIOID COMBINATIONS at day zero - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  n <- grepl("OPIOID PARTIAL AGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_OPIOIDPARTIALAGONISTS[i] <- ifelse(sum(n[n==TRUE])>=1, 1, 0)
}
####Prescribed an OPIOID PARTIAL AGONISTS at day zero - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  o <- grepl("PERIPHERAL OPIOID RECEPTOR ANTAGONISTS", tempanchordate$`GPI Drug Class Description`)
  finalData$DrugClass_PERIPHERALOPIOIDRECEPTORANTAGONISTS[i] <- ifelse(sum(o[o==TRUE])>=1, 1, 0)
}
####Prescribed an PERIPHERAL OPIOID RECEPTOR ANTAGONISTS at day zero - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  p <- grepl("OH|KY", tempBefore$Location)
  finalData$DrugClass_Location_OpioidCrisis[i] <- ifelse(sum(p[p==TRUE])>=1, 1, 0)
}
####Location - Large Opioid Crisis - KY and OH --- Should we split this up? - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  q <- grepl("PSYCH-DEP|PSYCH-ANX|PSYCH", tempBefore$`Drug Group Description`)
  finalData$DrugGroupDescription_Psych[i] <- ifelse(sum(q[q==TRUE])>=1, 1, 0)
}
####On Psych Medication - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  s <- grepl("OXYCODONE", tempanchordate$`Generic Name`)
  finalData$Generic_Oxycodone[i] <- ifelse(sum(s[s==TRUE])>=1, 1, 0)
}
####Prescribed generic name - Oxycodone - DONE

for(i in 1:12){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  t <- grepl("General Practice", tempanchordate$Specialty)
  finalData$Specialty_GeneralPractice[i] <- ifelse(sum(t[t==TRUE])>=1, 1, 0)
}
###Prescribing Physician_General Practice - DONE

for(i in 1:15){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  u <- grepl("Physical Medicine", tempanchordate$Specialty)
  finalData$Specialty_PhysicalMedicine[i] <- ifelse(sum(u[u==TRUE])>=1, 1, 0)
}
###Prescribing Physician_Physical Medicine - DONE

for(i in 1:15){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  v <- grepl("Orthopaedic Surgery", tempanchordate$Specialty)
  finalData$Specialty_OrthopaedicSurgery[i] <- ifelse(sum(v[v==TRUE])>=1, 1, 0)
}
###Prescribing Physician_Orthopaedic Surgery - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  w <- grepl("MORPHINE", tempanchordate$`Generic Name`)
  finalData$Generic_MORPHINE[i] <- ifelse(sum(w[w==TRUE])>=1, 1, 0)
}
####Prescribed generic name - Morphine - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  x <- grepl("ARTHRITIS", tempBefore$Diagnosis)
  finalData$Diagnosis_ARTHRITIS[i] <- ifelse(sum(x[x==TRUE])>=1, 1, 0)
}
###Diagnosis of ARTHRITIS - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  y <- grepl("MALIGNANT NEOPLASM", tempBefore$Diagnosis)
  finalData$Diagnosis_MALIGNANTNEOPLASM[i] <- ifelse(sum(y[y==TRUE])>=1, 1, 0)
}
###Diagnosis of MALIGNANT NEOPLASM - DONE

for(i in 1:10){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempBefore<- temp[temp$Days<=0,]
  z <- grepl("ROTATOR CUFF TEAR", tempBefore$Diagnosis)
  finalData$Diagnosis_ROTATORCUFFTEAR[i] <- ifelse(sum(z[z==TRUE])>=1, 1, 0)
}
###Diagnosis of ROTATOR CUFF TEAR - DONE

for(i in 1:15){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  aa <- grepl("FENTANYL", tempanchordate$`Brand Name`)
  finalData$Specialty_FENTANYL[i] <- ifelse(sum(aa[aa==TRUE])>=1, 1, 0)
}
###BrandName_FENTANYL - DONE

for(i in 1:15){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  bb <- grepl("OXYCONTIN", tempanchordate$`Brand Name`)
  finalData$Specialty_OXYCONTIN[i] <- ifelse(sum(bb[bb==TRUE])>=1, 1, 0)
}
###BrandName_OXYCONTIN - DONE

for(i in 1:15){
  temp <- Humana[Humana$id%in%patientIDs[[i]],]
  tempanchordate <- temp[temp$Days==0,]
  cc <- grepl("DEMEROL", tempanchordate$`Brand Name`)
  finalData$Specialty_DEMEROL[i] <- ifelse(sum(cc[cc==TRUE])>=1, 1, 0)
}
###BrandName_DEMEROL - DONE