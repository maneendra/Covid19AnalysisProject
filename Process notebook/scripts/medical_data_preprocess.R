library(sp)
library(gsubfn)
library(tidyr)
library(magrittr)
library(data.table)
library(tm)
library(NLP)
library(stringr)
library(dplyr)
library(ggthemes)
library(wordcloud2)
library(plotly)
library(here)
library(ggplot2)
library(stats)
library(graphics)
library(plotly)
library(ggthemes)
med <- read.csv("./data/2020-05-27_WOLFRAM_PATIENT_MEDICAL_DATASET_NOVEL_CORONAVIRUS.csv")
#Preprocessing age variable
age <- function(x){
  x <- as.numeric(x)
  if(x %in% 1:100){
    return (x)
  }else {
    x <- NA
    return (x)
  }
}
med$Age <- sapply(med$Age,age)
med$Age[is.na(med$Age)] <- as.integer(mean(med$Age, na.rm = T))

# 1st peprocessing function : removes punctuations
prep.punctuationRemoval <- function(x){
  x <- as.character(x)
  x <- removePunctuation(x)
  x <- str_split(x,boundary("word"),simplify = T)[2]
  return(x)
}
#2nd preprocessing function : removes unwanted brackets
prep.bracketRemoval <- function(x){
  x <- str_extract(x,"[{](.*)[}]")
  y <- str_remove(x,"[{]")
  z <- str_remove(y,"[}]")
  a <- as.factor(z)
  return(a)
}
#3rd preprocessing funtion  : removes missing values
prep.removeMissingVal <- function(x){
  x = as.character(x)
  if( x == 'Missing["NotAvailable"]'){
    x <- NA
  }
  return(x)
}
#preprocessing Sex
med$Sex <- sapply(med$Sex,prep.punctuationRemoval)
med$Sex[is.na(med$Sex)] <- tail(names(sort(table(med$Sex))), 1)
med$Sex <- as.factor(med$Sex)
#preprocessing city
med$City <- sapply(med$City,prep.punctuationRemoval)
med$City <- as.factor(med$City)
#preprocessing country
med$Country <- sapply(med$Country ,prep.punctuationRemoval)
med$Country <- as.factor(med$Country)
#preprocessing GeoPosition
med$GeoPosition <- sapply(med$GeoPosition ,prep.bracketRemoval)
#preprocessing DateOfonsetSymptoms
med$DateOfOnsetSymptoms <- sapply(med$DateOfOnsetSymptoms, prep.bracketRemoval)
med$DateOfOnsetSymptoms <- as.Date(med$DateOfOnsetSymptoms, format = "%Y,%m,%d")
#preprocessing DateOfAdmissionHospital
med$DateOfAdmissionHospital <- sapply(med$DateOfAdmissionHospital, prep.bracketRemoval)
med$DateOfAdmissionHospital <- as.Date(med$DateOfAdmissionHospital, format = "%Y,%m,%d")
#preprocessing DateOfConfirmation
med$DateOfConfirmation <- sapply(med$DateOfConfirmation,prep.bracketRemoval)
med$DateOfConfirmation <- as.Date(med$DateOfConfirmation, format = "%Y,%m,%d")
#preprocessing Symptoms
med$Symptoms <- sapply(med$Symptoms , prep.bracketRemoval)
med$Symptoms <- as.factor(med$Symptoms)
#preprocess LivesInWuhan
med$LivesInWuhan <- sapply(med$LivesInWuhan ,prep.removeMissingVal)
med$LivesInWuhan <- as.factor(med$LivesInWuhan)
#preprocess Chronic Disease
med$ChronicDiseases <- sapply(med$ChronicDiseases , prep.bracketRemoval)
med$ChronicDiseases <- as.factor(med$ChronicDiseases)
#preprocess DischargeQ
med$DischargedQ <- sapply(med$DischargedQ, prep.removeMissingVal)
med$DischargedQ <- as.factor(med$DischargedQ)
#preprocess DeathQ
med$DeathQ <- sapply(med$DeathQ, prep.removeMissingVal)
med$DeathQ <- as.factor(med$DeathQ)
#preprocess DateOfdeath
med$DateOfDeath <- sapply(med$DateOfDeath , prep.bracketRemoval)
med$DateOfDeath <- as.Date(med$DateOfDeath, format = "%Y,%m,%d")
#preprocess DateOfDischarge
med$DateOfDischarge <- sapply(med$DateOfDischarge , prep.bracketRemoval)
med$DateOfDischarge <- as.Date(med$DateOfDischarge, format = "%Y,%m,%d")
#Removing unused variables
med$LivesInWuhanComment <- NULL 
med$ReportedMarketExposure <- NULL
med$ReportedMarketExposureComment <- NULL 
med$SequenceAvailable <- NULL
med$TravelHistoryDates <- NULL
med$TravelHistoryLocation <- NULL
med$AdministrativeDivision <- NULL
