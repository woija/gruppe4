library(dplyr)
library(rio)
#install.packages("splitstackshape")
library(splitstackshape)
library(tonymisc)
#install.packages("pander")
library(pander)
#install.packages("memisc")
library(memisc)
library("devtools")
#install_github("cran/tonymisc")
library(tonymisc)
library("rio")
library("magrittr")
library("dplyr")


setwd("/Users/Mathias/OneDrive/KU/Social Data Science/gruppe4" ) ## Skriv stien

data <- import("optagelse3.csv") # l??ser csv-filen 
lon <- import("londata.csv") # l??ser csv-filen

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 

data.1 <- data %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         TotalOptH = TotalOpt/100,
         TotalAng =AngMaend + AngKvinder,
         AngMK = round(AngMaend / AngKvinder,2),  
         KvindeAndOpt = round(OptKvinder / TotalOpt, 2),
         KvindeAndAng = round(AngKvinder / TotalAng, 2)) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data.2 <- data.1 %>% left_join(lon, by = "Placering") %>%
  mutate(GnsIndkomstHT = GnsIndkomst/100000)

data3 <- data.2 %>% filter(!is.na(GnsIndkomstHT))


KvindeAndOpt <- sum(data3$OptKvinder, na.rm = TRUE)/sum(data3$TotalOpt, na.rm = TRUE)
KvindeAndAng <- sum(data3$AngKvinder, na.rm = TRUE)/sum(data3$TotalAng, na.rm = TRUE)

Optagne=KvindeAndOpt
Angsoegere=KvindeAndAng
Kategori="Andel kvinder"
andel= data.frame(Kategori, Angsoegere, Optagne)
andel

save(andel, file = "andel")
