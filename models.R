rm(list=ls()) ## Sletter alle variable og funktioner
cat("\014") # Sletter consol teksten

library(dplyr)
library(rio)
install.packages("splitstackshape")
library(splitstackshape)
library(tonymisc)
install.packages("pander")
library(pander)
install.packages("memisc")
library(memisc)
library("devtools")
install_github("cran/tonymisc")
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
         TotalAng =AngMaend + AngKvinder,
         AngMK = round(AngMaend / AngKvinder,2),  
        KvindeAndOpt = round(OptKvinder / TotalOpt, 2),
                   KvindeAndAng = round(AngKvinder / TotalAng, 2)) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data.2 <- data.1 %>% left_join(lon, by = "Placering") %>%
  mutate(GnsIndkomstHT = GnsIndkomst/100000)

#Expander per frequency i stedet for at vaegte - logit kan ikke regne vaegtede Marginal Effects
data.3 <- expandRows(data.2, "TotalOpt") %>%
  mutate(TotalOpt = OptMaend + OptKvinder)


#Linear Probability Model
#Linear Probability Model, Model 1
model1 <- lm(KvindeAndOpt ~ GnsIndkomstHT, data = data.3)
robust1 <- coeftest(model1, vcov = vcovHC(model1, "HC1"))    # robust; HC1 (Stata default)

#Linear Probability Model, Model 2
model2 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning, data = data.3)
coeftest(model2, vcov = vcovHC(model2, "HC1"))    # robust; HC1 (Stata default)


#Linear Probability Model, Model 3
model3 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient, data = data.3)
coeftest(model3, vcov = vcovHC(model3, "HC1"))    # robust; HC1 (Stata default)

#Linear Probability Model, Model 4
model4 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient + TotalOpt, data = data.3)
coeftest(model4, vcov = vcovHC(model4, "HC1"))    # robust; HC1 (Stata default)



#LOGIT
#Estimerer logit marginal effects for model 1
modell1<-glm(KvindeAndOpt ~ GnsIndkomstHT, data = data.3, family=binomial(link=logit), x=TRUE)
logit1 <- mfx_me(modell1) ## Create "mfx" object to trick mtable()

#Estimerer logit marginal effects for model 2
modell2<-glm(KvindeAndOpt ~ GnsIndkomstHT + Retning, data = data.3, family=binomial(link=logit), x=TRUE)
logit2 <- mfx_me(modell2) ## Create "mfx" object to trick mtable()

#Estimerer logit marginal effects for model 3
modell3<-glm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient, data = data.3, family=binomial(link=logit), x=TRUE)
logit3 <- mfx_me(modell3) ## Create "mfx" object to trick mtable()

#Estimerer logit marginal effects for model 4
modell4<-glm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient + TotalOpt, data = data.3, family=binomial(link=logit), x=TRUE)
logit4 <- mfx_me(modell4) ## Create "mfx" object to trick mtable()

OLS<- mtable("(1)"=model1, "(2)"=model2, "(3)"=model3, "(4)"=model4, summary.stats="N")
Logit<- mtable("(1)"=logit1,"(2)"=logit2,"(3)"=logit3,"(4)"=logit4, summary.stats="N") ## produces a table with nice output
c(OLS,Logit) # not that this makes sense, but ...
Table <- c("OLS"=OLS,
  "Logit"=Logit)


Table <- relabel(Table,
                      "(Intercept)" = "Konstant",
                      GnsIndkomstHT = "Indkomst i 100.000 kr.",
                      TotalOpt = "Totalt optag",
                      Kvotient = "Adgangskvotient")

