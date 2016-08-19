rm(list=ls()) ## Sletter alle variable og funktioner
cat("\014") # Sletter consol teksten
library(dplyr)
library(rio)

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
                   KvindeAndAng = round(AngKvinder / TotalAng, 2)
           ) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data.2 <- data.1 %>% left_join(lon, by = "Placering")

install.packages("mfx")
library(mfx)
install.packages("splitstackshape")
library(splitstackshape)
install.packages("stargazer")
library(stargazer)
install.packages("https://cran.r-project.org/src/contrib/Archive/tonymisc/tonymisc_1.1.0.tar.gz")
library(tonymisc)
install.packages("pander")
library(pander)
install.packages("memisc")
library(memisc)
library("devtools")
install_github("cran/tonymisc")
library(tonymisc)


#Expander per frequency i stedet for at vaegte - logit kan ikke regne vaegtede Marginal Effects
data.3 <- expandRows(data.2, "TotalOpt") %>%
  mutate(TotalOpt = OptMaend + OptKvinder,
         GnsIndkomstHT = GnsIndkomst/100000,)

#Linear Probability Model
#Linear Probability Model, Model 1
model1 <- lm(KvindeAndOpt ~ GnsIndkomstHT, data = data.3)
summary(model1)


#Linear Probability Model, Model 2
model2 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning, data = data.3)
summary(model2)

#Linear Probability Model, Model 3
model3 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient, data = data.3)
summary(model3)

#Linear Probability Model, Model 4
model4 <- lm(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient + TotalOpt, data = data.3)
summary(model4)


#LOGIT
#Estimerer logit marginal effects for model 1
modell1 <- logitmfx(KvindeAndOpt ~ GnsIndkomstHT, data = data.3, atmean = TRUE, robust = TRUE)

#Estimerer logit marginal effects for model 2
modell2 <- logitmfx(KvindeAndOpt ~ GnsIndkomstHT + Retning,data = data.3, atmean = TRUE, robust = TRUE)

#Estimerer logit marginal effects for model 3
modell3 <- logitmfx(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient,data = data.3, atmean = TRUE, robust = TRUE)

#Estimerer logit marginal effects for model 4
modell4 <- logitmfx(KvindeAndOpt ~ GnsIndkomstHT + Retning + Kvotient + TotalOpt,data = data.3, atmean = TRUE, robust = TRUE)

#Skriver output tabel
stargazer(type="html",model1, model2, model3, model4, title="Results", align=TRUE)

stargazer(type="html",modell1, modell2, modell3, modell4, title="Results", align=TRUE)

install.packages("xtable")
library(xtable)

newobject<-xtable(modell1)
print.xtable(newobject, type="html", file="filename.html")



mtable12 <- mtable('Model 1' = model1,
                    'Model 2' = modell1),
                    summary.stats = c('R-squared','F','p','N'))

pander(mtable12)

#GAMLE IDEER
?glm
modw<-glm(KvindeAndOpt ~ GnsIndkomst, family=binomial(link=logit), x=TRUE)

logit1 <- mfx_me(modw) ## Create "mfx" object to trick mtable()
mtable(modw, logit1) ## produces a table with nice output

#maBina(w=modw, x.mean = TRUE, rev.dum = TRUE, digits = 9)

#modd<-glm(KvindeAndOpt ~ GnsIndkomst, family=binomial(link=logit), data = data.4, x=TRUE)
#maBina(w=modd, x.mean = TRUE, rev.dum = TRUE, digits = 9)





