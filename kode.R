rm(list=ls()) ## Sletter alle variable og funktioner
cat("\014") # Sletter consol teksten

install.packages("freqweights")
library("freqweights")
library("rio")
library("dplyr")
library("ggplot2")
setwd("/Users/simonharmat/Dropbox/Studie/Data_science/Gruppe4" ) ## Skriv stien


data <- import("optagelse3.csv") # læser csv-filen 
lon <- import("londata1.csv") # læser csv-filen
?import

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 

data.1 <- data %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         KvindeAndOpt = round(OptKvinder /(OptMaend + OptKvinder),2),
         AngMK = round(AngMaend / AngKvinder,2),
                       TotalAng = AngMaend + AngKvinder,
         KvindeAndAng = round(AngKvinder /(AngMaend + AngKvinder),2)) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data2 <- data.1 %>% left_join(lon, by = "Placering")

data3 <-  data2 %>%
  filter(!is.na(GnsIndkomst)) %>%
  mutate(GnsIndT = GnsIndkomst/100000,
         )

fit <- lmfreq(KvindeAndOpt ~ GnsIndT + Kvotient, data3, freq = "TotalOpt")
summary(fit)
data3 <- data2 %>%
  train <-  data2 %>% 
  filter(Aar == 2013 | Aar == 2014 | Aar == 2015) %>%
  select(Aar, InstNr, InstNavn, AngMaend, AngKvinder, Kvotient, KvindeAndOpt, GnsIndkomst, Retning)

test <-  data2 %>% 
  filter(Aar != 2013 | Aar != 2014 | Aar != 2015) %>%
  select(Aar, InstNr, InstNavn, AngMaend, AngKvinder, Kvotient, KvindeAndOpt, GnsIndkomst, Retning)

install.packages("rpart")
library("rpart")
set.seed(1)
model = rpart(KvindeAndOpt ~ ., data = train,
              method = "class")
KvindeAndOpt = predict(model, newdata = test,
                  type = "class")


names(data3)

  
  
  fit2 <- lm(KvindeAndOpt ~ poly(GnsIndkomst, 9)  , data3)
summary(fit2)

 fit <- glm(KvindeAndOpt ~ poly(GnsIndkomst, 9), data3, family = binomial(link="logit"))
summary(fit)



p <- ggplot(data3, aes(GnsIndkomst, AngKvinder, ))
p + geom_point() + 
  #scale_colour_hue(l=50) +   
  geom_smooth(method=lm,    se=TRUE,      fullrange=TRUE)



data4 <- data3 %>%              
       group_by(Aar, Retning)  %>% 
       summarise(sum.AngMaend = sum(AngMaend, na.rm = TRUE), sum.AngKvinder = sum(AngKvinder, na.rm = TRUE) )  

ggplot(data4, aes(x = Aar))  +
  geom_bar(y = sum.AngMaend) +
  geom_bar(y = sum.AngKvinder)

ggplot(data4, aes(Aar, y = value,)) + 
  geom_line(aes(y = sum.AngMaend, col = "sum.AngMaend")) + 
  geom_line(aes(y = sum.AngKvinder, col = "sum.AngKvinder"))

  summearise(sum.AngMaend = sum(AngMaend, na.rm = TRUE)), sum.AngKvinder = sum(AngKvinder, na.rm = TRUE))
  
, fill = status ) + geom_bar() + facet_wrap(~ bechdel_test, ncol = 2, scales = "free_x")

mangler.ret <- data2 %>%
  filter( !is.na(GnsIndkomst) & is.na(Retning) )
 
hist(data3$GnsIndkomst, freq=F, main='GnsIndkomst', 
     col='darkgreen', )




data3 <- data2 %>% 
  filter( !is.na(GnsIndkomst) & OptMK < 1 ) 



data4 <- data2 %>% 
  filter( !is.na(GnsIndkomst) & Aar == '2016') %>%
  top_n( 10 , AngMaend )



data4 <- data2 %>% 
  filter( !is.na(GnsIndkomst) & Aar == '2016') %>%
  top_n( 10 , AngKvinder )

data5 <- data2 %>% 
  filter( !is.na(GnsIndkomst) & Aar == '2015') %>%
  top_n( 10 , AngKvinder )



%>%  
  group_by(Placering) %>%
  summarise(OptMaend/AngMaend, OptKvinderAngKvinder, GnsIndkomst)


data4 <- data3 %>%
  mutate(ratioM = AngMaend/ OptMaend, ratioK = AngKvinder /OptKvinder   ) %>%




  


data$Kvotient <- gsub(",",".",data$Kvotient)

data$Kvotient <- as.numeric(data$Kvotient)
data$AngMaend <- as.numeric(data$AngMaend)
data$AngKvinder <- as.numeric(data$AngKvinder)



class(data$Kvotien)
class(data$angMaend)
class(data.1$Kvotient)

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
 filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 

head(data)



data.3 <- data.2 %>%
  filter(Kvotient > 7.4 & AngMK < 1)  ) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data.4 <- data.3 %>%
  filter(Kvotient > 11 & OptMK > 0.6)  ) %>%
  arrange( - AngMK, -OptMK)



data.3 <- data.2 %>%
  filter(OptMK != Inf )
  lin.model.1$fittedvalues


class(data.1$OptMK) data.2$AngMK <- round(data.2$AngMK, 2) na.rm = TRUE) 

OptMK <- data$OptMaend/data$OptKvinder
data$AngMaend[1]/data$AngKvinder[1]

class(data$AngMaend)
data.2 <- data %>%
  group_by(Aar) %>%
  summarise(OptMaend = sum(OptMaend), OptKvinder = sum(OptKvinder), AngMaend = sum(AngMaend), AngKvinder = sum(AngKvinder))
       
df.avg <- df %>%
  group_by(year) %>%
  summarise(count = n())

AngMK = AngMaend / AngKvinder)

data.16 <- data  %>% filter(Aar == 2016) 
data.16$AngMaend <- as.numeric(data.16$AngMaend)
sum(class(data.16$AngMaend)
mean(data.16$AngMaend, na.rm = TRUE)
