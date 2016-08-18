rm(list=ls()) ## Sletter alle variable og funktioner
cat("\014") # Sletter consol teksten


setwd("/Users/simonharmat/Dropbox/Studie/Data_science/Gruppe4" ) ## Skriv stien


data <- import("optagelse3.csv") # læser csv-filen 
lon <- import("londata.csv") # læser csv-filen
?import

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 

data.1 <- data %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         AngMK = round(AngMaend / AngKvinder,2)  ) %>%
  arrange(-TotalOpt, - AngMK, -OptMK)

data2 <- data.1 %>% left_join(lon, by = "Placering")


mangler.ret <- data2 %>%
  filter( !is.na(GnsIndkomst) )
 
hist(data3$GnsIndkomst, freq=F, main='GnsIndkomst', 
     col='darkgreen', )




data3 <- data2 %>% 
  filter( !is.na(GnsIndkomst) & AngMK < 15 ) %>%
  top_n( 10 , GnsIndkomst )

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
 
  fit2 <- lm(GnsIndkomst ~ AngMK , data3)
summary(fit2)


p <- ggplot(data3, aes(GnsIndkomst, AngMK  ))
p + geom_point() + 
  #scale_colour_hue(l=50) +   
  geom_smooth(method=lm,    se=TRUE,      fullrange=TRUE)

  


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
