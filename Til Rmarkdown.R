library(viridis)
library(dplyr)
library(rio)
library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(magrittr)


?gather
## Load data

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
         KvindeAndAng = round(AngKvinder / TotalAng, 2))

data.2 <- data.1 %>% left_join(lon, by = "Placering") 

data3 <-  data.2 %>%
  filter(!is.na(GnsIndkomst)) %>%
  mutate(GnsIndT = GnsIndkomst/100000)

#Plot af køn på institutioner 

Uni.2 = data3 %>%
  group_by(InstNavn) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.2gather = Uni.2 %>%
  gather(key = Køn, 
         value = frequency,
         -InstNavn)

## NYT: GIVET NAVN TIL PLOT + TILFØJET EKSTRA EFTER XLAB

institution = ggplot(data=Uni.2gather, aes(x=reorder(InstNavn, frequency), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Antal") + xlab("") + guides(fill=FALSE) +
  scale_fill_brewer(palette = "Dark2") +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15),
                                                                  axis.text.x = element_text(size=20))
##NYT: LAVET PLOT AF KØN OG RETNING
#Plot af køn og retninger på retninger på uddannelser

Uni.7 = data3 %>%
  group_by(Retning) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.7gather = Uni.7 %>%
  gather(key = Køn, 
         value = frequency,
         -Retning)

retning = ggplot(data=Uni.7gather, aes(x=reorder(Retning, frequency), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Antal") + xlab("") + 
  scale_fill_brewer(palette = "Dark2") +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15),
                                                                  axis.text.x = element_text(size=20))

 ##NYT:
# Samler de to ovenstående plots

grid.arrange(institution, retning, ncol=2)

#Plot af køn og top 10 indkomster
Uni.3 = data3 %>%
  group_by(GnsIndkomst, Uddannelse) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE)) 


Uni.3 = Uni.3 %>%
  arrange(desc(GnsIndkomst)) %>%
  head(n=10)

Uni.3gather = Uni.3 %>%
  gather(key = Køn, 
         value = frequency,
         -GnsIndkomst, - Uddannelse)

##NYT: GIVET NAVN TIL PLOT + TILFØJET EKSTRA EFTER XLAB
indkomst = ggplot(data=Uni.3gather, aes(x=reorder(GnsIndkomst, Uddannelse), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Antal") + xlab("") + guides(fill=FALSE) + 
  scale_x_discrete(breaks=c("801156", "805757", "809791", "847594", "849602", "853619", "909823", "926244", "1052373", "1180098"),
                   labels=c("Civil.ing.kemi, 801.156", "Civil.ing.elektro, 805.757", "Cand.oecon, 809.791", "Erhvervsøkonomi mat, 847.594", 
                            "Jura, 849.602", "Civil.ing.maskin, 853.619", "Matematikøkonomi, 909.823", "Læge , 926.244", 
                            "Cand.polit, 1.052.372", "Forsikringsmatematik, 1.180.098")) + 
  scale_fill_brewer(palette = "Dark2") +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15),
                                                                  axis.text.x = element_text(size=20))

#Plot af køn og kvotienter 
Uni.4 = data3 %>%
  group_by(OptNr) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE), Kvotient=mean(Kvotient)) 

Uni.4 = Uni.4 %>%
  arrange(desc(Kvotient)) %>%
  head(n=10)


Uni.4gather = Uni.4 %>%
  gather(key = Køn, 
         value = frequency,
         -Kvotient, - OptNr)

##NYT: GIVET NAVN TIL PLOT
kvotient = ggplot(data=Uni.4gather, aes(x=reorder(Kvotient, Køn), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Antal") + xlab("") +
  scale_x_discrete(breaks=c("10.825", "10.85", "10.975", "11", "11.05", "11.15", "11.375", "11.675", "11.95", "12.125"), 
                   labels=c("Statskundskab, KU, 10.83", "Medicin, AU, 10.85", "Antropologi, KU, 10.98", "Internation Shipping and Trade, CBS, 11.00", 
                            "Erhvervsøkonomi Projektledelse, CBS, 11.05", "Medicin, KU, 11.15 ", "Psykologi, KU, 11.38", "International Business and Politics, CBS, 11.68", 
                            "Molekylærbiomedicin, KU, 11.95", "International Business, CBS, 12.13")) + 
  scale_fill_brewer(palette = "Dark2") +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15),
                                                                  axis.text.x = element_text(size=20))
##NYT:
#Samler de to ovenstående plots

grid.arrange(indkomst, kvotient, ncol=2)

#Top 10 for mænd
Uni.5 = data3 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE))

Uni.5 = Uni.5 %>%
  arrange(desc(Mænd)) %>%
  head(n=10)

Uni.5$OptNr = factor(Uni.5$OptNr)

Mænd <- ggplot(data=Uni.5, aes(x=reorder(OptNr, Mænd), y= Mænd)) +  geom_bar(stat="identity", , fill = "#D95F02") +
  #geom_point() +
  coord_flip() + ylab("Antal mænd") + xlab("") + 
  scale_x_discrete(breaks=c("13045", "22415", "30010", "17115", "22010", "13060", "22610", "10110", "10410", "13010"),
                   labels=c("HA Projektledelse, CBS", "Jura, AU", "Arkitekt", "Læge, SDU", "Læge, AU", 
                            "International Business, CBS", "HA, AU", "Læge, KU", "Jura, KU", "HA, CBS")) +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15), axis.text.x = element_text(size=20))


#Top 10 for kvinder
Uni.6 = data3 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise (Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE))

Uni.6 = Uni.6 %>%
  arrange(desc(Kvinder)) %>%
  head(n=10)

Uni.6$OptNr = factor(Uni.6$OptNr)

Kvinder <- ggplot(data=Uni.6, aes(x=reorder(OptNr, Kvinder), y= Kvinder)) + geom_bar(stat="identity", fill = "#1B9E77") +
  coord_flip() + ylab("Antal kvinder") + xlab("") + 
  scale_x_discrete(breaks=c("22415", "16020", "22420", "17115", "10140", "22010", "32010", "10410", "10368", "10110"),
                   labels=c("Jura, AU", "Human/teologi, RUC", "Psykologi, AU", "Læge, SDU", "Veterinærmedicin, KU", "Læge, AU", 
                            "Designer Unika Design", "Jura, KU", "Psykologi, KU", "Læge, KU")) +  theme_minimal() + theme(axis.title=element_text(size=20), axis.text.y = element_text(size=15),
                                                                                                                          axis.text.x = element_text(size=20))

grid.arrange(Mænd, Kvinder, ncol=2)




