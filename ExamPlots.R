library("readr")
library("dplyr")
library("rio")
library("dplyr")
library("magrittr")
library("ggplot2")
library("tidyr")
library("stringr")

londata = read.csv("londata1.csv", sep = ";")
Udddata = read.csv("optagelse3.csv", sep = ";")

Udddata = Udddata %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         TotalAng = AngMaend + AngKvinder,
         AngMK = round(AngMaend / AngKvinder,2)  ) %>%
  arrange(-TotalOpt, -TotalAng, - AngMK, -OptMK)

Udddata.1 = Udddata %>% 
  left_join(londata, by = "Placering")

Udddata.2 = Udddata.1 %>%
  filter( !is.na(GnsIndkomst) )

Udddata.2 = Udddata.2 %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 

Udddata.2 = Udddata.2 %>%
  mutate( KvindeAndOpt = round(OptKvinder / TotalOpt, 2),
          KvindeAndAng = round(AngKvinder / TotalAng, 2)
  )



### PLOT AF KØN PÅ INSTITUTIONER
Uni.2 = Udddata.2 %>%
  group_by(InstNavn) %>%
  summarise(AntalM =sum(round(AngMaend/4,0), na.rm =TRUE), AntalK =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.2gather = Uni.2 %>%
  gather(key = gender, 
         value = frequency,
          -InstNavn)

ggplot(data=Uni.2gather, aes(x=reorder(InstNavn, frequency), y=frequency, fill=gender)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") +
  ggtitle("Fordeling af køn på danske uddannelse institutioner") + 
  scale_fill_brewer(palette = "Set1")

### PLOT AF KØN OG TOPLØN
Uni.3 = Udddata.2 %>%
  group_by(GnsIndkomst, Uddannelse) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE)) 


Uni.3 = Uni.3 %>%
  arrange(desc(GnsIndkomst)) %>%
  head(n=10)

Uni.3gather = Uni.3 %>%
  gather(key = Køn, 
         value = frequency,
         -GnsIndkomst, - Uddannelse)

ggplot(data=Uni.3gather, aes(x=reorder(GnsIndkomst, Uddannelse), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") +
scale_x_discrete(breaks=c("801156", "805757", "809791", "847594", "849602", "853619", "909823", "926244", "1052373", "1180098"),
                 labels=c("Civil.ing.kemi, 801.156", "Civil.ing.elektro, 805.757", "Cand.oecon, 809.791", "Erhvervsøkonomi mat, 847.594", 
                          "Jura, 849.602", "Civil.ing.maskin, 853.619", "Matematikøkonomi, 909.823", "Læge , 926.244", 
                          "Cand.polit, 1.052.372", "Forsikringsmatematik, 1.180.098")) +
  ggtitle("Top 10 af højeste lønninger og gennesnits ansøgertallet for disse 
          uddannelser, 2013-2016") + 
  scale_fill_brewer(palette = "Set1")



### GNS. TOP 10 MEST SØGTE MÆND
Uni.5 = Udddata.3 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE))

Uni.5 = Uni.5 %>%
  arrange(desc(Mænd)) %>%
  head(n=10)

Uni.5$OptNr = factor(Uni.5$OptNr)

ggplot(data=Uni.5, aes(x=reorder(OptNr, Mænd), y= Mænd)) + geom_point() +
  coord_flip() + ylab("Total") + xlab("") + 
  scale_x_discrete(breaks=c("13045", "22415", "30010", "17115", "22010", "13060", "22610", "10110", "10410", "13010"),
                   labels=c("Erhversøkonomi Projektledelse, CBS", "Jura, AU", "Arkitekt", "Læge, SDU", "Læge, AU", 
                            "International Business, CBS", "Erhvervsøkonomi, AU", "Læge, KU", "Jura, KU", "Erhvervsøkonomi, CBS")) + 
ggtitle("Top 10 gennemsnitlig mest søgte uddannelse for 
        mænd, 2013-2016") 
 

### GNS. TOP 10 MEST SØGTE KVINDER
Uni.6 = Udddata.3 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise (Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE))

Uni.6 = Uni.6 %>%
  arrange(desc(Kvinder)) %>%
  head(n=10)

Uni.6$OptNr = factor(Uni.6$OptNr)

ggplot(data=Uni.6, aes(x=reorder(OptNr, Kvinder), y= Kvinder)) + geom_point() + 
  coord_flip() + ylab("Total") + xlab("") +
scale_x_discrete(breaks=c("22415", "16020", "22420", "17115", "10140", "22010", "32010", "10410", "10368", "10110"),
                 labels=c("Jura, AU", "Human/teologi, RUC", "Psykologi, AU", "Læge, SDU", "Veterinærmedicin, KU", "Læge, AU", 
                          "Designer Unika Design", "Jura, KU", "Psykologi, KU", "Læge, KU")) + 
ggtitle("Top 10 gennemsnitlig mest søgte uddannelse for 
        kvinder, 2013-2016") 

