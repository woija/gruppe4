library("readr")
library("dplyr")
library("rio")
library("dplyr")
library("magrittr")
library("ggplot2")
library("tidyr")
library("stringr")
library("grid")
library("gridExtra")
library("RColorBrewer")



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
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.2gather = Uni.2 %>%
  gather(key = Køn, 
         value = frequency,
          -InstNavn)

ggplot(data=Uni.2gather, aes(x=reorder(InstNavn, frequency), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") +
  ggtitle("Fordeling af køn på danske uddannelse institutioner") + theme_minimal()
  scale_fill_brewer(palette = "Dark2")

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

o= ggplot(data=Uni.3gather, aes(x=reorder(GnsIndkomst, Uddannelse), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") + 
scale_x_discrete(breaks=c("801156", "805757", "809791", "847594", "849602", "853619", "909823", "926244", "1052373", "1180098"),
                 labels=c("Civil.ing.kemi, 801.156 kr", "Civil.ing.elektro, 805.757 kr", "Cand.oecon, 809.791 kr", "Erhvervsøkonomi mat, 847.594 kr", 
                          "Jura, 849.602 kr", "Civil.ing.maskin, 853.619 kr", "Matematikøkonomi, 909.823 kr", "Læge , 926.244 kr", 
                          "Cand.polit, 1.052.372 kr", "Forsikringsmatematik, 1.180.098 kr")) +
  ggtitle("Top 10 af højeste lønninger og gennesnits ansøger fordelingen for disse 
          uddannelser, 2013-2016") + 
  scale_fill_brewer(palette = "Dark2") + theme_minimal()



### GNS. TOP 10 MEST SØGTE MÆND
Uni.5 = Udddata.2 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE))

Uni.5 = Uni.5 %>%
  arrange(desc(Mænd)) %>%
  head(n=10)

Uni.5$OptNr = factor(Uni.5$OptNr)

m = ggplot(data=Uni.5, aes(x=reorder(OptNr, Mænd), y= Mænd)) +  geom_bar(stat="identity", fill= "Red") +
  #geom_point() +
  coord_flip() + ylab("Total") + xlab("") + 
  scale_x_discrete(breaks=c("13045", "22415", "30010", "17115", "22010", "13060", "22610", "10110", "10410", "13010"),
                   labels=c("Erhversøkonomi Projektledelse, CBS", "Jura, AU", "Arkitekt", "Læge, SDU", "Læge, AU", 
                            "International Business, CBS", "Erhvervsøkonomi, AU", "Læge, KU", "Jura, KU", "Erhvervsøkonomi, CBS")) + 
ggtitle("Top 10 gennemsnitlig mest søgte uddannelse for 
        mænd, 2013-2016") 
 

### GNS. TOP 10 MEST SØGTE KVINDER
Uni.6 = Udddata.2 %>%
  group_by(OptNr, Uddannelse) %>%
  summarise (Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE))

Uni.6 = Uni.6 %>%
  arrange(desc(Kvinder)) %>%
  head(n=10)

Uni.6$OptNr = factor(Uni.6$OptNr)

k = ggplot(data=Uni.6, aes(x=reorder(OptNr, Kvinder), y= Kvinder)) +   geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") 
scale_x_discrete(breaks=c("22415", "16020", "22420", "17115", "10140", "22010", "32010", "10410", "10368", "10110"),
                 labels=c("Jura, AU", "Human/teologi, RUC", "Psykologi, AU", "Læge, SDU", "Veterinærmedicin, KU", "Læge, AU", 
                          "Designer Unika Design", "Jura, KU", "Psykologi, KU", "Læge, KU")) + 
ggtitle("Top 10 gennemsnitlig mest søgte uddannelse for 
        kvinder, 2013-2016") + 
  scale_fill_brewer(palette = "Dark2") + theme_minimal()
  



###KOMBINERE DE TO SENESTE PLOTS
grid.arrange(m, k, ncol=2)

### KOMBINERE

grid.arrange( l, o, ncol=2)




