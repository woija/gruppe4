library("readr")
library("dplyr")
library("rio")
library("dplyr")
library("magrittr")
library("ggplot2")
library("tidyr")
library("stringr")

londata = import("londata1.csv")
Udddata = import("optagelse3.csv")

Udddata.1 = Udddata %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         TotalAng = AngMaend + AngKvinder,
         AngMK = round(AngMaend / AngKvinder,2)  ) %>%
  arrange(-TotalOpt, -TotalAng, - AngMK, -OptMK)

Udddata.2 = Udddata.1 %>% 
  left_join(londata, by = "Placering")

Udddata.3 = Udddata.2 %>%
  filter( !is.na(GnsIndkomst) )

data = Udddata.3 %>%
  mutate( KvindeAndOpt = round(OptKvinder / TotalOpt, 2),
          KvindeAndAng = round(AngKvinder / TotalAng, 2)
  )

Uni.2 = Udddata.3 %>%
  group_by(InstNavn) %>%
  summarise(AntalM =sum(round(AngMaend/4,0), na.rm =TRUE), AntalK =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

### PLOT AF KØN PÅ INSTITUTIONER
Uni.2gather = Uni.2 %>%
  gather(key = gender, 
         value = frequency,
          -InstNavn)

ggplot(data=Uni.2gather, aes(x=reorder(InstNavn, frequency), y=frequency, fill=gender)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") 

### PLOT AF KØN OG TOPLØN
Uni.3 = Udddata.3 %>%
  group_by(GnsIndkomst, Uddannelse) %>%
  summarise(AntalM =sum(round(AngMaend/4,0), na.rm =TRUE), AntalK =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.3 = Uni.3 %>%
  arrange(desc(GnsIndkomst)) %>%
  head(n=10)

Uni.3gather = Uni.3 %>%
  gather(key = gender, 
         value = frequency,
         -GnsIndkomst, - Uddannelse)

ggplot(data=Uni.3gather, aes(x=reorder(GnsIndkomst, Uddannelse), y=frequency, fill=gender)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") 

### PLOT AF KØN OG KVOTIENTER
Uni.4 = Udddata.3 %>%
  group_by(Kvotient, Uddannelse) %>%
  summarise(AntalM =sum(round(AngMaend/4,0), na.rm =TRUE), AntalK =sum(round(AngKvinder/4,0), na.rm=TRUE)) 

Uni.4 = Uni.4 %>%
  arrange(desc(Kvotient)) %>%
  head(n=10)

Uni.4gather = Uni.4 %>%
  gather(key = gender, 
         value = frequency,
         -Kvotient, - Uddannelse)

ggplot(data=Uni.4gather, aes(x=reorder(Uddannelse, Kvotient), y=frequency, fill=gender)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") 