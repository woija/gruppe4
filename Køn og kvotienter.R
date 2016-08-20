library("rio")
library("ggplot2")
library("dplyr")
library("stringr")
library("ggvis")
library("ggrepel")
library("knitr")

londata <- read.csv("londata1.csv", sep = ";")

uddannelse <- read.csv("optagelse3.csv", sep = ";")

# samler data
data <- uddannelse %>%
  left_join(londata, by = "Placering")

data <- data %>%
  filter(!is.na(GnsIndkomst))

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) ) 


#Kvindeandel i optag og ansøg
data2 = data %>%
  mutate(KvindeAndOpt = round(OptKvinder /(OptKvinder + OptMaend),2), 
         KvindeAndAng = round(AngKvinder / (AngKvinder + AngMaend),2),
         TotalOpt = OptKvinder + OptMaend,
         TotalAng = AngKvinder + AngMaend)


### PLOT AF KØN OG KVOTIENTER
Uni.4 = data2 %>%
  group_by(OptNr) %>%
  summarise(Mænd =sum(round(AngMaend/4,0), na.rm =TRUE), Kvinder =sum(round(AngKvinder/4,0), na.rm=TRUE), Kvotient=mean(Kvotient)) 

Uni.4 = Uni.4 %>%
  arrange(desc(Kvotient)) %>%
  head(n=10)


Uni.4gather = Uni.4 %>%
  gather(key = Køn, 
         value = frequency,
         -Kvotient, - OptNr)

ggplot(data=Uni.4gather, aes(x=reorder(Kvotient, Køn), y=frequency, fill=Køn)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") +
scale_x_discrete(breaks=c("10.825", "10.85", "10.975", "11", "11.05", "11.15", "11.375", "11.675", "11.95", "12.125"), 
                 labels=c("Statskundskab, KU, 10.83", "Medicin, AU, 10.85", "Antropologi, KU, 10.98", "Internation Shipping and Trade, CBS, 11.00", 
                          "Erhvervsøkonomi Projektledelse, CBS, 11.05", "Medicin, KU, 11.15 ", "Psykologi, KU, 11.38", "International Business and Politics, CBS, 11.68", 
                          "Cand.Scient u.n.a. Naturvidenskab , 11.95", "International Business, CBS, 12.13")) +
  ggtitle("Top 10 gennemsnitlig højest kvotient og ansøger fordelt på køn") + 
  scale_fill_brewer(palette = "Set1")



