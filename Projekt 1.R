

install.packages("rio")
library("rio")
library("ggplot2")
library("dplyr")
library("stringr")
install.packages("ggvis")
library("ggvis")
install.packages("ggrepel")
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


# plot af retninger og gns indkomst 
p = ggplot(data = data1, aes(x=Retning, y=GnsIndkomst)) + 
  geom_point()
p

##  Kvindeandel i optag og ansøg
data2 = data %>%
  mutate(KvindeAndOpt = round(OptKvinder /(OptKvinder + OptMaend),2), 
         KvindeAndAng = round(AngKvinder / (AngKvinder + AngMaend),2),
         TotalOpt = OptKvinder + OptMaend,
         TotalAng = AngKvinder + AngMaend)

## 
fit <- lm(KvindeAndOpt ~ GnsIndkomst, data = data2)
summary(fit)

## 
p1 <- ggplot(data = data2, aes(x = GnsIndkomst, y = KvindeAndOpt)) + 
  geom_point()
p1

p2 <- ggplot(data = data2, aes(x = Kvotient,  y = KvindeAndOpt)) +
  geom_point()
p2



## Plots
# Udvikling i total antal ansøgere 
data3 = data2 %>%
  group_by(Aar, Retning) %>%
  summarise( Total = sum(TotalAng, na.rm=TRUE),
             TotalKviAng = sum(AngKvinder, na.rm=TRUE)/sum(TotalAng, na.rm=TRUE),
             TotalKviOpt = sum(OptKvinder, na.rm=TRUE)/sum(TotalOpt, na.rm=TRUE)
             )

#Udvikling i total ansøgere
data3 %>%
  ggvis(~Aar, ~Total, stroke = ~factor(Retning)) %>%
  layer_lines()

#Udvikling i kvindeandel ansøgere
data3 %>%
  ggvis(~Aar, ~TotalKviAng, stroke = ~factor(Retning)) %>%
  layer_lines()

#Udvikling i kvindeandel optagelse
data3 %>%
  ggvis(~Aar, ~TotalKviOpt, stroke = ~factor(Retning)) %>%
  layer_lines

ggplot(data3, aes(Aar, TotalKviOpt, color = Retning)) +
  geom_line() #+
  #geom_text_repel(aes(label=paste("Retning", Retning)))


## 

data4 = data2 %>%
  group_by(InstNavn) %>%
  summarise( TotalOptMaend = sum(OptMaend, na.rm=TRUE),
             TotalOptKvinder = sum(OptKvinder, na.rm=TRUE),
             TotalAngMaend = sum(AngMaend, na.rm=TRUE),
             TotalAngKvinder = sum(AngKvinder, na.rm=TRUE),
             TotalOpt = TotalOptMaend + TotalOptKvinder, 
             TotalAng = TotalAngMaend + TotalAngKvinder,
             GnsTotalAng = TotalAng/4
  )


### EKSTRA
data4 %>%
  filter(Aar == "2016") %>%
  ggvis(~InstNavn, ~TotalOpt) %>%
  layer_bars() %>%
  coord_flip()

  ggplot(data4, aes(x = InstNavn, y = TotalOpt), color = Aar) + 
  geom_point() + 
  coord_flip()

ggplot(data4, aes(x=InstNavn, y=GnsTotalAng)) +
  geom_bar(stat="identity", fill="#53cfff", position="dodge") + 
  geom_text(aes(x=InstNavn, y=GnsTotalAng, ymax=GnsTotalAng, hjust=1, label=GnsTotalAng), size=4) + 
  theme_light(base_size=16) +
  #theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") +
  coord_flip() +
  ggtitle("Gennemsnitligt antal ansøgere")



data5 = data2 %>%
  group_by(InstNavn) %>%
  summarise( GnsAngM = round(sum(AngMaend, na.rm=TRUE)/4,0),
             GnsAngK = round(sum(AngKvinder, na.rm=TRUE)/4, 0))


### ENDELIGT KØNSPLOT
data6 = data.frame(
  InstNavn = c(rep("Designskolen", 2), rep("Arkitektskolen", 2), rep("ITU", 2), rep("DTU",2),
               rep("Det Kongelige Danske Kunstakademi", 2), rep("RUC", 2), 
               rep("AAU", 2), rep("CBS", 2), rep("SDU", 2), rep("AU", 2), rep("KU", 2)),
  GnsAng = c(57, 193, 138, 172, 357, 99, 927, 394, 520, 970, 722, 1078, 2593, 2355, 2686, 2395,
             2546, 2934, 3780, 4542, 4642, 7332),
  Koen = c(rep(c("Mand","Kvinde"), 11))
) 

ggplot(data=data6, aes(x=reorder(InstNavn, GnsAng), y=GnsAng, fill=Koen)) +
  geom_bar(stat="identity") +
  coord_flip() + ylab("Total") + xlab("") +
  scale_fill_manual(values=c("royalblue2", "royalblue4")) + # valgfrie farver se link: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 
  ggtitle("Fordeling af ansøgere")


## TOP 10 for hhv maend og kvinder i 2016 pba antal ansøgere
dataTopK = data2 %>%
  filter(Aar == "2016", AngKvinder > 400) %>%
  arrange(-AngKvinder)

dataTopM = data2 %>%
  filter(Aar == "2016", AngMaend > 250) %>%
  arrange(-AngMaend)

ggplot(data = dataTopK, aes(x = reorder(OptNavn, AngKvinder), y = AngKvinder)) + 
  geom_bar(stat = "identity", color="royalblue2", fill="royalblue2") +
  coord_flip() + ggtitle("Top 10 uddannelser for kvinder") + 
  ylab("Ansøgere") + xlab("")

ggplot(data = dataTopM, aes(x=reorder(OptNavn, AngMaend), y = AngMaend)) +
  geom_bar(stat = "identity", color = "royalblue4", fill="royalblue4") + 
  coord_flip() + ggtitle("Top 10 uddannelser for mænd") + 
  ylab("Ansøgere") + xlab("")

#Måske "studiestart bla bla.." kan fjernes fra OptNavn variablen. 
#Kan være top 10 skal laves for gennemsnit for de fire år, medmindre der er et
#bestemt år, der er interessant. 
             