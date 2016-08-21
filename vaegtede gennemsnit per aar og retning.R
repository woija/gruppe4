install.packages("plyr")
library(plyr)

#udregner vaegtede, forventede lonninger per kon og aar
aarm <- ddply(data.2, .(Aar),function(x) data.frame(Kvinder=weighted.mean(x$GnsIndkomst, x$OptKvinder, na.rm = TRUE)))
aark <- ddply(data.2, .(Aar),function(x) data.frame(Maend=weighted.mean(x$GnsIndkomst, x$OptMaend, na.rm = TRUE)))
gnsaar <- aark %>% left_join(aarm, by = "Aar")
gnsaar[5,1]="I alt"
gnsaar[5,2]=weighted.mean(data.2$GnsIndkomst, data.2$OptKvinder, na.rm = TRUE)
gnsaar[5,1]=weighted.mean(data.2$GnsIndkomst, data.2$OptMaend, na.rm = TRUE)
gnsaar$Forskel <- gnsaar$Maend - gnsaar$Kvinder 
save(gnsaar, file = "gnsaar.RData")

#udregner vaegtede, forventede lonninger per kon og studieretning
retningm <- ddply(data.2, .(Retning),function(x) data.frame(Kvinder=weighted.mean(x$GnsIndkomst, x$OptKvinder, na.rm = TRUE)))
retningk <- ddply(data.2, .(Retning),function(x) data.frame(Maend=weighted.mean(x$GnsIndkomst, x$OptMaend, na.rm = TRUE)))
gnsretning <- retningk %>% left_join(retningm, by = "Retning")
gnsretning[11,1]="I alt"
gnsretning[11,3]=weighted.mean(data.2$GnsIndkomst, data.2$OptKvinder, na.rm = TRUE)
gnsretning[11,2]=weighted.mean(data.2$GnsIndkomst, data.2$OptMaend, na.rm = TRUE)
gnsretning$Forskel <- gnsretning$Maend - gnsretning$Kvinder 
save(gnsretning, file = "gnsretning.RData")
