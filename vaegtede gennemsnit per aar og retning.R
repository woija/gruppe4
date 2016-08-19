install.packages("plyr")
library(plyr)

#udregner vaegtede, forventede lonninger per kon og aar
aarm <- ddply(data.2, .(Aar),function(x) data.frame(Kvinder=weighted.mean(x$GnsIndkomst, x$OptKvinder, na.rm = TRUE)))
aark <- ddply(data.2, .(Aar),function(x) data.frame(Maend=weighted.mean(x$GnsIndkomst, x$OptMaend, na.rm = TRUE)))
gnsaar <- aark %>% left_join(aarm, by = "Aar")
gnsaar$Forskel <- gnsaar$Maend - gnsaar$Kvinder 

#udregner vaegtede, forventede lonninger per kon og studieretning
retningm <- ddply(data.2, .(Retning),function(x) data.frame(Kvinder=weighted.mean(x$GnsIndkomst, x$OptKvinder, na.rm = TRUE)))
retningk <- ddply(data.2, .(Retning),function(x) data.frame(Maend=weighted.mean(x$GnsIndkomst, x$OptMaend, na.rm = TRUE)))
gnsretning <- retningk %>% left_join(retningm, by = "Retning")
gnsretning$Forskel <- gnsretning$Maend - gnsretning$Kvinder 
