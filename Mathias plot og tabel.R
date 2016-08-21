gaar = gnsaar[1:4, 1:3]
gretning = gnsretning [1:10, 1:4]

# TABEL 1


\begin{ThreePartTable}
\begin{TableNotes}[para,flushleft]Kilde: Danmarks Statistik\end{TableNotes}
\begin{longtable}[c]{@{}crrrr@{}}
\caption{Vægtede, forventede lønninger pr. køn og år, 2013-2016}\\
\toprule\addlinespace
År & 2013 & 2014 & 2015 & 2016 
\\\addlinespace
\midrule\endhead
Mænd & 612.272 & 614.439 & 617.768 & 619.243 
\\
Kvinder & 567.313 & 568.562 & 572.960 & 572.933 
\\
Forskel & 44.959 & 45.877 & 44.809 & 46.310 
\\\addlinespace
\bottomrule
\insertTableNotes
\end{longtable}
\end{TreePartTable}


# PLOT 1


gretning.gather = gretning %>%
  gather(key = Køn, 
         value = frequency,
         -Retning, -Forskel)

ggplot(data=gretning.gather, aes(x=reorder(Retning, Forskel), y=Forskel)) +
  geom_bar(stat="identity", aes(fill = Forskel > 0 )) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02") ) +
  coord_flip() + ylab("Forskel i forventede løn") + xlab("") + guides(fill=FALSE) + 
  expand_limits(y=-50000) +
  theme_minimal() +theme(axis.title=element_text(size=7), axis.text.y = element_text(size=7), axis.text.x = element_text(size=7)) +
  scale_x_discrete(breaks=c("Ing", "Bus/kom", "Jur", "Natur", "Kom", "Business", "Hum", "Science", "Sund", "Samf"),
                   labels=c("Ingeniør", "Business-Kommunikation", "Jura", "Naturvidenskab", "Kommunikation", "Business", 
                            "Humaniora", "Science", "Sundhedsvidenskab", "Samfundsvidenskab")) 
  