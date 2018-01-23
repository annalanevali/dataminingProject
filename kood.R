
# Data mining project code

# Andmed olid saadaval vaid pdf kujul. Andmed on käsitsi sisestatud lehelt: http://biathlonresults.com/
# Valida 2016/2017 hooaeg, World CUP/WCHs, BMW IBU World Cup Biathlon 1 Oestersund (Swe) 
# (25 NOV-04 DEC), Men 20 km Individual, Reports, Competition analysis.

# Read in data
data = read.csv2("C:\\Users\\laanevalia\\Desktop\\AK projekt\\results_copy.csv", 
                 sep= ",", dec=".", header = TRUE, skip = 2,  stringsAsFactors=FALSE) 
attach(data)

# Dimensions of data
dim(data) #103 rows and 23 columns.

#Meaning of the attributes are explained in excel file. Overview of data
head(data)

# Column names
names(data)

# First overview of attributes and repairs/converting/new attributes.

#Attribute "Riik"
sort(table(Riik), decreasing = TRUE)

# Code to name for analysis later

library(countrycode)
data$RiigiNimi<-countrycode(Riik, "iso3c", "country.name")
# Got a Warning message: In countrycode(Riik, "iso3c", "country.name") : Some values were not matched unambiguously.
# These were BLG, BUL, CRO, GER, LAT, SLO, SUI

# Replace LAT->LVA, SUI->CHE, GER-> DEU, BUL->BGR, SLO->SVN, CRO->HRV, (my mistake) BLG->BLR. Reason: 
#https://en.wikipedia.org/wiki/List_of_IOC_country_codes
#http://www.fao.org/countryprofiles/iso3list/en/

data$Riik[data$Riik == "BLG"] <- "BLR"
data$Riik[data$Riik == "BUL"] <- "BGR"
data$Riik[data$Riik == "CRO"] <- "HRV"
data$Riik[data$Riik == "GER"] <- "DEU"
data$Riik[data$Riik == "LAT"] <- "LVA"
data$Riik[data$Riik == "SLO"] <- "SVN"
data$Riik[data$Riik == "SUI"] <- "CHE"

# Again
data$RiigiNimi<-countrycode(data$Riik, "iso3c", "country.name")
attach(data)

# Atrributes "Ring1"-"Ring5"

# First we have to converte them
data$Ring1 <- as.double(format(strptime(Ring1,  format="%M.%S"),format="%M.%S"))
data$Ring2 <- as.double(format(strptime(Ring2,  format="%M.%S"),format="%M.%S"))
data$Ring3 <- as.double(format(strptime(Ring3,  format="%M.%S"),format="%M.%S"))
data$Ring4 <- as.double(format(strptime(Ring4,  format="%M.%S"),format="%M.%S"))
data$Ring5 <- as.double(format(strptime(Ring5,  format="%M.%S"),format="%M.%S"))
attach(data)

# Statistics
summary(Ring1)
summary(Ring2)
summary(Ring3)
summary(Ring4)
summary(Ring5)

# Shooting attributes. Number of shots missed in total and by prone and staniding
table(sort(Trahve))

table(sort(Lamades1))

table(sort(Lamades2))

table(sort(Püsti1))

table(sort(Püsti2))

# Some graphs. How shooting time (in seconds) effects shots missed

plot(Lamades1,TiiruaegL1)

plot(Lamades2,TiiruaegL2)

plot(Püsti1,TiiruaegP1)

plot(Püsti2,TiiruaegP2) #Big expeption

# Some new attributes
data$lamadeskokku <- Lamades1+Lamades2
data$punstikokku <- Püsti1+Püsti2
data$lamadestiir <- TiiruaegL1+TiiruaegL2
data$pustitiir <- TiiruaegP1+TiiruaegP2
attach(data)

# Statistics
summary(TiiruaegL1)
summary(TiiruaegP1)
summary(TiiruaegL2)
summary(TiiruaegP2)

# Number of points
sort(table(Punkte),decreasing = TRUE)

# Let's see how Estonian athetes did.
subset(data, data$Riik=="EST")

#Three Estonian athletes: K.Kõiv, R.Zahkna and J.Talihärm represented Estonia. They got respectively 
# 21, 84 and 98 place.  

# Aim of this project was fist to get some model that predicts how to get good results in biathlon. 
# That failed due to data. There are too many random exceptions to make comprehensive conclusions.
# For that reason I analyzed data by teams (countries) and tried to give suggestions to how get better places.
 
library("dplyr")
library("rworldmap")
library("fields")
library("ggplot2")


# Heatmap of team sizes per country.Number of athletes allowed to complete is based on previous season. 
# Stronger countries get more places. 

andmed_grupeeritult = group_by(data, RiigiNimi)

summeeritult2 <- summarise(andmed_grupeeritult, arv = n())
mapped_data2 <- joinCountryData2Map(summeeritult2, joinCode = "NAME", nameJoinColumn = "RiigiNimi")
colourPalette = two.colors(n=max(summeeritult2$arv)-min(summeeritult2$arv)+1, start="dark green", middle='yellow', end="red", alpha=1.0)
mapParams2<-mapCountryData(mapped_data2, nameColumnToPlot="arv", mapTitle="Number of athletes competing", 
                           catMethod = seq(0, 6,1), colourPalette = colourPalette, addLegend=FALSE)
do.call(addMapLegend, c(mapParams2, legendLabels="all",legendWidth=0.5, labelFontSize=0.8))


# Estonia has 3 athletes competing which is avarage result. Not the best but could be worse. 
subset(summeeritult2, summeeritult2[1]=="Estonia")


# Maximum number of places
subset(summeeritult2, summeeritult2[2]==max(summeeritult2[2]))
# France, Germany , Norway, Russian Federation with 6

# Minimum number of places
subset(summeeritult2, summeeritult2[2]==min(summeeritult2[2]))
# Belgium and Croatia with 1


# Heatmap of world cup points by countries. Lets see which countries were most successful.

# Which countires
data$Riik[Punkte!=0]
# So European countries, USA, Korea, Japan, Kazanztan have some points from first world cup. 

summeeritult <- summarise(andmed_grupeeritult, Punktekokku = sum(Punkte)/n())

# Most successful countries after first world cup.
mapped_data <- joinCountryData2Map(summeeritult, joinCode = "NAME", nameJoinColumn = "RiigiNimi")
colourPalette <- two.colors(n=round(max(summeeritult$Punktekokku)-min(summeeritult$Punktekokku)+1), 
                            start="dark green", middle='yellow', end="red")
mapParams<- mapCountryData(mapped_data, nameColumnToPlot="Punktekokku", mapTitle="World Cup points by countries", 
                          catMethod = seq(0, 26, 1), colourPalette = colourPalette, addLegend=FALSE)
do.call(addMapLegend, c(mapParams,legendLabels="all",legendWidth=0.5, labelFontSize=0.5))

# Estonia has 6.67 points. Could do better (reminder: not all athles get points, only first 40) 
subset(summeeritult, summeeritult[1]=="Estonia")

# Maximum number of points
subset(summeeritult, summeeritult[2]==max(summeeritult[2]))
# Norway with 25.67

# Minimum number of points
subset(summeeritult, summeeritult[2]==min(summeeritult[2]))
# Belgium, Canada, Croatia, Japan, Kazakhstan, Lithuania,  Poland,  Republic of Korea and Ukraine with 0


# Lets see which teams are the best at prone and standing shooting by making a plot.
library(reshape2)
library(ggplot2)

summeeritult3 <- summarise(andmed_grupeeritult, Prone=sum(lamadeskokku)/n(), 
                           Standing=sum(punstikokku)/n())
DF1 <- melt(summeeritult3, id.var="RiigiNimi")

ggplot(DF1, aes(x = RiigiNimi, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual("", values = c("Prone" = " darkolivegreen3", "Standing" = "orange"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Missed shots in total") +
  xlab("Country") + ylab("Missed shots")+ scale_y_continuous(limits = c(0,7.5), breaks = round(seq(0,7.5)),1)

# Missed shots by shooting range. Last graph was by prone and standind shooting in total. 
summeeritult4 <- summarise(andmed_grupeeritult, Prone=sum(lamadeskokku)/(2*n()), 
                           Standing=sum(punstikokku)/(2*n()))
DF2 <- melt(summeeritult4, id.var="RiigiNimi")

ggplot(DF2, aes(x = RiigiNimi, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual("", values = c("Prone" = " darkolivegreen3", "Standing" = "orange"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Missed shots in total") +
  xlab("Country") + ylab("Missed shots")+ scale_y_continuous(limits = c(0,7.5), breaks = round(seq(0,7.5)),1)

# Best at prone shooting
subset(summeeritult3, summeeritult3[2]==min(summeeritult3[2]))
# Latvia with 0.67/10

# Worst at prone shooting
subset(summeeritult3, summeeritult3[2]==max(summeeritult3[2]))
# Japan with 3.5/10

# Best at standing shooting
subset(summeeritult3, summeeritult3[3]==min(summeeritult3[3]))
# Romania with 1.67/10

# Worst at prone shooting
subset(summeeritult3, summeeritult3[3]==max(summeeritult3[3]))
# Belgium with 6/10

# Results of Estonian athletes
subset(summeeritult3, summeeritult3[1]=="Estonia")
# Estonia: prone 2/10    and standing   4/10.


# Lets see which teams are the fastest at prone and standing shooting by making a plot

summeeritult5 <- summarise(andmed_grupeeritult, Prone=sum(lamadestiir)/n(), 
                          Standing=sum(pustitiir)/n())
DF3 <- melt(summeeritult5, id.var="RiigiNimi")

ggplot(DF3, aes(x = RiigiNimi, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +ggtitle("Shooting time") +xlab("Country") + ylab("Time")+
  scale_fill_manual("Time(sec)", values = c("Prone" = " darkolivegreen3", "Standing" = "orange"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Previous graph was shooting time by prone and standing shooting time in total. Lets results see per shooting range

summeeritult6 <- summarise(andmed_grupeeritult, Prone=sum(lamadestiir)/(2*n()), 
                           Standing=sum(pustitiir)/(2*n()))
DF4 <- melt(summeeritult6, id.var="RiigiNimi")

ggplot(DF4, aes(x = RiigiNimi, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +ggtitle("Shooting time") +xlab("Country") + ylab("Time")+
  scale_fill_manual("Time(sec)", values = c("Prone" = " darkolivegreen3", "Standing" = "orange"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Best at prone shooting time (by two shooting ranges)
subset(summeeritult5, summeeritult5[2]==min(summeeritult5[2]))
# Belgium with 107.1 sec

# Worst at prone shooting time (by two shooting ranges)
subset(summeeritult5, summeeritult5[2]==max(summeeritult5[2]))
# Japan 140.8 sec

# Best at standing shooting time (by two shooting ranges)
subset(summeeritult5, summeeritult5[3]==min(summeeritult5[3]))
# Belarus with 102.15 sec

# Worst at prone shooting
subset(summeeritult5, summeeritult5[3]==max(summeeritult5[3]))
# Czech Republic with 134.92.

# Results of Estonian athletes
subset(summeeritult5, summeeritult5[1]=="Estonia")
# Estonia: prone 133.8667 sec  and standing   129.4 sec.


# Analyse skiing times by making a graph.

summeeritult7 <- summarise(andmed_grupeeritult, Loop1=mean(Ring1),
                           Loop2=mean(Ring2), Loop3=mean(Ring3),
                           Loop4=mean(Ring4), Loop5=mean(Ring5))
DF5 <- melt(summeeritult7, id.var="RiigiNimi")

ggplot(DF5, aes(x = RiigiNimi, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+scale_fill_manual("Time(min)", values = c("Loop1" = " darkolivegreen3", 
  "Loop2" = "orange", "Loop3" = "darkgoldenrod3", "Loop4" = "darkorange", Loop5="darkorange3"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Loop time") +
  xlab("Country") + ylab("Time")

# Best and worse for loop 1
subset(summeeritult7, summeeritult7[2]==max(summeeritult7[2]))
subset(summeeritult7, summeeritult7[2]==min(summeeritult7[2]))
# Worst Republic of Korea  with 10.30, best Norway with 9.14

# Best and worse for loop 2
subset(summeeritult7, summeeritult7[3]==max(summeeritult7[3]))
subset(summeeritult7, summeeritult7[3]==min(summeeritult7[3]))
# Worst Republic of Korea  with 10.2, best Norway with 9.2

# Best and worse for loop 3
subset(summeeritult7, summeeritult7[4]==max(summeeritult7[4]))
subset(summeeritult7, summeeritult7[4]==min(summeeritult7[4]))
# Worst Republic of Korea  with 10.49, best Norway with 9.28

# Best and worse for loop 4
subset(summeeritult7, summeeritult7[5]==max(summeeritult7[5]))
subset(summeeritult7, summeeritult7[5]==min(summeeritult7[5]))
# Worst Republic of Korea  with 10.43, best Norway with 9.37

# Best and worse for loop 4
subset(summeeritult7, summeeritult7[6]==max(summeeritult7[6]))
subset(summeeritult7, summeeritult7[6]==min(summeeritult7[6]))
# Worst Republic of Korea  with 10.17, best Norway with 9.19

# Märkus: olen siin teinud väikse vea. Arvutades riikide keskmist oleks tulnud kindlasti kasutada mingit funktsiooni
# mis arvestaks et tegu on minutitega. Hetkel on arvestatud minuteid ja sekundeid kui numbreid aga ühes minutis on 
# 60 sekundit, mitte 100.

# Results of Estonian team
subset(summeeritult7, summeeritult7[1]=="Estonia")

#RiigiNimi    Loop1 Loop2    Loop3    Loop4    Loop5
#<chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#  1   Estonia 9.476667  9.84 9.946667 10.12333 9.486667

# Overall if competition  would be held tomorrow I would suggest to Estonian team to focus on the shooting
# (not so much on a skiing). But in the long term if they want a gold medal they have to become faster in skiing 
# than Norway. 