library(ggplot2)
library(GGally)
library(dplyr)
library(lmtest)
library(skedastic)
library(nortest)
library(openxlsx)
library(tseries)


#wczytanie danych z gretla 
data <- na.omit(gretldata) # usuwanie braków danych 
data %>% arrange(totsc8) # uporządkowanie danych rosnąco względem totsc8
pairs(data) # wykresy rozrzutu zmiennych 
ggpairs(data,
        axisLabels = "none",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"),
        diag = list(continuous = "blankDiag", discrete = "barDiag", na = "naDiag")) # tablica korelacji 
write.xlsx(data, getwd()) # importowanie danych w celu przerzucenia ich do gretl'a



boxplot(gretldata$uhat, horizontal = TRUE) #boxplot reszt 

outlier <- gretldata %>% filter(uhat == max(gretldata$uhat)) #outlier 
data_no_outliers <- gretldata %>% filter(!(uhat == max(uhat))) #dane po usunieciu outliera 
write.xlsx(data_no_outliers, getwd()) # ponowny import danych do gretla 



# Wykres 6. Wykres rozrzutu zmiennych totsc8 i lnchpct z dwoma modelami regresji
x = seq(0,100,0.01)
y1 = 685.222 + 1.51645*mean(gretldata$percap) - 1.14542*x + 0.0059*x^2
y2 = 678.424 + 1.51645*mean(gretldata$percap) - 0.746954*x

plot(gretldata$lnchpct,gretldata$totsc8,
     xlim=c(0,100), ylim=c(640,760), xlab="lnchpct", ylab="totsc8")
lines(x,y1)
lines(x,y2)