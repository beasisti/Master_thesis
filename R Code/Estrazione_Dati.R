# Necessary libraries -----------------------------------------------------
library(tidyr)
library(lubridate)
library(ggplot2)

library(tibble)
library(readr)
library(readxl)
library(dplyr)
library(fda)
library(roahd)
library(colorspace)
library(ggridges)
library(ggpubr)
library(proxy)
library(SparseM)
library(maps)
library(sp)
library(ggmap)

library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(rnaturalearth)
library(htmltools)

# Dati O3 ----------------------------------------------------------
dati1 = read.csv("./Dati_0/Dati_sensori_aria_2000-2009.csv")
dati2 = read.csv("./Dati_0/Dati_sensori_aria_2010-2017.csv")
dati3 = read.csv("./Dati_0/Dati_sensori_aria_dal_2018.csv")

stazioni = read.csv("./Dati_0/Stazioni_aria.csv")
unique(stazioni$NomeTipoSensore)
staz_o3 = stazioni[stazioni$NomeTipoSensore == "Ozono", ] # 92 stations
# write.csv(staz_o3, "./Dati_iniziali/stazioni_O3.csv")
rm(stazioni)

dati_o3_1 = dati1[which(dati1$idSensore %in% staz_o3$IdSensore), ]
range(dati_o3_1$Valore)
dati_o3_1$Valore[dati_o3_1$Valore < 0] = NA 
range(dati_o3_1$Valore, na.rm=T)    # ok
length(unique(dati_o3_1$idSensore)) # 82 stazioni
rm(dati1)

dati_o3_2 = dati2[which(dati2$idSensore %in% staz_o3$IdSensore), ]
range(dati_o3_2$Valore)
dati_o3_2$Valore[dati_o3_2$Valore < 0] = NA 
range(dati_o3_2$Valore, na.rm=T)      # ok
length(unique(dati_o3_2$idSensore))   # 86 stazioni
rm(dati2)

dati_o3_3 = dati3[which(dati3$idSensore %in% staz_o3$IdSensore), ]
range(dati_o3_3$Valore)
dati_o3_3$Valore[dati_o3_3$Valore < 0] = NA 
range(dati_o3_3$Valore, na.rm=T)      # ok
length(unique(dati_o3_3$idSensore))   # 51 stazioni
rm(dati3)

idSensore_1_2 <- unique(dati_o3_2$idSensore)[unique(dati_o3_2$idSensore) %in% unique(dati_o3_1$idSensore)]
# 77 stazioni in comune negli anni 2000-2017
idSensore_1_3 <- unique(dati_o3_3$idSensore)[unique(dati_o3_3$idSensore) %in% unique(dati_o3_1$idSensore)]
# 43 stazioni in comune tra 2000-2009 e 2018-2022
idSensore_2_3 <- unique(dati_o3_3$idSensore)[unique(dati_o3_3$idSensore) %in% unique(dati_o3_2$idSensore)]
# 51 stazioni in comune negli anni 2010-2022
idSensore_comuni_totale <- intersect(intersect(idSensore_1_2, idSensore_1_3),
                                     idSensore_2_3)

# 43 stazioni in comune su tutto il periodo, ma selezioniamo le 51
# quelle che iniziano a registrare dal 2010 sono: 10584 12020 17288 17295 17297 20041 20154 30165
dati_o3_1 = dati_o3_1[which(dati_o3_1$idSensore %in% idSensore_2_3), ]
dati_o3_2 = dati_o3_2[which(dati_o3_2$idSensore %in% idSensore_2_3), ]
dati_o3_3 = dati_o3_3[which(dati_o3_3$idSensore %in% idSensore_2_3), ]
dati_final = rbind(dati_o3_1, rbind(dati_o3_2, dati_o3_3))

# write_csv(dati_final, file='./Dati_iniziali/datasetO3.csv')

stazioni.usate <- staz_o3[which(staz_o3$IdSensore %in% unique(dati_final$idSensore)), -1]
# write.csv(stazioni.usate, "./Dati_iniziali/51stazioni_O3.csv")

rm(list = ls())


# EDA O3 ----------------------------------------------------------

dati = read_csv('./Dati_iniziali/datasetO3.csv')
prova = read.csv("./Dati_iniziali/stazioni_O3.csv")

# dates in proper format
dati$Data = strptime(dati$Data, format = "%d/%m/%Y %H:%M:%S")

nobs = table(dati$Data, dati$idSensore)
length(which(nobs>1))  # OK: no double obs
length(which(nobs==0)) # 1596249 missing values

# working with dates and hours
dates = dati$Data
unique(dates$mon)
unique(dates$year)
june22 = dati[which(dates$mon == 5 & dates$year == 122), ]

# obs exceeding threshold
idx = which(june22$Valore > 180)

# time-series
ggplot(data=june22, aes(x=as.POSIXct(Data), y=Valore, color=factor(idSensore))) +
  geom_line() + geom_hline(yintercept = 180, lwd=1) + theme(legend.position = "none") +
  xlab("Time") + ylab("Ozone concentration") +
  geom_point(data=june22[idx, ], aes(fill=factor(idSensore)), pch=21, col=1, size=2.5)


# example
data22 = dati[which(dates$year == 122), ]
prova = xtabs(Valore > 180 ~ Data$mon+idSensore, data=data22)
prova = as.data.frame(prova)

# barplot
ggplot(prova, aes(x=Data.mon, y=Freq, fill=idSensore, color=idSensore)) +
  geom_bar(position = 'identity', stat="identity", alpha = 0.3) + 
  theme(legend.position = "none")

# interpolation
ggplot(prova, aes(x=Data.mon, y=Freq, fill=idSensore, color=idSensore)) +
  geom_point(size=4, pch=21, col=1) + 
  geom_line(aes(x=as.numeric(Data.mon)),lwd=0.7, lty=2) + 
  theme(legend.position = "none") + xlab("month") + ylab("occurrence")
