# Necessary libraries -----------------------------------------------------
library(tidyr)
library(readxl)
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
library(lubridate)
library(rnaturalearth)
library(htmltools)
library(reshape2)
library(tidyverse)

stazioni.usate = read.csv("./Dati_iniziali/51stazioni_O3.csv")


# Zone variable ----------------------------------------------------------

stazioni.usate <- stazioni.usate %>%
  mutate(Zone = NA)

# Zone A: Pianura urbanizzata
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(5707, 10025, 9899, 9925, 5730, 10041, 10288, 10454, 6832, 5718, 6691, 9972, 5750, 12020, 20041), "A", Zone))
# Zone B: Pianura 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(9856, 5738, 10463, 5952, 5960, 9997, 6818, 5721, 10081, 10171, 6884, 9861, 20154), "B", Zone))
# Zone C1: Prealpi, Appennino 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(5732, 9991, 10584), "C1", Zone))
# Zone C2: Montagna 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(5739), "C2", Zone))
# Zone D: Fondovalle 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(10270, 5742, 10437), "D", Zone))
# Zone Mi: Agglomerato di Milano 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(6665, 5710, 6582, 5719, 5725, 5749, 9882, 5717, 10282, 5735, 17295, 17288), "Mi", Zone))
# Zone Bg: Agglomerato di Bergamo 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(6904, 17297), "Bg", Zone))
# Zone Br: Agglomerato di Brescia 
stazioni.usate <- stazioni.usate %>%
  mutate(Zone = ifelse(IdSensore %in% c(6804, 30165), "Br", Zone))


# Area & Type variables ----------------------------------------------------------

data <- read_xlsx("./Covariate_0/Elenco_stazioni.xlsx")

data <- data %>%
  separate(TIPO_EOI, into = c("Area", "Type"), sep = 1)

# change names in data to be matched with stazioni.usate

data <- data %>%
  mutate(NOME_STAZ = gsub("Abbadia Cerreto", "Abbadia Cerreto v. Fanfulla", NOME_STAZ),
         NOME_STAZ = gsub("Arconate", "Arconate v. De Gasperi", NOME_STAZ),
         NOME_STAZ = gsub("Bergamo - via Meucci", "Bergamo v.Meucci", NOME_STAZ),
         NOME_STAZ = gsub("Bertonico", "Bertonico v.Moro", NOME_STAZ),
         NOME_STAZ = gsub("Bormio", "Bormio v.Monte Braulio", NOME_STAZ),
         NOME_STAZ = gsub("Brescia - Villaggio Sereno", "Brescia Villaggio Sereno", NOME_STAZ),
         NOME_STAZ = gsub("Busto Arsizio - Accam", "Busto Arsizio  Accam", NOME_STAZ),
         NOME_STAZ = gsub("Calusco d'Adda", "Calusco d'Adda v. Caduti sul Lavoro", NOME_STAZ),
         NOME_STAZ = gsub("Cantù - via Meucci", "Cantù v.Meucci", NOME_STAZ),
         NOME_STAZ = gsub("Casirate d'Adda", "Casirate d'Adda v. Cimitero", NOME_STAZ),
         NOME_STAZ = gsub("Colico", "Colico v.La Madoneta", NOME_STAZ),
         NOME_STAZ = gsub("Como - viale Cattaneo", "Como v.Cattaneo", NOME_STAZ),
         NOME_STAZ = gsub("Cormano", "Cormano v. Edison", NOME_STAZ),
         NOME_STAZ = gsub("Cornale", "Cornale v.Libertà", NOME_STAZ),
         NOME_STAZ = gsub("Corte de' Cortesi", "Corte de' Cortesi con Cignone v. Villa Strada Forca", NOME_STAZ),
         NOME_STAZ = gsub("Crema - via XI febbraio", "Crema v.XI febbraio", NOME_STAZ),
         NOME_STAZ = gsub("Cremona - via Fatebenefratelli", "Cremona Via Fatebenefratelli", NOME_STAZ),
         NOME_STAZ = gsub("Darfo", "Darfo Boario Terme v. De Amicis", NOME_STAZ),
         NOME_STAZ = gsub("Erba", "Erba v. Battisti", NOME_STAZ),
         NOME_STAZ = gsub("Ferno", "Ferno v.Di Dio", NOME_STAZ),
         NOME_STAZ = gsub("Ferrera Erbognone - EST", "Ferrera Erbognone ENI", NOME_STAZ),
         NOME_STAZ = gsub("Gambara", "Gambara v.Parma", NOME_STAZ),
         NOME_STAZ = gsub("Lecco - via Sora", "Lecco v.Sora", NOME_STAZ),
         NOME_STAZ = gsub("Lodi S. Alberto", "Lodi S.Alberto", NOME_STAZ),
         NOME_STAZ = gsub("Lonato", "Lonato del Garda v.del Marchesino", NOME_STAZ),
         NOME_STAZ = gsub("Magenta", "Magenta v. F. Turati", NOME_STAZ),
         NOME_STAZ = gsub("Mantova - S.Agnese", "Mantova S.Agnese", NOME_STAZ),
         NOME_STAZ = gsub("Meda", "Meda v. Gagarin", NOME_STAZ),
         NOME_STAZ = gsub("Merate", "Merate v. Madonna di Loreto", NOME_STAZ),
         NOME_STAZ = gsub("Casoni Borroni", "Mezzana Bigli loc. Casoni", NOME_STAZ), 
         NOME_STAZ = gsub("Milano - Pascal Città Studi", "Milano Pascal Città Studi", NOME_STAZ),
         NOME_STAZ = gsub("Milano - Verziere", "Milano Verziere", NOME_STAZ),
         NOME_STAZ = gsub("Moggio", "Moggio Loc. Penscei", NOME_STAZ),
         NOME_STAZ = gsub("Monza - Parco", "Monza Parco", NOME_STAZ),
         NOME_STAZ = gsub("Monza - via Machiavelli", "Monza v.Machiavelli", NOME_STAZ),
         NOME_STAZ = gsub("Morbegno - via Cortivacci", "Morbegno v.Cortivacci", NOME_STAZ),
         NOME_STAZ = gsub("Motta Visconti", "Motta Visconti v. De Gasperi", NOME_STAZ),
         NOME_STAZ = gsub("Osio Sotto", "Osio Sotto v.per Levate", NOME_STAZ),
         NOME_STAZ = gsub("Pavia - via Folperti", "Pavia v. Folperti", NOME_STAZ),
         NOME_STAZ = gsub("Perledo - piazza A. Moro", "Perledo P.zza della stazione", NOME_STAZ),
         NOME_STAZ = gsub("Limito", "Pioltello Limito", NOME_STAZ),
         NOME_STAZ = gsub("Ponti sul Mincio", "Ponti sul Mincio v.San Martino", NOME_STAZ),
         NOME_STAZ = gsub("Sarezzo - via Minelli", "Sarezzo v.Minelli", NOME_STAZ),
         NOME_STAZ = gsub("Saronno - via Santuario", "Saronno v.Santuario", NOME_STAZ),
         NOME_STAZ = gsub("Schivenoglia", "Schivenoglia v. Malpasso", NOME_STAZ),
         NOME_STAZ = gsub("Sondrio - via Paribelli", "Sondrio v.Paribelli", NOME_STAZ),
         NOME_STAZ = gsub("Spinadesco", "Spinadesco v.Battisti", NOME_STAZ),
         NOME_STAZ = gsub("Valmadrera", "Valmadrera v.Pozzi", NOME_STAZ),
         NOME_STAZ = gsub("Varese - Vidoletti", "Varese v.Vidoletti", NOME_STAZ),
         NOME_STAZ = gsub("Viadana", "Viadana v. Cavalli", NOME_STAZ),
         NOME_STAZ = gsub("Voghera - via Pozzoni", "Voghera v.Pozzoni", NOME_STAZ))

data_staz <- inner_join(data, stazioni.usate, by = c("NOME_STAZ" = "NomeStazione"))


# remove unnecessary columns
data_staz$PROV. <- NULL
data_staz$COMUNE <- NULL
data_staz$ZCODE <- NULL
data_staz$NOME_STAZ <- NULL
data_staz$`UTM-X` <- NULL
data_staz$`UTM-Y` <- NULL
data_staz$X <- NULL

# write.csv(data_staz, "./Datasets_2000_23/Sensors.csv", row.names = FALSE)


# Map stations O3 ----------------------------------------------------------

data_staz <- read.csv("./Datasets_2000_23/Sensors.csv")

italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())


## ZONE
# color stations wrt their Zone
data_staz <- data_staz %>%
  mutate(color1 = case_when(
    Zone == "A" ~ "#8B4513",
    Zone == "B" ~ "#16A34A",
    Zone == "C1" ~ "#87CEEB",
    Zone == "C2" ~ "#6A5ACD",
    Zone == "D" ~ "#2F4F4F",
    TRUE ~ "#FF6347" # Mi, Bg, Br same color
  ))

zone_colors <- c("Cities" = "#FF6347", "A" = "#8B4513", "B" = "#16A34A", 
                 "C1" = "#87CEEB", "C2" = "#6A5ACD", "D" = "#2F4F4F")
zone_labels <- names(zone_colors)

mappa_lombardia_zone <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = map_lombardia, 
              stroke = T, 
              opacity = 1, 
              color = '#293133',
              fillOpacity = .3, 
              fill = T, 
              fillColor = '#293133',
              dashArray = "1", 
              weight = 2.5)   %>%
  addCircleMarkers(data = data_staz, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color1,
                   radius = 4,
                   label = data_staz$IdSensore) %>%
  # addLabelOnlyMarkers(data = stazioni.usate, 
                      # ~lng, 
                      # ~lat,
                      # label = stazioni.usate$IdSensore) %>%
  addLegend(position = "topright", 
            colors = zone_colors, 
            labels = zone_labels, 
            title = "Zone",
            opacity = 1) %>%
  setView(lng = 9.1900, lat = 45.4642, zoom = 8)
mappa_lombardia_zone


## AREA
# color stations wrt their Area
data_staz <- data_staz %>%
  mutate(color2 = case_when(
    Area == "R" ~ "#006400",
    Area == "S" ~ "#1E90FF",
    TRUE ~ "#FF4500" 
  ))

area_colors <- c("R" = "#006400", "S" = "#1E90FF", 
                 "U" = "#FF4500")
area_labels <- c("R", "S", "U")

mappa_lombardia_area <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = map_lombardia, 
              stroke = T, 
              opacity = 1, 
              color = '#293133',
              fillOpacity = .3, 
              fill = T, 
              fillColor = '#293133',
              dashArray = "1", 
              weight = 2.5)   %>%
  addCircleMarkers(data = data_staz, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color2,
                   radius = 4,
                   label = data_staz$IdSensore) %>%
  # addLabelOnlyMarkers(data = stazioni.usate, 
  # ~lng, 
  # ~lat,
  # label = stazioni.usate$IdSensore) %>%
  addLegend(position = "topright", 
            colors = area_colors, 
            labels = area_labels, 
            title = "Area",
            opacity = 1) %>%
  setView(lng = 9.1900, lat = 45.4642, zoom = 8)
mappa_lombardia_area


## TYPE
# color stations wrt their Area
data_staz <- data_staz %>%
  mutate(color3 = case_when(
    Type == "B" ~ "#7FFF00",
    Type == "I" ~ "#8A2BE2",
    TRUE ~ "#FF8C00" 
  ))

type_colors <- c("B" = "#7FFF00", "I" = "#8A2BE2", "T" = "#FF8C00")
type_labels <- c("B", "I", "T")

mappa_lombardia_type <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = map_lombardia, 
              stroke = T, 
              opacity = 1, 
              color = '#293133',
              fillOpacity = .3, 
              fill = T, 
              fillColor = '#293133',
              dashArray = "1", 
              weight = 2.5)   %>%
  addCircleMarkers(data = data_staz, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color3,
                   radius = 4,
                   label = data_staz$IdSensore) %>%
  # addLabelOnlyMarkers(data = stazioni.usate, 
  # ~lng, 
  # ~lat,
  # label = stazioni.usate$IdSensore) %>%
  addLegend(position = "topright", 
            colors = type_colors, 
            labels = type_labels, 
            title = "Type",
            opacity = 1) %>%
  setView(lng = 9.1900, lat = 45.4642, zoom = 8)
mappa_lombardia_type


# Weather variables ----------------------------------------------------------

monthly_means_list <- list()

map_beaufort <- function(x) {
  nodi <- x / 1.852
  if (nodi == 0) {
    return(0)
  } else if (nodi <= 3) {
    return(1)
  } else if (nodi <= 6) {
    return(2)
  } else if (nodi <= 10) {
    return(3)
  } else if (nodi <= 16) {
    return(4)
  } else if (nodi <= 21) {
    return(5)
  } else if (nodi <= 27) {
    return(6)
  } else if (nodi <= 33) {
    return(7)
  } else {
    return("Wind speed is too high for Beaufort scale")
  }
}


for (i in 1:51) {
  staz <- read.csv(paste0("./Covariate_0/Meteo/staz", i, ".csv"), skip = 2, header = T)
  
  staz$time <- ymd(staz$time)
  staz$Year <- year(staz$time)
  staz$Month <- month(staz$time)
  staz$Day <- day(staz$time)
  staz$beaufort = sapply(staz$wind_speed_10m_max..km.h, map_beaufort)
  staz <- staz[which(staz$Month %in% 4:10), ]
  
  monthly.means <- staz %>%
    group_by(Year, Month) %>%
    summarize(
      mean_temperature = mean(temperature_2m_mean...C.),
      mean_precipitation_sum = mean(precipitation_sum..mm.),
      mean_precipitation_hours = mean(precipitation_hours..h.),
      mean_windspeed_10m_max = mean(wind_speed_10m_max..km.h.),
      mean_radiation_sum = mean(shortwave_radiation_sum..MJ.m..),
      count_highwind = mean(beaufort)
    ) %>%
    ungroup()
  monthly_means_list[[i]] <- monthly.means
  
  # calculate the number of consecutive days with Beaufort > 4 for each month
  staz <- staz %>%
    group_by(Year, Month) %>%
    mutate(
      consecutive_highwind_days = sequence(rle(beaufort > 3)$lengths) * (beaufort > 3)
    ) %>%
    ungroup()
  
  # take the maximum number of consecutive days with Beaufort > 4 for each month
  monthly_max_consecutive_days <- staz %>%
    group_by(Year, Month) %>%
    summarize(max_consecutive_highwind_days = max(consecutive_highwind_days))
  
  monthly_means_list[[i]] <- left_join(monthly_means_list[[i]], monthly_max_consecutive_days, by = c("Year", "Month"))
}


meteo <- data.frame()
for (i in 1:51) {
  monthly_means_list[[i]]$Station <- i
  meteo <- bind_rows(meteo, monthly_means_list[[i]])
}

sapply(meteo, function(y) sum(length(which(is.na(y))))) # no NAs


for (i in 1:51) {
  meteo[which(meteo$Station == i), 10] <- stazioni.usate$IdSensore[i]
}


# Density variable ----------------------------------------------------------

pop.1 <- read.csv("./Covariate_0/Popolazione/popolazione_2000.csv")
pop.2 <- read.csv("./Covariate_0/Popolazione/popolazione_2001_2018.csv")
pop.3 <- read.csv("./Covariate_0/Popolazione/popolazione_2019_2023.csv")

pop <- rbind(rbind(pop.1[, c(2,7,9)], pop.2[, c(2,11,13)]), pop.3[, c(2,11,13)])
rm(pop.1, pop.2, pop.3)

pop <- pop %>%
  mutate(Territorio = recode(Territorio,
                          "Cornale e Bastida" = "Cornale"))
                          

density = stazioni.usate[, c("IdSensore", "Comune")]
comuni = unique(density$Comune) 

pop_wide <- pop %>%
  pivot_wider(names_from = TIME, values_from = Value)
rm(pop)

density <- density %>%
  left_join(pop_wide, by = c("Comune" = "Territorio"))


# Total covariates ----------------------------------------------------------

density_long <- melt(density, id.vars = c("IdSensore", "Comune"), variable.name = "Year", value.name = "Density")
density_long$Year <- as.numeric(gsub("X", "", as.character(density_long$Year)))
rm(density)

# join meteo and density
combined_df <- left_join(meteo, density_long, by = c("Station" = "IdSensore", "Year"))
combined_df$Comune <- NULL
rm(density_long, meteo)

# add zone and altitude
combined_df <- left_join(combined_df, stazioni.usate[, c(2,7,18)], by = c("Station" = "IdSensore"))
combined_df$Quota <- ifelse(is.na(combined_df$Quota), 16, combined_df$Quota)

# add area & type to final dataset
combined_df <- left_join(combined_df, data_staz[, 1:3], by = c("Station" = "IdSensore"))

combined_df <- combined_df[order(combined_df$Station),]

# write.csv(combined_df, "./Datasets_2000_23/Covariates.csv", row.names = FALSE)

