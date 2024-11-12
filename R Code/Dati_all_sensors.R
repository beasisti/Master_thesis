# Necessary libraries -----------------------------------------------------
library(ggplot2)
library(tibble)
library(readr)
library(readxl)
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
library(zoo)
library(gdata)
library(caret)
library(geosphere)
library(viridis)


# 180 data --------------------------------------------------------------------

dati180.1 <- read_csv('./Datasets_2000_23/Dataset_180.csv')
dati180.2 <- read_csv('./Datasets_2007_23/Dataset_180.csv')
dati180.3 <- read_csv('./Datasets_2012_23/Dataset_180.csv')

stations_2000_2007 <- unique(dati180.1$idSensore)
stations_2007_2012 <- setdiff(unique(dati180.2$idSensore), stations_2000_2007)
stations_2012_2023 <- setdiff(unique(dati180.3$idSensore), c(stations_2000_2007, stations_2007_2012))

create_na_block <- function(stations, start_year, end_year, months) {
  expand.grid(idSensore = stations, Year = start_year:end_year, Month = months, Count_180 = NA)
}

na_block_2007 <- create_na_block(stations_2007_2012, 2000, 2006, unique(dati180.1$Month))
na_block_2012 <- create_na_block(stations_2012_2023, 2000, 2011, unique(dati180.1$Month))

dati180.2 <- dati180.2 %>% filter(idSensore %in% stations_2007_2012)
dati180.3 <- dati180.3 %>% filter(idSensore %in% stations_2012_2023)

combined_data <- rbind(dati180.1, na_block_2007, dati180.2, na_block_2012, dati180.3)

combined_data <- combined_data %>%
  mutate(
    station_order = case_when(
      idSensore %in% stations_2000_2007 ~ 1,
      idSensore %in% stations_2007_2012 ~ 2,
      idSensore %in% stations_2012_2023 ~ 3
    )
  ) %>%
  arrange(station_order, idSensore, Year, Month)

combined_data$station_order <- NULL

# write.csv(combined_data, "./Datasets_all_sensors/Dataset_180.csv", row.names = FALSE)

rm(list = ls())


## Plot responses 2000_2023  -----------------------------------------------------

count_180_df <- read.csv("./Datasets_all_sensors/Dataset_180.csv")

wrapped180 = count_180_df %>%
  pivot_wider(names_from = idSensore, values_from = Count_180, values_fill = 0)

matplot(wrapped180[-c(1,2)], type = 'l', xlab = 'Years', ylab = "Day count", ylim = c(0,31),
        xaxt = "n", main = "Days with max > 180")
labels <- c("2000","2001","2002","2003","2004","2005","2006","2007",
            "2008","2009","2010","2011","2012","2013","2014","2015",
            "2016","2017","2018","2019","2020","2021","2022","2023")
num_years <- length(labels)
total_points <- nrow(wrapped180)
points_per_year <- total_points / num_years
ticks <- seq(points_per_year / 2, total_points, by = points_per_year)
axis(1, at = ticks, labels = labels)
abline(v = seq(points_per_year, total_points - points_per_year / 2, by = points_per_year), lty = 4, col = "darkgray")


data_aggregated <- count_180_df %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

ggplot(data_aggregated, aes(x = Date.mon, y = Count_180, fill = as.factor(idSensore), color = as.factor(idSensore))) +
  geom_line(aes(group = idSensore), lwd = 0.5, lty = 1) +
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(x = "Years",
       y = "Occurrence") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Ruota e regola la dimensione delle etichette dell'asse x
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8))
# 12 x 5

rm(list = ls())

# 120 data --------------------------------------------------------------------

dati120.1 <- read_csv('./Datasets_2000_23/Dataset_120.csv')
dati120.2 <- read_csv('./Datasets_2007_23/Dataset_120.csv')
dati120.3 <- read_csv('./Datasets_2012_23/Dataset_120.csv')

stations_2000_2007 <- unique(dati120.1$idSensore)
stations_2007_2012 <- setdiff(unique(dati120.2$idSensore), stations_2000_2007)
stations_2012_2023 <- setdiff(unique(dati120.3$idSensore), c(stations_2000_2007, stations_2007_2012))

create_na_block <- function(stations, start_year, end_year, months) {
  expand.grid(idSensore = stations, Year = start_year:end_year, Month = months, Count_120 = NA)
}

na_block_2007 <- create_na_block(stations_2007_2012, 2000, 2006, unique(dati120.1$Month))
na_block_2012 <- create_na_block(stations_2012_2023, 2000, 2011, unique(dati120.1$Month))

dati120.2 <- dati120.2 %>% filter(idSensore %in% stations_2007_2012)
dati120.3 <- dati120.3 %>% filter(idSensore %in% stations_2012_2023)

combined_data <- rbind(dati120.1, na_block_2007, dati120.2, na_block_2012, dati120.3)

combined_data <- combined_data %>%
  mutate(
    station_order = case_when(
      idSensore %in% stations_2000_2007 ~ 1,
      idSensore %in% stations_2007_2012 ~ 2,
      idSensore %in% stations_2012_2023 ~ 3
    )
  ) %>%
  arrange(station_order, idSensore, Year, Month)

combined_data$station_order <- NULL

# write.csv(combined_data, "./Datasets_all_sensors/Dataset_120.csv", row.names = FALSE)


## Plot responses 2000_2023  -----------------------------------------------------

count_120_df <- read.csv("./Datasets_all_sensors/Dataset_120.csv")

wrapped120 = count_120_df %>%
  pivot_wider(names_from = idSensore, values_from = Count_120, values_fill = 0)

matplot(wrapped120[-c(1,2)], type = 'l', xlab = 'Years', ylab = "Day count", ylim = c(0,31),
        xaxt = "n", main = "Days with MovAvg > 120")
labels <- c("2000","2001","2002","2003","2004","2005","2006","2007",
            "2008","2009","2010","2011","2012","2013","2014","2015",
            "2016","2017","2018","2019","2020","2021","2022","2023")
num_years <- length(labels)
total_points <- nrow(wrapped120)
points_per_year <- total_points / num_years
ticks <- seq(points_per_year / 2, total_points, by = points_per_year)
axis(1, at = ticks, labels = labels)
abline(v = seq(points_per_year, total_points - points_per_year / 2, by = points_per_year), lty = 4, col = "darkgray")

data_aggregated <- count_120_df %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

ggplot(data_aggregated, aes(x = Date.mon, y = Count_120, fill = as.factor(idSensore), color = as.factor(idSensore))) +
  geom_line(aes(group = idSensore), lwd = 0.5, lty = 1) +
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(x = "Years",
       y = "Occurrence") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Ruota e regola la dimensione delle etichette dell'asse x
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8))
# 12 x 5


rm(list = ls())

# Filter covariates  ------------------------------------------------------------------------------------

X <- read_csv('./Datasets_2000_23/Covariates.csv')

# check which kind of zones we have
unique_zones <- X %>%
  distinct(Station, Zone)
table(unique_zones$Zone) 

# modify the column Zone: C1, C2 become C
X <- X %>%
  mutate(Zone = case_when(Zone %in% c("C1", "C2", "D") ~ "C", TRUE ~ Zone))

# Bg, Br, Mi become Cities
X <- X %>%
  mutate(Zone = case_when(Zone %in% c("Bg", "Br", "Mi") ~ "Cities", TRUE ~ Zone))

# check which kind of areas we have
unique_areas <- X %>%
  distinct(Station, Area)
table(unique_areas$Area) 

# check which kind of types we have
unique_types <- X %>%
  distinct(Station, Type)
table(unique_types$Type) 

# I becomes T
X <- X %>%
  mutate(Type = case_when(Type %in% c("I") ~ "T", TRUE ~ Type))


# standardize numerical variables
colonne_da_standardizzare <- c("mean_temperature", "mean_precipitation_sum", 
                               "mean_precipitation_hours", "mean_windspeed_10m_max",
                               "mean_radiation_sum", "count_highwind", "max_consecutive_highwind_days", 
                               "Density", "Quota")
X[, colonne_da_standardizzare] <- scale(X[, colonne_da_standardizzare])

# If you need to run the code to get the response for zone/area/type model, go directly 
# to section Construction Y for zone/area/type model

# dummy for Zone
baseline <- "C" # mountain
X$Zone <- as.factor(X$Zone)
X$Zone <- factor(X$Zone, levels = c(baseline, setdiff(levels(X$Zone), baseline)))
zone_dummies <- model.matrix(~ Zone - 1, data = X)
zone_dummies <- zone_dummies[, -1]
colnames(zone_dummies) <- paste("Zone", levels(X$Zone)[-1], sep = "_")
X_final <- cbind(X, zone_dummies)
X_final <- X_final[, !names(X_final) %in% "Zone"]

# dummy for Area
baseline <- "R" # rural
X$Area <- as.factor(X$Area) 
X$Area <- factor(X$Area, levels = c(baseline, setdiff(levels(X$Area), baseline))) 
area_dummies <- model.matrix(~ Area - 1, data = X)
area_dummies <- area_dummies[, -1]
colnames(area_dummies) <- paste("Area", levels(X$Area)[-1], sep = "_")
X_final <- cbind(X_final, area_dummies)
X_final <- X_final[, !names(X_final) %in% "Area"]

# dummy for Type
baseline <- "B" # background
X$Type <- as.factor(X$Type) 
X$Type <- factor(X$Type, levels = c(baseline, setdiff(levels(X$Type), baseline))) 
Type_T <- model.matrix(~ Type - 1, data = X)
Type_T <- Type_T[, -1]
X_final <- cbind(X_final, Type_T)
X_final <- X_final[, !names(X_final) %in% "Type"]

X_final <- X_final %>%
  mutate(
    station_order = case_when(
      Station %in% stations_2000_2007 ~ 1,
      Station %in% stations_2007_2012 ~ 2,
      Station %in% stations_2012_2023 ~ 3
    )
  ) %>%
  arrange(station_order, Station, Year, Month)

X_final$station_order <- NULL

# write.csv(X_final, "./Datasets_all_sensors/variables_to_select.csv", row.names = FALSE)


# Construction of Y for zone/area/type model  ------------------------------------------------------------------------------------

Y180 <- read_csv('./Datasets_all_sensors/Dataset_180.csv')
Y120 <- read_csv('./Datasets_all_sensors/Dataset_120.csv')

Y180 <- Y180 %>%
  left_join(X %>% dplyr::select(Station, Year, Month, Zone, Area, Type), 
            by = c("idSensore" = "Station", "Year", "Month"))

Y120 <- Y120 %>%
  left_join(X %>% dplyr::select(Station, Year, Month, Zone, Area, Type), 
            by = c("idSensore" = "Station", "Year", "Month"))

# write.csv(Y180, "./Datasets_all_sensors/180_zone_area_type.csv")
# write.csv(Y120, "./Datasets_all_sensors/120_zone_area_type.csv")


# Distance matrix  ------------------------------------------------------------------------------------

stations <- read_csv('./Dati_iniziali/51stazioni_O3.csv')

stations <- stations %>%
  mutate(
    station_order = case_when(
      IdSensore %in% stations_2000_2007 ~ 1,
      IdSensore %in% stations_2007_2012 ~ 2,
      IdSensore %in% stations_2012_2023 ~ 3
    )
  ) %>%
  arrange(station_order, IdSensore)

stations$station_order <- NULL

dist_mat <- matrix(0, ncol = dim(stations)[1], nrow = dim(stations)[1])
for (i in 1:dim(stations)[1])
{
  for (j in i:dim(stations)[1])
  {
    dist_mat[i,j] <- dist_mat[j,i] <- distGeo(c(stations$lat[i], stations$lng[i]), c(stations$lat[j], stations$lng[j]))/1000
  }
}

# write.csv(dist_mat, "./Datasets_all_sensors/distances.csv")


## Computing max distance for space model ------------------------

sensori <- data.frame(stations$lng, stations$lat)
distanze <- distm(sensori, fun = distHaversine)
distanza_massima <- max(distanze[upper.tri(distanze)])
km <- distanza_massima/1000

rho <- km/3 # 66

rm(list = ls())


# Map stations -----------------------------------------------------------------------

stazioni.usate = read.csv("./Dati_iniziali/51stazioni_O3.csv")

determina_inizio <- function(station_id) {
  if (station_id %in% stations_2000_2007) {
    return(2000)
  } else if (station_id %in% stations_2007_2012) {
    return(2007)
  } else if (station_id %in% stations_2012_2023) {
    return(2012)
  } else {
    return(NA)
  }
}

stazioni.usate$anno_inizio <- sapply(stazioni.usate$IdSensore, determina_inizio)

stazioni.usate <- stazioni.usate %>%
  mutate(color1 = case_when(
    anno_inizio == 2000 ~ "#FF0000",
    anno_inizio == 2007 ~ "#0000FF",
    anno_inizio == 2012 ~ "#00FF00"
  ))

anno_colors <- c("2000" = "#FF0000", "2007" = "#0000FF", "2012" = "#00FF00")
anno_labels <- names(anno_colors)

italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())

mappa_lombardia <- leaflet() %>%
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
  addCircleMarkers(data = stazioni.usate, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color1,
                   radius = 4,
                   label = stazioni.usate$IdSensore) %>%
  # addLabelOnlyMarkers(data = stazioni.usate, 
  # ~lng, 
  # ~lat,
  # label = stazioni.usate$IdSensore) %>%
  addLegend(position = "topright", 
            colors = anno_colors, 
            labels = anno_labels, 
            title = "Activation Year",
            opacity = 1) %>%
  setView(lng = 9.1900, lat = 45.4642, zoom = 8)
mappa_lombardia


# Bivariate dataset -----------------------------------------------

ozono_180 <- read_csv('./Datasets_all_sensors/Dataset_180.csv')
ozono_120 <- read_csv('./Datasets_all_sensors/Dataset_120.csv')

ozono_tot <- ozono_180 %>%
  left_join(ozono_120 %>% select(idSensore, Year, Month, Count_120), 
            by = c("idSensore", "Year", "Month"))

# write.csv(ozono_tot, "./Datasets_all_sensors/Dataset_bivariate.csv")

casi_na1 <- ozono_tot[is.na(ozono_tot$Count_180) & is.na(ozono_tot$Count_120), ]
casi_na2 <- ozono_tot[is.na(ozono_tot$Count_180) | is.na(ozono_tot$Count_120), ]

