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


# Select stations 2000_2023  -----------------------------------------------------

ozono <- read_csv('./Dati_iniziali/O3_completed.csv')

# I take the stations with observations across this whole period
data_apr_1_2000 <- ozono %>%
  filter(timestamp >= as.POSIXct("2000-04-01 00:00:00") & timestamp < as.POSIXct("2000-04-02 00:00:00"))
stations_apr_1_2000 <- data_apr_1_2000 %>%
  filter(!is.na(Stato)) %>%
  pull(idSensore) %>%
  unique()
stations_apr_1_2000 <- c(stations_apr_1_2000, c(5739, 5750, 5738, 5742, 6691)) # from the NA_image

final_ozono <- ozono %>%
  filter(idSensore %in% stations_apr_1_2000)
# these are all the stations for which we have data in 2000-2023

rm(ozono, data_apr_1_2000)


# Map of stations 2000_2023  -----------------------------------------------------

stazioni.usate = read.csv("./Dati_iniziali/51stazioni_O3.csv")
stazioni.usate <- stazioni.usate %>%
  filter(IdSensore %in% stations_apr_1_2000)
X <- read_csv('./Datasets_2000_23/Covariates.csv')
X <- X %>%
  filter(Station %in% stations_apr_1_2000)
stazioni.usate$Zone <- X$Zone[match(stazioni.usate$IdSensore, X$Station)]
stazioni.usate$Area <- X$Area[match(stazioni.usate$IdSensore, X$Station)]
stazioni.usate$Type <- X$Type[match(stazioni.usate$IdSensore, X$Station)]

italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())

## ZONE
# color stations wrt their zone
stazioni.usate <- stazioni.usate %>%
  mutate(color = case_when(
    Zone == "A" ~ "darkorange",
    Zone == "B" ~ "#F8FB63",
    Zone == "C1" ~ "blue",
    Zone == "C2" ~ "cyan",
    Zone == "D" ~ "black",
    TRUE ~ "#E75480" # Mi, Bg, Br same color
  ))

zone_colors <- c("A" = "darkorange", "B" = "#F8FB63", 
                 "C1" = "blue", "C2" = "cyan", "D" = "black",
                 "Cities" = "#E75480")
zone_labels <- names(zone_colors)

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
                   fillColor = ~color,
                   radius = 4,
                   label = stazioni.usate$IdSensore) %>%
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
mappa_lombardia


## AREA
# color stations wrt their Area
stazioni.usate <- stazioni.usate %>%
  mutate(color2 = case_when(
    Area == "U" ~ "red",
    Area == "S" ~ "#F8FB63",
    TRUE ~ "lightgreen" 
  ))

area_colors <- c("U" = "red", "S" = "#F8FB63", 
                 "R" = "lightgreen")
area_labels <- c("Urban", "Suburban", "Rural")

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
  addCircleMarkers(data = stazioni.usate, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color2,
                   radius = 4,
                   label = stazioni.usate$IdSensore) %>%
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
stazioni.usate <- stazioni.usate %>%
  mutate(color3 = case_when(
    Type == "T" ~ "darkred",
    Type == "B" ~ "pink",
    TRUE ~ "cyan" 
  ))

type_colors <- c("T" = "darkred", "B" = "pink", 
                 "I" = "cyan")
type_labels <- c("Traffic", "Background", "Industrial")

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
  addCircleMarkers(data = stazioni.usate, 
                   ~lng, 
                   ~lat, 
                   stroke = TRUE, 
                   color = "black", 
                   weight = 1.5, 
                   fillOpacity = 1,
                   fillColor = ~color3,
                   radius = 4,
                   label = stazioni.usate$IdSensore) %>%
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


# 180 model  ------------------------------------------------------------------------------------
## Dealing with NAs 2000_2023  -----------------------------------------------------

massimi <- final_ozono %>%
  group_by(idSensore, Year, Month, Day) %>%
  summarize(
    max = ifelse(
      sum(is.na(Valore)) < 6,
      max(Valore, na.rm = TRUE),
      ifelse(
        any(Valore[!is.na(Valore)] >= 180),
        max(Valore, na.rm = TRUE),
        -1
      )
    )
  ) %>%
  ungroup()

# Filling the vector mm_na with whether a month is admissible or not
# a month is admissible iff it has >= 6 non NA (ie -1) for max variable
mm_na <- NULL
for (s in unique(massimi$idSensore)) {
  for (y in unique(massimi$Year[which(massimi$idSensore == s)])) {
    for (m in unique(massimi$Month[which(massimi$idSensore == s & massimi$Year == y)])) {
      if (sum(massimi$max[which(massimi$idSensore == s & massimi$Year == y & massimi$Month == m)] == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, s, y, m))
      } else {
        mm_na <- rbind(mm_na, c(0, s, y, m))
      }
    }
  }
}
mm_na <- data.frame(mm_na)
colnames(mm_na) <- c("Admissible", "idSensore", "Year", "Month")

# filling massimi replacing -1 values with a linear
# interpolation of the 2 nearest admissible maximums

findFirstDay <- function(row, df) {
  while (row <= nrow(df)) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row + 1
  }
  return(row)
}

findLastDay <- function(row, df) {
  while (row >= 1) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row - 1
  }
  return(row)
}

maximum_df <- NULL
for (s in unique(massimi$idSensore)) {
  temp_df_id <- massimi[which(massimi$idSensore == s), ]
  for (y in unique(temp_df_id$Year)) {
    temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
    first_adm <- findFirstDay(1, temp_df)
    last_adm <- findLastDay(nrow(temp_df), temp_df)
    if (first_adm != 1 & first_adm != (nrow(temp_df) + 1)) {
      temp_df[1, "max"] <- temp_df[first_adm, "max"]
    }
    if (last_adm != nrow(temp_df) & last_adm != 0) {
      temp_df[nrow(temp_df), "max"] <- temp_df[last_adm, "max"]
    }
    if (first_adm == (nrow(temp_df) + 1) | last_adm == 0) {
      temp_df[1, "max"] <- -1
      temp_df[nrow(temp_df), "max"] <- -1
      print(paste("For sensor ", s, " the year ", y, " is not admissible"))
    } else {
      for (row in seq_len(nrow(temp_df))) {
        if (temp_df[row, "max"] == -1) {
          prev_adm <- findLastDay(row, temp_df)
          next_adm <- findFirstDay(row, temp_df)
          temp_df[row, "max"] <- (temp_df[next_adm, "max"] - temp_df[prev_adm, "max"]) /
            (next_adm - prev_adm) * (row - prev_adm) + temp_df[prev_adm, "max"]
        }
      }
    }
    maximum_df <- rbind(maximum_df, temp_df)
  }
}
maximum_df <- data.frame(maximum_df)

# Placing NAs when a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
                       maximum_df$Year == mm_na[i, "Year"] &
                       maximum_df$Month == mm_na[i, "Month"]), "max"] <- NA
  }
}

count_180_df <- NULL
for (s in unique(maximum_df$idSensore)) {
  temp_df_id <- maximum_df[which(maximum_df$idSensore == s), ]
  for (y in 2000:2023) {
    if (y %in% unique(temp_df_id$Year)) {
      temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
      for (m in 4:10) {
        if (m %in% unique(temp_df$Month)) {
          temp_df_m <- temp_df[which(temp_df$Month == m), ]
          count_180_df <- rbind(count_180_df, c(sum(temp_df_m$max >= 180), s, y, m))
        } else {
          count_180_df <- rbind(count_180_df, c(NA, s, y, m))
        }
      }
    } else {
      for (m in 4:10) {
        count_180_df <- rbind(count_180_df, c(NA, s, y, m))
      }
    }
  }
}

count_180_df <- data.frame(count_180_df)
colnames(count_180_df) <- c("Count_180", "idSensore", "Year", "Month")

# write.csv(count_180_df, "./Datasets_2000_23/Dataset_180.csv", row.names = FALSE)

keep(stations_apr_1_2000, sure = TRUE)


## Plot NAs 2000_2023  -----------------------------------------------------

count_180_df <- read.csv("./Datasets_2000_23/Dataset_180.csv")

sensors <- unique(count_180_df$idSensore)
years <- 2000:2023
mesi <- 4:10

sum(is.na(count_180_df$Count_180))
sum(is.na(count_180_df$Count_180)) / nrow(count_180_df) # 0.1130952

sen <- 1:length(sensors)
time <- 1:(length(years) * length(mesi))

nas <- NULL
for (i in sensors) {
  nas <- rbind(nas, as.numeric(is.na(count_180_df$Count_180[count_180_df$idSensore == i])))
}

sum(nas[nrow(nas), ] == 1) / dim(nas)[2]
thre <- rep(0, length(sensors))
for (i in 1:length(sensors)) {
  thre[i] <- sum(nas[i, ] == 1) / dim(nas)[2]
}
plot(thre, xlab = 'Station', ylab = '% NAs', ylim = c(0,1))
abline(h = 0.1, col = 'red', lty = 2)


## Plot responses 2000_2023  -----------------------------------------------------

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


keep(stations_apr_1_2000, sure = TRUE)

# 120 model  ------------------------------------------------------------------------------------

ozono_120 <- read_csv('./Dati_iniziali/O3_all_months.csv')
ozono_120 <- ozono_120 %>%
  filter(idSensore %in% stations_apr_1_2000)

## Dealing with NAs 2000_2023  -----------------------------------------------------

# Create a new dataframe that measures the moving avg of the 8 hours
# MovingAvg = NA if there are > 4 NA

ozono_120 <- ozono_120 %>%
  arrange(idSensore, timestamp) %>%
  group_by(idSensore) %>%
  mutate(
    MovingAvg = rollapply(Valore, width = 8, FUN = function(x) {
      if (sum(!is.na(x)) >= 4) {
        mean(x, na.rm = TRUE)
      } else {
        NA
      }
    }, by = 1, align = "right", fill = NA)
  )
ozono_120 <- ozono_120 %>%
  filter(Month >= 4 & Month <= 10)

# I take the max daily value of MovingAvg if there are > 16 MovingAvg non NA 
# or, if there are >= 8 NA, if at least one of the recorded is > 120

massimi <- ozono_120 %>%
  group_by(idSensore, Year, Month, Day) %>%
  summarize(
    max = ifelse(
      sum(is.na(MovingAvg)) < 6,
      max(MovingAvg, na.rm = TRUE),
      ifelse(
        any(MovingAvg[!is.na(MovingAvg)] >= 120),
        max(MovingAvg, na.rm = TRUE),
        -1
      )
    )
  ) %>%
  ungroup()

# Filling the vector mm_na with whether a month is admissible or not
# a month is admissible iff it has >= 6 non NA (ie -1) for max variable
mm_na <- NULL
for (s in unique(massimi$idSensore)) {
  for (y in unique(massimi$Year[which(massimi$idSensore == s)])) {
    for (m in unique(massimi$Month[which(massimi$idSensore == s & massimi$Year == y)])) {
      if (sum(massimi$max[which(massimi$idSensore == s & massimi$Year == y & massimi$Month == m)] == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, s, y, m))
      } else {
        mm_na <- rbind(mm_na, c(0, s, y, m))
      }
    }
  }
}
mm_na <- data.frame(mm_na)
colnames(mm_na) <- c("Admissible", "idSensore", "Year", "Month")

# filling massimi replacing -1 values with a linear
# interpolation of the 2 nearest admissible maximums
# usiamo lo stesso criterio di riempimento usato per count_180

findFirstDay <- function(row, df) {
  while (row <= nrow(df)) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row + 1
  }
  return(row)
}

findLastDay <- function(row, df) {
  while (row >= 1) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row - 1
  }
  return(row)
}

maximum_df <- NULL
for (s in unique(massimi$idSensore)) {
  temp_df_id <- massimi[which(massimi$idSensore == s), ]
  for (y in unique(temp_df_id$Year)) {
    temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
    first_adm <- findFirstDay(1, temp_df)
    last_adm <- findLastDay(nrow(temp_df), temp_df)
    if (first_adm != 1 & first_adm != (nrow(temp_df) + 1)) {
      temp_df[1, "max"] <- temp_df[first_adm, "max"]
    }
    if (last_adm != nrow(temp_df) & last_adm != 0) {
      temp_df[nrow(temp_df), "max"] <- temp_df[last_adm, "max"]
    }
    if (first_adm == (nrow(temp_df) + 1) | last_adm == 0) {
      temp_df[1, "max"] <- -1
      temp_df[nrow(temp_df), "max"] <- -1
      print(paste("For sensor ", s, " the year ", y, " is not admissible"))
    } else {
      for (row in seq_len(nrow(temp_df))) {
        if (temp_df[row, "max"] == -1) {
          prev_adm <- findLastDay(row, temp_df)
          next_adm <- findFirstDay(row, temp_df)
          temp_df[row, "max"] <- (temp_df[next_adm, "max"] - temp_df[prev_adm, "max"]) /
            (next_adm - prev_adm) * (row - prev_adm) + temp_df[prev_adm, "max"]
        }
      }
    }
    maximum_df <- rbind(maximum_df, temp_df)
  }
}
maximum_df <- data.frame(maximum_df)

# Placing Nas when a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
                       maximum_df$Year == mm_na[i, "Year"] &
                       maximum_df$Month == mm_na[i, "Month"]), "max"] <- NA
  }
}

count_120_df <- NULL
for (s in unique(maximum_df$idSensore)) {
  temp_df_id <- maximum_df[which(maximum_df$idSensore == s), ]
  for (y in 2000:2023) {
    if (y %in% unique(temp_df_id$Year)) {
      temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
      for (m in 4:10) {
        if (m %in% unique(temp_df$Month)) {
          temp_df_m <- temp_df[which(temp_df$Month == m), ]
          count_120_df <- rbind(count_120_df, c(sum(temp_df_m$max >= 120), s, y, m))
        } else {
          count_120_df <- rbind(count_120_df, c(NA, s, y, m))
        }
      }
    } else {
      for (m in 4:10) {
        count_120_df <- rbind(count_120_df, c(NA, s, y, m))
      }
    }
  }
}

count_120_df <- data.frame(count_120_df)
colnames(count_120_df) <- c("Count_120", "idSensore", "Year", "Month")

# write.csv(count_120_df, "./Datasets_2000_23/Dataset_120.csv", row.names = FALSE)

keep(stations_apr_1_2000, sure = TRUE)


## Plot NAs 2000_2023  -----------------------------------------------------

count_120_df <- read.csv("./Datasets_2000_23/Dataset_120.csv")

sensors <- unique(count_120_df$idSensore)
years <- 2000:2023
mesi <- 4:10

sum(is.na(count_120_df$Count_120))
sum(is.na(count_120_df$Count_120)) / nrow(count_120_df) # 0.09993734

sen <- 1:length(sensors)
time <- 1:(length(years) * length(mesi))

nas <- NULL
for (i in sensors) {
  nas <- rbind(nas, as.numeric(is.na(count_120_df$Count_120[count_120_df$idSensore == i])))
}

sum(nas[nrow(nas), ] == 1) / dim(nas)[2]
thre <- rep(0, length(sensors))
for (i in 1:length(sensors)) {
  thre[i] <- sum(nas[i, ] == 1) / dim(nas)[2]
}
plot(thre, xlab = 'Station', ylab = '% NAs', ylim = c(0,1))
abline(h = 0.1, col = 'red', lty = 2)


## Plot responses 2000_2023  -----------------------------------------------------

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

keep(stations_apr_1_2000, sure = TRUE)


# Filter covariates  ------------------------------------------------------------------------------------

X <- read_csv('./Datasets_2000_23/Covariates.csv')
X <- X %>%
  filter(Station %in% stations_apr_1_2000)

# check which kind of zones we have
unique_zones <- X %>%
  distinct(Station, Zone)
table(unique_zones$Zone) 

# modify the column Zone: C1, C2 and D become all C
X <- X %>%
  mutate(Zone = case_when(Zone %in% c("C1", "C2", "D") ~ "C", TRUE ~ Zone))

# check which kind of areas we have
unique_areas <- X %>%
  distinct(Station, Area)
table(unique_areas$Area) 

# check which kind of types we have
unique_types <- X %>%
  distinct(Station, Type)
table(unique_types$Type) 


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


# write.csv(X_final, "./Datasets_2000_23/variables_to_select.csv", row.names = FALSE)

keep(stations_apr_1_2000, sure = TRUE)

# Construction of Y for zone/area/type model  ------------------------------------------------------------------------------------

Y180 <- read_csv('./Datasets_2000_23/Dataset_180.csv')
Y120 <- read_csv('./Datasets_2000_23/Dataset_120.csv')

Y180 <- Y180 %>%
  left_join(X %>% select(Station, Year, Month, Zone, Area, Type), 
            by = c("idSensore" = "Station", "Year", "Month"))

Y120 <- Y120 %>%
  left_join(X %>% select(Station, Year, Month, Zone, Area, Type), 
            by = c("idSensore" = "Station", "Year", "Month"))

# write.csv(Y180, "./Datasets_2000_23/180_zone_area_type.csv")
# write.csv(Y120, "./Datasets_2000_23/120_zone_area_type.csv")


# Distance matrix  ------------------------------------------------------------------------------------

stations <- read_csv('./Dati_iniziali/51stazioni_O3.csv')

stations <- stations %>%
  filter(IdSensore %in% stations_apr_1_2000)

dist_mat <- matrix(0, ncol = dim(stations)[1], nrow = dim(stations)[1])
for (i in 1:dim(stations)[1])
{
  for (j in i:dim(stations)[1])
  {
    dist_mat[i,j] <- dist_mat[j,i] <- distGeo(c(stations$lat[i], stations$lng[i]), c(stations$lat[j], stations$lng[j]))/1000
  }
}

# write.csv(dist_mat, "./Datasets_2000_23/distances.csv")


## Computing max distance for space model ------------------------

sensori <- data.frame(stations$lng, stations$lat)
distanze <- distm(sensori, fun = distHaversine)
distanza_massima <- max(distanze[upper.tri(distanze)])
km <- distanza_massima/1000

rho <- km/3 # 57

rm(list = ls())


# Bivariate dataset -----------------------------------------------

ozono_180 <- read_csv('./Datasets_2000_23/Dataset_180.csv')
ozono_120 <- read_csv('./Datasets_2000_23/Dataset_120.csv')

ozono_tot <- ozono_180 %>%
  left_join(ozono_120 %>% select(idSensore, Year, Month, Count_120), 
            by = c("idSensore", "Year", "Month"))

# write.csv(ozono_tot, "./Datasets_2000_23/Dataset_bivariate.csv")
