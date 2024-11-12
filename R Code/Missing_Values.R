# Necessary libraries -----------------------------------------------------
library(ggplot2)
library(tibble)
library(readr)
library(readxl)
library(fda)
library(reshape2)
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

# Plot NAs  -----------------------------------------------------
ozono = read_csv('./Dati_iniziali/datasetO3.csv')

# shift hours
ozono$Data <- dmy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

sapply(ozono, function(y) sum(length(which(is.na(y))))) # 269994 missing values

ozono$timestamp <- make_datetime(ozono$Year, ozono$Month, ozono$Day, ozono$Hour)
all_timestamps <- expand.grid(
  idSensore = unique(ozono$idSensore),
  timestamp = seq(min(ozono$timestamp), max(ozono$timestamp), by = "1 hour"))
ozono_completed <- left_join(all_timestamps, ozono, by = c("idSensore", "timestamp"))
rm(all_timestamps)

ozono_completed <- ozono_completed %>%
  mutate(Year = year(timestamp),
         Month = month(timestamp),
         Day = day(timestamp),
         Hour = hour(timestamp))
ozono_completed$Data <- NULL

# write.csv(ozono_completed, "./Dati_iniziali/O3_all_months.csv", row.names = FALSE)

ozono <- ozono_completed %>%
  filter(Month >= 4 & Month <= 10)
rm(ozono_completed)

# write.csv(ozono, "./Dati_iniziali/O3_completed.csv", row.names = FALSE)

ozono <- ozono %>%
  mutate(
    station_order = case_when(
      idSensore %in% stations_2000_2007 ~ 1,
      idSensore %in% stations_2007_2012 ~ 2,
      idSensore %in% stations_2012_2023 ~ 3
    )
  ) %>%
  arrange(station_order, idSensore, Year, Month)

ozono$station_order <- NULL

sensors <- unique(ozono$idSensore)
years <- unique(ozono$Year)
months <- 4:10 # from april to october

Na_ore <- matrix(0, nrow = length(sensors), ncol = (length(months) * length(years)))

calc_na_hourly <- function(data) {
  sum(is.na(data$Valore))
}

for (i in seq_along(sensors)) {
  sensor_data <- ozono %>% filter(idSensore == sensors[i])
  
  for (j in seq_along(years)) {
    year_data <- sensor_data %>% filter(Year == years[j])
    
    for (k in seq_along(months)) {
      month_data <- year_data %>% filter(Month == months[k])
      
      Na_ore[i, (j - 1) * length(months) + k] <- calc_na_hourly(month_data)
    }
  }
}

col_palette <- colorRampPalette(c("lavender", "cornflowerblue", "navy"))
image(t(Na_ore), axes = FALSE, col=col_palette(1000000))
axis(2, at = seq(0,1, length=51), labels = sensors, cex.axis = 0.6, las = 2)
x_positions <- seq(0, 1, length = length(years)) - (1 / length(years) / 2)
axis(1, at = x_positions, labels = years, cex.axis = 0.6, las = 0)
box()
# abline(v = 7.3 / 24, col = "black", lty = 2)  
# abline(v = 12.53 / 24, col = "black", lty = 2) 
abline(h = 0.37, col = "black", lty = 1, lwd = 1.5) 
abline(h = 0.77, col = "black", lty = 1, lwd = 1.5) 
mtext("Sensors", side = 2, line = 3, cex = 0.8)  # Y-axis label
mtext("Years", side = 1, line = 3, cex = 0.8)    # X-axis label

