# Necessary libraries -----------------------------------------------------
library(tidyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tibble)
library(readr)
library(readxl)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(fda)
library(gridExtra)
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

staz <- read.csv("./Datasets_2000_23/Sensors.csv")


# 180  ------------------------------------------------------------------

df.180 <- read.csv("./Datasets_all_sensors/Dataset_180.csv")


## Area --------------------------------------------------------------------

data_aggregated <- df.180 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# urban

urban_data <- data_aggregated %>%
  filter(Area == "U")
num_urban_stations <- length(unique(urban_data$idSensore))
urban_colors <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_urban_stations)
urban_data <- urban_data %>%
  mutate(color = urban_colors[as.numeric(factor(idSensore))])

p1 <- ggplot(urban_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Urban",
       subtitle = "27",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# suburban

suburban_data <- data_aggregated %>%
  filter(Area == "S")
num_suburban_stations <- length(unique(suburban_data$idSensore))
suburban_colors <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_suburban_stations)
suburban_data <- suburban_data %>%
  mutate(color = suburban_colors[as.numeric(factor(idSensore))])

p2 <- ggplot(suburban_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Suburban",
       subtitle = "12",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# rural

rural_data <- data_aggregated %>%
  filter(Area == "R")
num_rural_stations <- length(unique(rural_data$idSensore))
rural_colors <- colorRampPalette(c("#006400", "#228B22", "#32CD32", "#7FFF00", "#ADFF2F"))(num_rural_stations)
rural_data <- rural_data %>%
  mutate(color = rural_colors[as.numeric(factor(idSensore))])

p3 <- ggplot(rural_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Rural",
       subtitle = "12",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p2, p3, ncol = 3)
# 15 x 3.5

## Type --------------------------------------------------------------------

data_aggregated <- df.180 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# traffic & industrial

traffic_data <- data_aggregated %>%
  filter(Type %in% c("T", "I"))
num_traffic_stations <- length(unique(traffic_data$idSensore))
traffic_colors <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_traffic_stations)
traffic_data <- traffic_data %>%
  mutate(color = traffic_colors[as.numeric(factor(idSensore))])

p1 <- ggplot(traffic_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Non-background",
       subtitle = "6",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# industrial 

industrial_data <- data_aggregated %>%
  filter(Type == "I")
num_industrial_stations <- length(unique(industrial_data$idSensore))
industrial_colors <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_industrial_stations)
industrial_data <- industrial_data %>%
  mutate(color = industrial_colors[as.numeric(factor(idSensore))])

p2 <- ggplot(industrial_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Industrial",
       subtitle = "2",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  # Usa direttamente i colori dalla colonna 'color'
  scale_color_identity() +  # Usa direttamente i colori dalla colonna 'color'
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Ruota e regola la dimensione delle etichette dell'asse x
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# background

background_data <- data_aggregated %>%
  filter(Type == "B")
num_background_stations <- length(unique(background_data$idSensore))
background_colors <- colorRampPalette(c("#006400", "#228B22", "#32CD32", "#7FFF00", "#ADFF2F"))(num_background_stations)
background_data <- background_data %>%
  mutate(color = background_colors[as.numeric(factor(idSensore))])

p3 <- ggplot(background_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Background",
       subtitle = "45",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p3, ncol = 2)
# 15 x 3.5


## Zone --------------------------------------------------------------------

data_aggregated <- df.180 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# cities

cities_data <- data_aggregated %>%
  filter(Zone == c("Mi", "Bg", "Br"))
num_cities_stations <- length(unique(cities_data$idSensore))
palette_cities <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_cities_stations)  
cities_data <- cities_data %>%
  mutate(color = palette_cities[as.numeric(factor(idSensore))])

p1 <- ggplot(cities_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Cities",
       subtitle = "16",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# urbanized plain

A_data <- data_aggregated %>%
  filter(Zone == "A")
num_A_stations <- length(unique(A_data$idSensore))
palette_A <- colorRampPalette(c("#8B4513", "#A0522D", "#D2B48C", "#DEB887", "#F5DEB3"))(num_A_stations)
A_data <- A_data %>%
  mutate(color = palette_A[as.numeric(factor(idSensore))])

p2 <- ggplot(A_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Urbanized Plain",
       subtitle = "15",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# plain

B_data <- data_aggregated %>%
  filter(Zone == "B")
num_B_stations <- length(unique(B_data$idSensore))
palette_B <- colorRampPalette(c("#32CD32", "#228B22", "#7FFF00", "#ADFF2F", "#00FF7F"))(num_B_stations)
B_data <- B_data %>%
  mutate(color = palette_B[as.numeric(factor(idSensore))])

p3 <- ggplot(B_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Plain",
       subtitle = "13",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# foothill

C1_data <- data_aggregated %>%
  filter(Zone == "C1")
num_C1_stations <- length(unique(C1_data$idSensore))
palette_C1 <- colorRampPalette(c("#4682B4", "#5F9EA0", "#B0C4DE", "#87CEEB", "#ADD8E6"))(num_C1_stations)
C1_data <- C1_data %>%
  mutate(color = palette_C1[as.numeric(factor(idSensore))])

p4 <- ggplot(C1_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Foothills",
       subtitle = "3",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# mountain

C2_data <- data_aggregated %>%
  filter(Zone == "C2")
num_C2_stations <- length(unique(C2_data$idSensore))
palette_C2 <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_C2_stations)
C2_data <- C2_data %>%
  mutate(color = palette_C2[as.numeric(factor(idSensore))])

p5 <- ggplot(C2_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Mountains",
       subtitle = "1",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# valley

D_data <- data_aggregated %>%
  filter(Zone == "D")
num_D_stations <- length(unique(D_data$idSensore))
palette_D <- colorRampPalette(c("#2F4F4F", "#556B2F", "#8FBC8F", "#6B8E23", "#808000"))(num_D_stations)
D_data <- D_data %>%
  mutate(color = palette_D[as.numeric(factor(idSensore))])

p6 <- ggplot(D_data, aes(x = Date.mon, y = Count_180, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Valley",
       subtitle = "3",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
# 15 x 7



# 120 --------------------------------------------------------------------

df.120 <- read.csv("./Datasets_all_sensors/Dataset_120.csv")


## Area --------------------------------------------------------------------

data_aggregated <- df.120 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# urban

urban_data <- data_aggregated %>%
  filter(Area == "U")
num_urban_stations <- length(unique(urban_data$idSensore))
urban_colors <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_urban_stations)
urban_data <- urban_data %>%
  mutate(color = urban_colors[as.numeric(factor(idSensore))])

p1 <- ggplot(urban_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Urban",
       subtitle = "27",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# suburban

suburban_data <- data_aggregated %>%
  filter(Area == "S")
num_suburban_stations <- length(unique(suburban_data$idSensore))
suburban_colors <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_suburban_stations)
suburban_data <- suburban_data %>%
  mutate(color = suburban_colors[as.numeric(factor(idSensore))])

p2 <- ggplot(suburban_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Suburban",
       subtitle = "12",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# rural

rural_data <- data_aggregated %>%
  filter(Area == "R")
num_rural_stations <- length(unique(rural_data$idSensore))
rural_colors <- colorRampPalette(c("#006400", "#228B22", "#32CD32", "#7FFF00", "#ADFF2F"))(num_rural_stations)
rural_data <- rural_data %>%
  mutate(color = rural_colors[as.numeric(factor(idSensore))])

p3 <- ggplot(rural_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Rural",
       subtitle = "12",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p2, p3, ncol = 3)
# 15 x 3.5


## Type --------------------------------------------------------------------

data_aggregated <- df.120 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# traffic & industrial

traffic_data <- data_aggregated %>%
  filter(Type %in% c("T", "I"))
num_traffic_stations <- length(unique(traffic_data$idSensore))
traffic_colors <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_traffic_stations)
traffic_data <- traffic_data %>%
  mutate(color = traffic_colors[as.numeric(factor(idSensore))])

p1 <- ggplot(traffic_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Non-background",
       subtitle = "6",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

# industrial

industrial_data <- data_aggregated %>%
  filter(Type == "I")
num_industrial_stations <- length(unique(industrial_data$idSensore))
industrial_colors <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_industrial_stations)
industrial_data <- industrial_data %>%
  mutate(color = industrial_colors[as.numeric(factor(idSensore))])

p2 <- ggplot(industrial_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Industrial",
       subtitle = "2",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  # Usa direttamente i colori dalla colonna 'color'
  scale_color_identity() +  # Usa direttamente i colori dalla colonna 'color'
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Ruota e regola la dimensione delle etichette dell'asse x
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# background

background_data <- data_aggregated %>%
  filter(Type == "B")
num_background_stations <- length(unique(background_data$idSensore))
background_colors <- colorRampPalette(c("#006400", "#228B22", "#32CD32", "#7FFF00", "#ADFF2F"))(num_background_stations)
background_data <- background_data %>%
  mutate(color = background_colors[as.numeric(factor(idSensore))])

p3 <- ggplot(background_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Background",
       subtitle = "45",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p3, ncol = 2)
# 15 x 3.5


## Zone --------------------------------------------------------------------

data_aggregated <- df.120 %>%
  left_join(staz[, c('Area', 'Type', 'Zone', 'IdSensore')], by = c("idSensore" = "IdSensore")) %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# cities

cities_data <- data_aggregated %>%
  filter(Zone == c("Mi", "Bg", "Br"))
num_cities_stations <- length(unique(cities_data$idSensore))
palette_cities <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(num_cities_stations)  
cities_data <- cities_data %>%
  mutate(color = palette_cities[as.numeric(factor(idSensore))])

p1 <- ggplot(cities_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Cities",
       subtitle = "16",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# urbanized plain

A_data <- data_aggregated %>%
  filter(Zone == "A")
num_A_stations <- length(unique(A_data$idSensore))
palette_A <- colorRampPalette(c("#8B4513", "#A0522D", "#D2B48C", "#DEB887", "#F5DEB3"))(num_A_stations)
A_data <- A_data %>%
  mutate(color = palette_A[as.numeric(factor(idSensore))])

p2 <- ggplot(A_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Urbanized Plain",
       subtitle = "15",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# plain

B_data <- data_aggregated %>%
  filter(Zone == "B")
num_B_stations <- length(unique(B_data$idSensore))
palette_B <- colorRampPalette(c("#32CD32", "#228B22", "#7FFF00", "#ADFF2F", "#00FF7F"))(num_B_stations)
B_data <- B_data %>%
  mutate(color = palette_B[as.numeric(factor(idSensore))])

p3 <- ggplot(B_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Plain",
       subtitle = "13",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# foothill

C1_data <- data_aggregated %>%
  filter(Zone == "C1")
num_C1_stations <- length(unique(C1_data$idSensore))
palette_C1 <- colorRampPalette(c("#4682B4", "#5F9EA0", "#B0C4DE", "#87CEEB", "#ADD8E6"))(num_C1_stations)
C1_data <- C1_data %>%
  mutate(color = palette_C1[as.numeric(factor(idSensore))])

p4 <- ggplot(C1_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Foothills",
       subtitle = "3",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# mountain

C2_data <- data_aggregated %>%
  filter(Zone == "C2")
num_C2_stations <- length(unique(C2_data$idSensore))
palette_C2 <- colorRampPalette(c("#1E90FF", "#6A5ACD", "#483D8B", "#8A2BE2", "#9370DB"))(num_C2_stations)
C2_data <- C2_data %>%
  mutate(color = palette_C2[as.numeric(factor(idSensore))])

p5 <- ggplot(C2_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Mountains",
       subtitle = "1",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# valley

D_data <- data_aggregated %>%
  filter(Zone == "D")
num_D_stations <- length(unique(D_data$idSensore))
palette_D <- colorRampPalette(c("#2F4F4F", "#556B2F", "#8FBC8F", "#6B8E23", "#808000"))(num_D_stations)
D_data <- D_data %>%
  mutate(color = palette_D[as.numeric(factor(idSensore))])

p6 <- ggplot(D_data, aes(x = Date.mon, y = Count_120, fill = color, color = color)) +
  geom_line(aes(group = idSensore, color = color), lwd = 0.5, lty = 1) + 
  geom_point(size = 1.5, pch = 21, col = 1) + 
  labs(title = "Valley",
       subtitle = "3",
       x = "Years",
       y = "Occurrences") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_fill_identity() +  
  scale_color_identity() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
# 15 x 7


# Meteorological factors ---------------------------------------------

meteo <- read.csv("./Datasets_2000_23/Covariates.csv")

meteo$Density <- NULL
meteo$Type <- NULL
meteo$Area <- NULL
meteo$Zone <- NULL
meteo$Quota <- NULL

# merge datasets
data <- merge(df.120, df.180, 
              by = c("Year", "Month", "idSensore"),
              all = TRUE)  

merged_data <- merge(meteo, data, 
                     by.x = c("Year", "Month", "Station"), 
                     by.y = c("Year", "Month", "idSensore"))

merged_data <- merged_data %>%
  rename(
    Temperature = mean_temperature,
    'Precipitation Tot' = mean_precipitation_sum,
    'Precipitation Hours' = mean_precipitation_hours,
    'Wind Speed' = mean_windspeed_10m_max,
    'Solar Radiation' = mean_radiation_sum,
    'High Wind Days' = count_highwind,
    'Consecutive High Wind Days' = max_consecutive_highwind_days,
    'Count 180' = Count_180,
    'Count 120' = Count_120
  )  


## Correlation plot --------------------------------------------------

selected_data <- merged_data[, c("Temperature", "Precipitation Tot",
                                 "Precipitation Hours", "Wind Speed",
                                 "Solar Radiation", "Count 180", "Count 120")]
correlation_matrix <- cor(selected_data, use = "complete.obs")

ggcorrplot(correlation_matrix, 
           method = "circle",  
           type = "full",     
           lab = TRUE,         
           lab_size = 2,  
           show.diag = FALSE,
           outline.color = "#293133",
           ggtheme = theme() + theme(axis.text = element_text(color = "black")),  
           tl.cex = 8, 
           tl.srt = 45, 
           tl.col = "black")  


## Time series 2000-2023 ------------------------------------------------------

data_aggregated <- merged_data %>%
  mutate(Date.mon = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


# 180

temperature_palette <- colorRampPalette(c("#FF4500", "#FF6347", "#FFA07A", "#FFD700", "#FF8C00"))(length(unique(data_aggregated$Station)))
count_180_palette <- colorRampPalette(c("#A9A9A9", "#696969", "#808080", "#D3D3D3", "#C0C0C0"))(length(unique(data_aggregated$Station)))
precipitations_palette <- colorRampPalette(c("#4682B4", "#5F9EA0", "#B0C4DE", "#87CEEB", "#ADD8E6"))(length(unique(data_aggregated$Station)))
radiation_palette <- colorRampPalette(c("#8A2BE2", "#9370DB", "#BA55D3", "#DA70D6", "#EE82EE"))(length(unique(data_aggregated$Station)))

p1 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = Temperature, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(temperature_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p2 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = Radiation, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.3) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.3) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(radiation_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p3 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = `Precipitation Hours`, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.3) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.3) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(precipitations_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )


# 120

p4 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = Temperature, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.3) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.3) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(temperature_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p5 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = Radiation, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.3) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.3) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(radiation_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p6 <- ggplot(data_aggregated) +
  geom_line(aes(x = Date.mon, y = `Precipitation Tot`, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.3) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.3) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_color_manual(values = c(precipitations_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

grid.arrange(p1, p4, ncol = 2)
grid.arrange(p2, p5, ncol = 2)
grid.arrange(p3, p6, ncol = 2)
# 15 x 3.5


## Time series 2020-2023 ----------------------------------------

df_filtered <- data_aggregated %>%
  filter(Year >= 2020 & Year <= 2023)

p1 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = Temperature, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(temperature_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p2 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = Radiation, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(radiation_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p3 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = `Precipitation Hours`, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 180`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 180`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Single-hour Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(precipitations_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )


# 120

p4 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = Temperature, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(temperature_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p5 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = Radiation, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(radiation_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p6 <- ggplot(df_filtered) +
  geom_line(aes(x = Date.mon, y = `Precipitation Tot`, group = Station, color = as.factor(Station)), size = 1) +
  geom_line(aes(x = Date.mon, y = `Count 120`, group = Station, color = paste0("Count 180", Station)), size = 0.5, lty = "dotted", alpha = 0.6) +
  geom_point(aes(x = Date.mon, y = `Count 120`, fill = paste0("Count 180", Station)), size = 1.5, pch = 21, color = "black", alpha = 0.6) +
  labs(x = "Years",
       y = "",
       title = "Sliding-window Threshold",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 31, by = 10), limits = c(0, 31)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = c(precipitations_palette, count_180_palette)) +
  scale_fill_manual(values = count_180_palette) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

grid.arrange(p1, p4, ncol = 2)
grid.arrange(p2, p5, ncol = 2)
grid.arrange(p3, p6, ncol = 2)
# 15 x 3.5

