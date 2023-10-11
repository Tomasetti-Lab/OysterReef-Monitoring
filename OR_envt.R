#####################################################################################################################
#####################################################################################################################
### R Code for Tomasetti-Lab written by Stephen Tomasetti
### For Project: Oyster Reef Control of Carbonate Chemistry 
### Article: https://doi.org/10.1111/gcb.16960
#####################################################################################################################

# Environmental Data Analysis for high-frequency oyster monitoring data

# Load libraries
library(tidyverse)
library(dplyr)
library(lubridate)

# Import high-frequency data via "Import Dataset"

# Create tibble
Envt <- as_tibble(environmental_hfb)

# Create new tibbles based on site
QC <- filter(Envt, site == "Quogue control")
QR <- filter(Envt, site == "Quogue reef")
SC <- filter (Envt, site == "Sedge control")
SR1 <- filter(Envt, site == "Sedge reef 1")
SR2 <- filter(Envt, site == "Sedge reef 2")

# CALCULATE DAILY MIN, MAX, MEANS, RANGES for HIGH-FREQUENCY OBSERVATIONS
# If any data point is missing, the daily summarized value will be NAN

# Quogue Control
QC_daily <- QC %>%
  group_by (cday) %>%
  summarise_at(vars(temp:phn), list(min = min, max = max, mean = mean), na.rm = FALSE)%>%
  mutate(temp_range = temp_max - temp_min, sal_range = sal_max - sal_min, vert_range = vert_max - vert_min, 
         tide_range = tide_max - tide_min, dosat_range = dosat_max - dosat_min, domgl_range = domgl_max - domgl_min, 
         phn_range = phn_max - phn_min)%>%
  relocate(cday, domgl_mean, domgl_min, domgl_max, domgl_range, dosat_mean, dosat_min, dosat_max, dosat_range,
           phn_mean, phn_min, phn_max, phn_range, temp_mean, temp_min, temp_max, temp_range, 
           sal_mean, sal_min, sal_max, sal_range, tide_mean, tide_min, tide_max, tide_range,
           vert_mean, vert_min, vert_max, vert_range,)
# Quogue Reef
QR_daily <- QR %>%
  group_by (cday) %>%
  summarise_at(vars(temp:phn), list(min = min, max = max, mean = mean), na.rm = FALSE)%>%
  mutate(temp_range = temp_max - temp_min, sal_range = sal_max - sal_min, vert_range = vert_max - vert_min, 
         tide_range = tide_max - tide_min, dosat_range = dosat_max - dosat_min, domgl_range = domgl_max - domgl_min, 
         phn_range = phn_max - phn_min)%>%
  relocate(cday, domgl_mean, domgl_min, domgl_max, domgl_range, dosat_mean, dosat_min, dosat_max, dosat_range,
           phn_mean, phn_min, phn_max, phn_range, temp_mean, temp_min, temp_max, temp_range, 
           sal_mean, sal_min, sal_max, sal_range, tide_mean, tide_min, tide_max, tide_range,
           vert_mean, vert_min, vert_max, vert_range,)
# Sedge Control
SC_daily <- SC %>%
  group_by (cday) %>%
  summarise_at(vars(temp:phn), list(min = min, max = max, mean = mean), na.rm = FALSE)%>%
  mutate(temp_range = temp_max - temp_min, sal_range = sal_max - sal_min, vert_range = vert_max - vert_min, 
         tide_range = tide_max - tide_min, dosat_range = dosat_max - dosat_min, domgl_range = domgl_max - domgl_min, 
         phn_range = phn_max - phn_min)%>%
  relocate(cday, domgl_mean, domgl_min, domgl_max, domgl_range, dosat_mean, dosat_min, dosat_max, dosat_range,
           phn_mean, phn_min, phn_max, phn_range, temp_mean, temp_min, temp_max, temp_range, 
           sal_mean, sal_min, sal_max, sal_range, tide_mean, tide_min, tide_max, tide_range,
           vert_mean, vert_min, vert_max, vert_range,)
# Sedge Reef 1
SR1_daily <- SR1 %>%
  group_by (cday) %>%
  summarise_at(vars(temp:phn), list(min = min, max = max, mean = mean), na.rm = FALSE)%>%
  mutate(temp_range = temp_max - temp_min, sal_range = sal_max - sal_min, vert_range = vert_max - vert_min, 
         tide_range = tide_max - tide_min, dosat_range = dosat_max - dosat_min, domgl_range = domgl_max - domgl_min, 
         phn_range = phn_max - phn_min)%>%
  relocate(cday, domgl_mean, domgl_min, domgl_max, domgl_range, dosat_mean, dosat_min, dosat_max, dosat_range,
           phn_mean, phn_min, phn_max, phn_range, temp_mean, temp_min, temp_max, temp_range, 
           sal_mean, sal_min, sal_max, sal_range, tide_mean, tide_min, tide_max, tide_range,
           vert_mean, vert_min, vert_max, vert_range,)
# Sedge Reef 2
SR2_daily <- SR2 %>%
  group_by (cday) %>%
  summarise_at(vars(temp:phn), list(min = min, max = max, mean = mean), na.rm = FALSE)%>%
  mutate(temp_range = temp_max - temp_min, sal_range = sal_max - sal_min, vert_range = vert_max - vert_min, 
         tide_range = tide_max - tide_min, dosat_range = dosat_max - dosat_min, domgl_range = domgl_max - domgl_min, 
         phn_range = phn_max - phn_min)%>%
  relocate(cday, domgl_mean, domgl_min, domgl_max, domgl_range, dosat_mean, dosat_min, dosat_max, dosat_range,
           phn_mean, phn_min, phn_max, phn_range, temp_mean, temp_min, temp_max, temp_range, 
           sal_mean, sal_min, sal_max, sal_range, tide_mean, tide_min, tide_max, tide_range,
           vert_mean, vert_min, vert_max, vert_range,)

# Save each tibble as a csv file
write_csv(SC_daily, "sc_daily.csv")
write_csv(SR1_daily, "sr1_daily.csv")
write_csv(SR2_daily, "sr2_daily.csv")
write_csv(QC_daily, "qc_daily.csv")
write_csv(QR_daily, "qr_daily.csv")