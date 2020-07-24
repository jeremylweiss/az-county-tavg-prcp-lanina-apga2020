

# This code calculates November-December average temperature and total
# precipitation for Arizona-county values, highlighting La Nina years, for 
# APGA 2020 conference presentation

# Precipitation and temperature data are from: 
# https://www.ncdc.noaa.gov/cag/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("reshape2")
library("ggplot2")
library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Set data import codes for filtering and analysis
state_code_az <- 2

# Set county fips information
County_name <- c("Apache", "Cochise", "Coconino", "Gila", "Graham", "Greenlee", 
                 "La Paz", "Maricopa", "Mohave", "Navajo", "Pima", "Pinal",
                 "Santa Cruz", "Yavapai", "Yuma")
County_fips <- c(1, 3, 5, 7, 9, 11, 12, 13, 15, 17, 19, 21, 23, 25, 27)
county_info <- as.data.frame(cbind(County_name, County_fips))

# Set time span
yr_start <- 1949
yr_end <- 2019

# Set La Nina years, based on list at:
# https://www.climate.gov/news-features/featured-images/temperature-patterns-during-every-la-ni%C3%B1a-winter-1950
yr_la_nina <- c(
  # strong
  1973, 1988, 1999, 1975, 2007, 1949, 1998,
  # moderate
  1970, 2010, 1955, 1984,
  #weak
  1995, 2005, 2008, 2011, 1954, 1971, 2000, 1964, 1983, 1974
  )

# Set El Nino years, based on list at:
# https://www.climate.gov/news-features/featured-images/us-winter-precipitation-during-every-el-ni%C3%B1o-1950
#yr_el_nino <- c(
  # strong
#  2015, 1982, 1997, 1957, 1972, 1991,
  # moderate
#  2009, 1965, 1986, 1963, 1968, 1994,
  #weak
#  2002, 1953, 1987, 1976, 1977, 2006, 1958, 1979, 2004, 2014, 1951, 1969
#)


# LOAD AND TRANSFORM MONTHLY DATA --------------------


# Precipitation ----------


prcp <- read.table(
  file = "climdiv-pcpncy-v1.0.0-20200706.txt", 
  header = FALSE, skip = 0, col.names = c(
    "codes", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ))

prcp <- select(prcp, codes, Nov, Dec)

prcp$codes <- as.character(prcp$codes)
for (i in 1:length(prcp$codes)) {
  if (nchar(prcp$codes[i]) == 10) {
    prcp$codes[i] <- paste0("0", prcp$codes[i])
  } #else {
    #prcp$codes[i] <- prcp$codes[i]
  #}
}
rm(i)

prcp["State_code"] <- as.numeric(substr(prcp$codes, start = 1, stop = 2))
prcp["County_fips"] <- as.numeric(substr(prcp$codes, start = 3, stop = 5))
#prcp["element_code"] <- as.numeric(substr(prcp$codes, start = 6, stop = 7))
prcp["Year"] <- as.numeric(substr(prcp$codes, start = 8, stop = 11))

prcp <- filter(prcp, State_code == state_code_az)
prcp <- filter(prcp, Year >= yr_start)
prcp <- filter(prcp, Year <= yr_end)

prcp["NovDec_total"] <- rowSums(cbind(prcp$Nov, prcp$Dec))

# Mark La Nina and non-La Nina years in record
prcp["ENSO"] <- NA
for (yr in 1:length(prcp$Year)) {
  if (prcp$Year[yr] %in% yr_la_nina == TRUE) {
    prcp$ENSO[yr] <- "La Niña years"
  } else {
    prcp$ENSO[yr] <- "non-La Niña years"
  }
}
rm(yr)


# Temperature ----------


tavg <- read.table(
  file = "climdiv-tmpccy-v1.0.0-20200706.txt", 
  header = FALSE, skip = 0, col.names = c(
    "codes", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ))

tavg <- select(tavg, codes, Nov, Dec)

tavg$codes <- as.character(tavg$codes)
for (i in 1:length(tavg$codes)) {
  if (nchar(tavg$codes[i]) == 10) {
    tavg$codes[i] <- paste0("0", tavg$codes[i])
  } #else {
  #tavg$codes[i] <- tavg$codes[i]
  #}
}
rm(i)

tavg["State_code"] <- as.numeric(substr(tavg$codes, start = 1, stop = 2))
tavg["County_fips"] <- as.numeric(substr(tavg$codes, start = 3, stop = 5))
#tavg["element_code"] <- as.numeric(substr(tavg$codes, start = 6, stop = 7))
tavg["Year"] <- as.numeric(substr(tavg$codes, start = 8, stop = 11))

tavg <- filter(tavg, State_code == state_code_az)
tavg <- filter(tavg, Year >= yr_start)
tavg <- filter(tavg, Year <= yr_end)

tavg["NovDec_avg"] <- rowMeans(cbind(tavg$Nov, tavg$Dec))

# Mark La Nina and non-La Nina years in record
tavg["ENSO"] <- NA
for (yr in 1:length(tavg$Year)) {
  if (tavg$Year[yr] %in% yr_la_nina == TRUE) {
    tavg$ENSO[yr] <- "La Niña years"
  } else {
    tavg$ENSO[yr] <- "non-La Niña years"
  }
}
rm(yr)


# CALCULATE AVERAGE LA NINA ANOMALY BY COUNTY --------------------


# Calculate 1981-2010 climatology by county
county_info["prcp_clim"] <- NA
for (county in 1:nrow(county_info)) {
  x <- filter(prcp, County_fips == county_info$County_fips[county])
  x <- filter(x, Year >= 1981 & Year <= 2010)
  county_info$prcp_clim[
    which(county_info$County_fips == county_info$County_fips[county])
    ] <- mean(x$NovDec_total)
  rm(x)
}
rm(county)

county_info["tavg_clim"] <- NA
for (county in 1:nrow(county_info)) {
  x <- filter(tavg, County_fips == county_info$County_fips[county])
  x <- filter(x, Year >= 1981 & Year <= 2010)
  county_info$tavg_clim[
    which(county_info$County_fips == county_info$County_fips[county])
    ] <- mean(x$NovDec_avg)
  rm(x)
}
rm(county)

# Calculate La Nina average
county_info["prcp_la_nina"] <- NA
for (county in 1:nrow(county_info)) {
  x <- filter(prcp, County_fips == county_info$County_fips[county])
  x <- filter(x, Year %in% yr_la_nina)
  county_info$prcp_la_nina[
    which(county_info$County_fips == county_info$County_fips[county])
    ] <- mean(x$NovDec_total)
  rm(x)
}
rm(county)

county_info["tavg_la_nina"] <- NA
for (county in 1:nrow(county_info)) {
  x <- filter(tavg, County_fips == county_info$County_fips[county])
  x <- filter(x, Year %in% yr_la_nina)
  county_info$tavg_la_nina[
    which(county_info$County_fips == county_info$County_fips[county])
    ] <- mean(x$NovDec_avg)
  rm(x)
}
rm(county)


# Calculate La Nina anomaly
county_info["prcp_ln_anom"] <- county_info$prcp_la_nina - county_info$prcp_clim
county_info["tavg_ln_anom"] <- county_info$tavg_la_nina - county_info$tavg_clim

# Calculate La Nina anomaly percent change
county_info["prcp_ln_perchg"] <- 
  ((county_info$prcp_ln_anom) / county_info$prcp_clim) * 100

