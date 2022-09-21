# ---------------------------------------------------------- #
# -------------------- Organize grid data ----------------- #
# ---------------------------------------------------------- #
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(magrittr)

# Steps:
# (1) Download the .nc files with the minimum and maximum temperatures for each year and all the coodinates (shared by Rowan).
# (2) Apply the readobs function and read the files from step (1).
# (3) Calculate the mean temperature.
# (4) Save dates and temperatures (min, max and mean) to a .rda file.


# Save the grid data for each year ----------------------------------------

# convert the nc file into a tibble, saving it as a rda file, and get the mean from min and max temp.
readeobs <- function(path){
  eobs <- raster::brick(path)  # read in netcdf file
  my_data <- raster::coordinates(eobs) %>% as_tibble()

  # save each day as a new column
  eobs_data <- eobs %>%
    values() %>%
    as_tibble()

  # adding lat and long information
  eobs_data$x <- my_data$x
  eobs_data$y <- my_data$y

  # removing mysterious "X" from all dates
  names(eobs_data) <- names(eobs_data) %>% str_remove_all("X")
  eobs_data %<>% drop_na() # remove nans

  eobs_tibble <- eobs_data %>%
    tidyr::pivot_longer(c(-x, -y), names_to = "date", values_to = "temp")

  return(eobs_tibble)
}

# Example (year 2014).
mineobs <- readeobs("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/min001_2014.nc")
maxeobs <- readeobs("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/max001_2014")

# get the min and max temperatures of calculate the mean temp.
mean001 <- cbind(mineobs, maxeobs$temp) %>%
  dplyr::rename("min_temp" = "temp",
                "max_temp" = "maxeobs$temp")
mean001$temp <- rowMeans(mean001[,c("min_temp", "max_temp")])

# save the data (change it to the right year)
mean001_2014 <- mean001
save(mean001_2014, file = "Data/Climate Data/mean001_2014.Rda")








