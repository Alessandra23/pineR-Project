library(dplyr)

remove(list = ls())

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Summary data ------------------------------------------------------------

load("data/summaryData.Rda")

# summaryData_27_sites <- summaryData |>
#   filter(!(Site == "Corracloon")) |>
#   filter(!(Site == "Corrakyle")) |>
#   distinct(Site, .keep_all = TRUE)
#
# summaryData_27_sites$Site[summaryData_27_sites$Site == "Lackenrea 1" ] <- "Lackenrea1"
# summaryData_27_sites$Site[summaryData_27_sites$Site == "Lackenrea 2" ] <- "Lackenrea2"
# summaryData_27_sites$Site[summaryData_27_sites$Site == "Kilurney" ] <- "Killurney"

# save(summaryData_27_sites, file="data/summaryData_27_sites.Rda")

load('data/summaryData_27_sites.Rda')

# Emergence data ----------------------------------------------------------

emergence <- readRDS("data/emergence.RDS")
names(emergence)[names(emergence) == "Glendine"] <- "Glendine1"
names(emergence)[names(emergence) == "Kilurney"] <- "Killurney"
names(emergence)[names(emergence) == "Ballymacshan"] <- "Ballymacshaneboy"
names(emergence)

emergence <- emergence[!(names(emergence) %in% c("Deerpark2","Hortland2", "Oakwood2",
                             "Ballinagee2", "Corracloon", "Corrakyle"))]

names(emergence)

# Weighted data -----------------------------------------------------------

listWeightedData <- list.files("data/weighted", pattern = "*.Rda", full.names = TRUE)
readWeightedData <- lapply(listWeightedData, loadRData)
names(readWeightedData) <- gsub("Data.Rda", "", basename(listWeightedData))
readWeightedData |> names()

# Climate data ------------------------------------------------------------


