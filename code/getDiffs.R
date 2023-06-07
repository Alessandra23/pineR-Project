library(caret)
library(pineR)
remove(list = ls())


# # function to calculate the difference between the quantiles
#
# diffOP <- function(observed, predicted, quantile = 0.5){
#   #nRow <- length(predicted$stumpT)
#   #dayPredicted <- quantile(predicted$gemtime_new_t0-nRow, quantile)
#   dayPredicted <- predicted[which.min(abs(predicted$qm - quantile)), 1]
#   dayObserved <- observed[which.min(abs(observed$prob - quantile)), 1]
#   daysDiff <-   dayObserved - dayPredicted
#   daysDiff <- round(daysDiff)
#   return(daysDiff)
# }
#


# Read emergence data ---------------------------------------------------------------

emergence <- readRDS("data/emergence.RDS")
names(emergence)[names(emergence) == "Glendine"] <- "Glendine1"
names(emergence)[names(emergence) == "Kilurney"] <- "Killurney"
names(emergence)[names(emergence) == "Ballymacshan"] <- "Ballymacshaneboy"
names(emergence)
emergence <- emergence[!(names(emergence) %in% c("Deerpark2","Hortland2", "Oakwood2",
                                                 "Ballinagee2", "Corracloon", "Corrakyle"))]


# read the data frame with the covariates
load('data/summaryData_27_sites.Rda')
summaryData_27_sites



# Run for the different depths --------------------------------------------


# Depth = 1
load("data/run models/runs_weighted_depth_1.Rda")
runs_weighted_depth_1
names(runs_weighted_depth_1)[names(runs_weighted_depth_1) == "Longfortpass"] <- "Longfordpass"

# Quantile 0.25 ------------------------------------------------------------

diff25_depth_1 <- lapply(names(runs_weighted_depth_1), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_1[[x]], quantile = 0.25)
})
names(diff25_depth_1) <- names(runs_weighted_depth_1)
diff25_depth_1 <- plyr::ldply(diff25_depth_1) |> plyr::rename(c('.id'='Site',
                                                'V1'='diff'))


# Quantile 0.50 ------------------------------------------------------------

diff50_depth_1 <- lapply(names(runs_weighted_depth_1), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_1[[x]], quantile = 0.5)
})
names(diff50_depth_1) <- names(runs_weighted_depth_1)
diff50_depth_1 <- plyr::ldply(diff50_depth_1) |>  plyr::rename(c('.id'='Site',
                                                 'V1'='diff'))

# Quantile 0.75 ------------------------------------------------------------

diff75_depth_1 <- lapply(names(runs_weighted_depth_1), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_1[[x]], quantile = 0.75)
})
names(diff75_depth_1) <- names(runs_weighted_depth_1)
diff75_depth_1 <- plyr::ldply(diff75_depth_1) |>  plyr::rename(c('.id'='Site',
                                                 'V1'='diff'))

diffs_df_depth_1 <- data.frame(Site = diff75_depth_1$Site,
                      "D25" = diff25_depth_1$diff,
                      "D50" = diff50_depth_1$diff,
                      "D75" = diff75_depth_1$diff)

#save(diffs_df_depth_1, file="data/run models/diffs_df_depth_1.Rda")


# Join with the covariates ------------------------------------------------

load("data/run models/diffs_df_depth_1.Rda")
diffs_df_depth_1

reorderindex_depth_1 <- match(diffs_df_depth_1$Site, summaryData_27_sites$Site)
newdata_depth_1 <- summaryData_27_sites[reorderindex_depth_1,]

data25_depth_1 <- data.frame(y = diffs_df_depth_1$D25,
                             species = newdata_depth_1$Species,
                             soil = newdata_depth_1$`Soil Type`,
                             altitude = newdata_depth_1$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_1$Aspect,
                             slope = newdata_depth_1$Slope)

data50_depth_1 <- data.frame(y = diffs_df_depth_1$D50,
                             species = newdata_depth_1$Species,
                             soil = newdata_depth_1$`Soil Type`,
                             altitude = newdata_depth_1$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_1$Aspect,
                             slope = newdata_depth_1$Slope)

data75_depth_1 <- data.frame(y = diffs_df_depth_1$D75,
                             species = newdata_depth_1$Species,
                             soil = newdata_depth_1$`Soil Type`,
                             altitude = newdata_depth_1$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_1$Aspect,
                             slope = newdata_depth_1$Slope)

data25_depth_1$species <- as.factor(data25_depth_1$species)
data25_depth_1$soil <- as.factor(data25_depth_1$soil)
data25_depth_1$aspect <- as.factor(data25_depth_1$aspect)

data50_depth_1$species <- as.factor(data50_depth_1$species)
data50_depth_1$soil <- as.factor(data50_depth_1$soil)
data50_depth_1$aspect <- as.factor(data50_depth_1$aspect)

data75_depth_1$species <- as.factor(data75_depth_1$species)
data75_depth_1$soil <- as.factor(data75_depth_1$soil)
data75_depth_1$aspect <- as.factor(data75_depth_1$aspect)

# save(data25_depth_1, file="data/run models/data25_depth_1.Rda")
# save(data50_depth_1, file="data/run models/data50_depth_1.Rda")
# save(data75_depth_1, file="data/run models/data75_depth_1.Rda")




# -------------------------------------------------------------------------




# Depth = 2
load("data/run models/runs_weighted_depth_2.Rda")
runs_weighted_depth_2
names(runs_weighted_depth_2)[names(runs_weighted_depth_2) == "Longfortpass"] <- "Longfordpass"

# Quantile 0.25 ------------------------------------------------------------

diff25_depth_2 <- lapply(names(runs_weighted_depth_2), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_2[[x]], quantile = 0.25)
})
names(diff25_depth_2) <- names(runs_weighted_depth_2)
diff25_depth_2 <- plyr::ldply(diff25_depth_2) |> plyr::rename(c('.id'='Site',
                                                                'V1'='diff'))


# Quantile 0.50 ------------------------------------------------------------

diff50_depth_2 <- lapply(names(runs_weighted_depth_2), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_2[[x]], quantile = 0.5)
})
names(diff50_depth_2) <- names(runs_weighted_depth_2)
diff50_depth_2 <- plyr::ldply(diff50_depth_2) |>  plyr::rename(c('.id'='Site',
                                                                 'V1'='diff'))

# Quantile 0.75 ------------------------------------------------------------

diff75_depth_2 <- lapply(names(runs_weighted_depth_2), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_2[[x]], quantile = 0.75)
})
names(diff75_depth_2) <- names(runs_weighted_depth_2)
diff75_depth_2 <- plyr::ldply(diff75_depth_2) |>  plyr::rename(c('.id'='Site',
                                                                 'V1'='diff'))

diffs_df_depth_2 <- data.frame(Site = diff75_depth_2$Site,
                               "D25" = diff25_depth_2$diff,
                               "D50" = diff50_depth_2$diff,
                               "D75" = diff75_depth_2$diff)

#save(diffs_df_depth_2, file="data/run models/diffs_df_depth_2.Rda")


# Join with the covariates ------------------------------------------------

load("data/run models/diffs_df_depth_2.Rda")
diffs_df_depth_2

reorderindex_depth_2 <- match(diffs_df_depth_2$Site, summaryData_27_sites$Site)
newdata_depth_2 <- summaryData_27_sites[reorderindex_depth_2,]

data25_depth_2 <- data.frame(y = diffs_df_depth_2$D25,
                             species = newdata_depth_2$Species,
                             soil = newdata_depth_2$`Soil Type`,
                             altitude = newdata_depth_2$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_2$Aspect,
                             slope = newdata_depth_2$Slope)

data50_depth_2 <- data.frame(y = diffs_df_depth_2$D50,
                             species = newdata_depth_2$Species,
                             soil = newdata_depth_2$`Soil Type`,
                             altitude = newdata_depth_2$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_2$Aspect,
                             slope = newdata_depth_2$Slope)

data75_depth_2 <- data.frame(y = diffs_df_depth_2$D75,
                             species = newdata_depth_2$Species,
                             soil = newdata_depth_2$`Soil Type`,
                             altitude = newdata_depth_2$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_2$Aspect,
                             slope = newdata_depth_2$Slope)

data25_depth_2$species <- as.factor(data25_depth_2$species)
data25_depth_2$soil <- as.factor(data25_depth_2$soil)
data25_depth_2$aspect <- as.factor(data25_depth_2$aspect)

data50_depth_2$species <- as.factor(data50_depth_2$species)
data50_depth_2$soil <- as.factor(data50_depth_2$soil)
data50_depth_2$aspect <- as.factor(data50_depth_2$aspect)

data75_depth_2$species <- as.factor(data75_depth_2$species)
data75_depth_2$soil <- as.factor(data75_depth_2$soil)
data75_depth_2$aspect <- as.factor(data75_depth_2$aspect)

# save(data25_depth_2, file="data/run models/data25_depth_2.Rda")
# save(data50_depth_2, file="data/run models/data50_depth_2.Rda")
# save(data75_depth_2, file="data/run models/data75_depth_2.Rda")




# -------------------------------------------------------------------------



# Depth = 3
load("data/run models/runs_weighted_depth_3.Rda")
runs_weighted_depth_3
names(runs_weighted_depth_3)[names(runs_weighted_depth_3) == "Longfortpass"] <- "Longfordpass"

# Quantile 0.25 ------------------------------------------------------------

diff25_depth_3 <- lapply(names(runs_weighted_depth_3), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_3[[x]], quantile = 0.25)
})
names(diff25_depth_3) <- names(runs_weighted_depth_3)
diff25_depth_3 <- plyr::ldply(diff25_depth_3) |> plyr::rename(c('.id'='Site',
                                                                'V1'='diff'))


# Quantile 0.50 ------------------------------------------------------------

diff50_depth_3 <- lapply(names(runs_weighted_depth_3), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_3[[x]], quantile = 0.5)
})
names(diff50_depth_3) <- names(runs_weighted_depth_3)
diff50_depth_3 <- plyr::ldply(diff50_depth_3) |>  plyr::rename(c('.id'='Site',
                                                                 'V1'='diff'))

# Quantile 0.75 ------------------------------------------------------------

diff75_depth_3 <- lapply(names(runs_weighted_depth_3), function(x){
  diffOP(observed = emergence[[x]], predicted = runs_weighted_depth_3[[x]], quantile = 0.75)
})
names(diff75_depth_3) <- names(runs_weighted_depth_3)
diff75_depth_3 <- plyr::ldply(diff75_depth_3) |>  plyr::rename(c('.id'='Site',
                                                                 'V1'='diff'))

diffs_df_depth_3 <- data.frame(Site = diff75_depth_3$Site,
                               "D25" = diff25_depth_3$diff,
                               "D50" = diff50_depth_3$diff,
                               "D75" = diff75_depth_3$diff)

#save(diffs_df_depth_3, file="data/run models/diffs_df_depth_3.Rda")


# Join with the covariates ------------------------------------------------

load("data/run models/diffs_df_depth_3.Rda")
diffs_df_depth_3

reorderindex_depth_3 <- match(diffs_df_depth_3$Site, summaryData_27_sites$Site)
newdata_depth_3 <- summaryData_27_sites[reorderindex_depth_3,]

data25_depth_3 <- data.frame(y = diffs_df_depth_3$D25,
                             species = newdata_depth_3$Species,
                             soil = newdata_depth_3$`Soil Type`,
                             altitude = newdata_depth_3$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_3$Aspect,
                             slope = newdata_depth_3$Slope)

data50_depth_3 <- data.frame(y = diffs_df_depth_3$D50,
                             species = newdata_depth_3$Species,
                             soil = newdata_depth_3$`Soil Type`,
                             altitude = newdata_depth_3$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_3$Aspect,
                             slope = newdata_depth_3$Slope)

data75_depth_3 <- data.frame(y = diffs_df_depth_3$D75,
                             species = newdata_depth_3$Species,
                             soil = newdata_depth_3$`Soil Type`,
                             altitude = newdata_depth_3$Altitude,
                             #aspect = newdata$`Aspect Resume`,
                             aspect = newdata_depth_3$Aspect,
                             slope = newdata_depth_3$Slope)

data25_depth_3$species <- as.factor(data25_depth_3$species)
data25_depth_3$soil <- as.factor(data25_depth_3$soil)
data25_depth_3$aspect <- as.factor(data25_depth_3$aspect)

data50_depth_3$species <- as.factor(data50_depth_3$species)
data50_depth_3$soil <- as.factor(data50_depth_3$soil)
data50_depth_3$aspect <- as.factor(data50_depth_3$aspect)

data75_depth_3$species <- as.factor(data75_depth_3$species)
data75_depth_3$soil <- as.factor(data75_depth_3$soil)
data75_depth_3$aspect <- as.factor(data75_depth_3$aspect)

# save(data25_depth_3, file="data/run models/data25_depth_3.Rda")
# save(data50_depth_3, file="data/run models/data50_depth_3.Rda")
# save(data75_depth_3, file="data/run models/data75_depth_3.Rda")








