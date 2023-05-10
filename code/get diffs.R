library(caret)
remove(list = ls())


# function to calculate the difference between the quantiles

diffOP <- function(observed, predicted, quantile = 0.5){
  #nRow <- length(predicted$stumpT)
  #dayPredicted <- quantile(predicted$gemtime_new_t0-nRow, quantile)
  dayPredicted <- predicted[which.min(abs(predicted$qm - quantile)), 1]
  dayObserved <- observed[which.min(abs(observed$prob - quantile)), 1]
  daysDiff <-   dayObserved - dayPredicted
  daysDiff <- round(daysDiff)
  return(daysDiff)
}



# Read emergence data ---------------------------------------------------------------

emergence <- readRDS("data/emergence.RDS")
names(emergence)[names(emergence) == "Glendine"] <- "Glendine1"
names(emergence)[names(emergence) == "Kilurney"] <- "Killurney"
names(emergence)[names(emergence) == "Ballymacshan"] <- "Ballymacshaneboy"
names(emergence)
emergence <- emergence[!(names(emergence) %in% c("Deerpark2","Hortland2", "Oakwood2",
                                                 "Ballinagee2", "Corracloon", "Corrakyle"))]

names(runs_weighted_depth_1) == names(emergence)

# read the data frame with the covariates

load('data/summaryData_27_sites.Rda')
summaryData_27_sites

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


# save(data25_depth_1, file="data/run models/data25_depth_1.Rda")
# save(data50_depth_1, file="data/run models/data50_depth_1.Rda")
# save(data75_depth_1, file="data/run models/data75_depth_1.Rda")








