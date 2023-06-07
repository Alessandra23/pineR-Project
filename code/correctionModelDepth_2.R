library(ggplot2)
library(pineR)

remove(list = c())

# Running LOOCV -----------------------------------------------------------

# Load the data
load('data/summaryData_27_sites.Rda')
summaryData_27_sites
#xtable::xtable(summaryData_27_sites)
# xtable::xtable(summaryData_27_sites[,c("Site ID", "Site","Years" ,"Species", "Soil Type" ,
#                         "Altitude", "Weather Station", "Distance",
#                         "Aspect" ,"Slope")])


load('data/run models/data25_depth_2.Rda')
load('data/run models/data50_depth_2.Rda')
load('data/run models/data75_depth_2.Rda')
str(data25_depth_2)
str(data50_depth_2)
str(data75_depth_2)


# Run ML models -----------------------------------------------------------

# Quantile 25% ------------------------------------------------------------

# RM - LASSO
model_lasso_25 <- correctionModel(data = data25_depth_2, model = 'lasso', seed = 023)

# RM - Quadratic
# This one is not in the package, then I need to write the function
getPredicts.quadratic <- function(data = data25_depth_2, seed = 023){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- lm(y ~ (poly(altitude, 2) + poly(slope, 2) + species + soil + aspect), data = data)

    predictions[i] <- predict(model, newdata = validation_set[, -1])
  }
  return(predictions)
}

model_quadratic_25 <- getPredicts.quadratic(data = data25_depth_2)

# Random forest
model_rf_25 <- correctionModel(data = data25_depth_2, model = 'ranger', num.trees = 100,
                               mtry = 2, importance = 'permutation', seed = 023)

# BART
model_bart_25 <- correctionModel(data = data25_depth_2, model = 'bart', num.trees = 100,
                                 seed = 023)

# SVM

model_svm_25 <- correctionModel(data = data25_depth_2, model = 'svm', seed = 023)

# XGBoost

model_xgb_25 <- correctionModel(data = data25_depth_2, model = 'xgboost',
                                max.depth = 2, nrounds = 50, seed = 023)

# NN
# tidy data to run NN

data25_depth_2_NN <- data25_depth_2
# Normalize the data
maxs <- apply(data25_depth_2_NN[,c('y', 'altitude', 'slope')], 2, max)
mins <- apply(data25_depth_2_NN[,c('y', 'altitude', 'slope')], 2, min)
scaled <- as.data.frame(scale(data25_depth_2_NN[,c('y', 'altitude', 'slope')],
                              center = mins,
                              scale = maxs - mins))
data25_depth_2_NN <- cbind(scaled, data25_depth_2_NN[,c('species', 'soil', 'aspect')])
data25_depth_2_NN_matrix <- model.matrix(~ y + altitude + slope + species + soil + aspect, data = data25_depth_2_NN)

colnames(data25_depth_2_NN_matrix)
colnames(data25_depth_2_NN_matrix)[7] <- "aspectNorthFacing"
colnames(data25_depth_2_NN_matrix)[8] <- "aspectNorthWesterly"
colnames(data25_depth_2_NN_matrix)[9] <- "aspectSouthEasterly"
colnames(data25_depth_2_NN_matrix)[10] <- "aspectSouthWesterly"

data_NN_25 <- data25_depth_2_NN_matrix[,-1] |> as.data.frame()

model_nn_25 <- correctionModel(data = data_NN_25, model = 'nn',
                               hidden = 4, algorithm = "rprop+", seed = 023)


models_corr_25_depth_2 <- list(lasso = model_lasso_25,
                               quadratic = model_quadratic_25,
                               rf = model_rf_25,
                               bart = model_bart_25,
                               svm = model_svm_25,
                               xgb = model_xgb_25,
                               nn = model_nn_25
)

# save(models_corr_25_depth_2, file="data/run models/models_corr_25_depth_2.Rda")



# Quantile 50% ------------------------------------------------------------

# RM - LASSO
model_lasso_50 <- correctionModel(data = data50_depth_2, model = 'lasso', seed = 023)

# RM - Quadratic
# This one is not in the package, then I need to write the function
getPredicts.quadratic <- function(data = data25_depth_2, seed = 023){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- lm(y ~ (poly(altitude, 2) + poly(slope, 2) + species + soil + aspect), data = data)

    predictions[i] <- predict(model, newdata = validation_set[, -1])
  }
  return(predictions)
}

model_quadratic_50 <- getPredicts.quadratic(data = data50_depth_2)

# Random forest
model_rf_50 <- correctionModel(data = data50_depth_2, model = 'ranger', num.trees = 100,
                               mtry = 2, importance = 'permutation', seed = 023)

# BART
model_bart_50 <- correctionModel(data = data50_depth_2, model = 'bart', num.trees = 100,
                                 seed = 023)

# SVM

model_svm_50 <- correctionModel(data = data50_depth_2, model = 'svm', seed = 023)

# XGBoost

model_xgb_50 <- correctionModel(data = data50_depth_2, model = 'xgboost',
                                max.depth = 2, nrounds = 50, seed = 023)

# NN
# tidy data to run NN

data50_depth_2_NN <- data50_depth_2
# Normalize the data
maxs <- apply(data50_depth_2_NN[,c('y', 'altitude', 'slope')], 2, max)
mins <- apply(data50_depth_2_NN[,c('y', 'altitude', 'slope')], 2, min)
scaled <- as.data.frame(scale(data50_depth_2_NN[,c('y', 'altitude', 'slope')],
                              center = mins,
                              scale = maxs - mins))
data50_depth_2_NN <- cbind(scaled, data50_depth_2_NN[,c('species', 'soil', 'aspect')])
data50_depth_2_NN_matrix <- model.matrix(~ y + altitude + slope + species + soil + aspect, data = data50_depth_2_NN)

colnames(data50_depth_2_NN_matrix)
colnames(data50_depth_2_NN_matrix)[7] <- "aspectNorthFacing"
colnames(data50_depth_2_NN_matrix)[8] <- "aspectNorthWesterly"
colnames(data50_depth_2_NN_matrix)[9] <- "aspectSouthEasterly"
colnames(data50_depth_2_NN_matrix)[10] <- "aspectSouthWesterly"

data_NN_50 <- data50_depth_2_NN_matrix[,-1] |> as.data.frame()

model_nn_50 <- correctionModel(data = data_NN_50, model = 'nn',
                               hidden = 4, algorithm = "rprop+", seed = 023)


models_corr_50_depth_2 <- list(lasso = model_lasso_50,
                               quadratic = model_quadratic_50,
                               rf = model_rf_50,
                               bart = model_bart_50,
                               svm = model_svm_50,
                               xgb = model_xgb_50,
                               nn = model_nn_50
)

# save(models_corr_50_depth_2, file="data/run models/models_corr_50_depth_2.Rda")


# Quantile 75% ------------------------------------------------------------

# RM - LASSO
model_lasso_75 <- correctionModel(data = data75_depth_2, model = 'lasso', seed = 023)

# RM - Quadratic
# This one is not in the package, then I need to write the function
getPredicts.quadratic <- function(data = data75_depth_2, seed = 023){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- lm(y ~ (poly(altitude, 2) + poly(slope, 2) + species + soil + aspect), data = data)

    predictions[i] <- predict(model, newdata = validation_set[, -1])
  }
  return(predictions)
}

model_quadratic_75 <- getPredicts.quadratic(data = data75_depth_2)

# Random forest
model_rf_75 <- correctionModel(data = data75_depth_2, model = 'ranger', num.trees = 100,
                               mtry = 2, importance = 'permutation', seed = 023)

# BART
model_bart_75 <- correctionModel(data = data75_depth_2, model = 'bart', num.trees = 100,
                                 seed = 023)

# SVM

model_svm_75 <- correctionModel(data = data75_depth_2, model = 'svm', seed = 023)

# XGBoost

model_xgb_75 <- correctionModel(data = data75_depth_2, model = 'xgboost',
                                max.depth = 2, nrounds = 50, seed = 023)

# NN
# tidy data to run NN

data75_depth_2_NN <- data75_depth_2
# Normalize the data
maxs <- apply(data75_depth_2_NN[,c('y', 'altitude', 'slope')], 2, max)
mins <- apply(data75_depth_2_NN[,c('y', 'altitude', 'slope')], 2, min)
scaled <- as.data.frame(scale(data75_depth_2_NN[,c('y', 'altitude', 'slope')],
                              center = mins,
                              scale = maxs - mins))
data75_depth_2_NN <- cbind(scaled, data75_depth_2_NN[,c('species', 'soil', 'aspect')])
data75_depth_2_NN_matrix <- model.matrix(~ y + altitude + slope + species + soil + aspect, data = data75_depth_2_NN)

colnames(data75_depth_2_NN_matrix)
colnames(data75_depth_2_NN_matrix)[7] <- "aspectNorthFacing"
colnames(data75_depth_2_NN_matrix)[8] <- "aspectNorthWesterly"
colnames(data75_depth_2_NN_matrix)[9] <- "aspectSouthEasterly"
colnames(data75_depth_2_NN_matrix)[10] <- "aspectSouthWesterly"

data_NN_75 <- data75_depth_2_NN_matrix[,-1] |> as.data.frame()

model_nn_75 <- correctionModel(data = data_NN_75, model = 'nn',
                               hidden = 4, algorithm = "rprop+", seed = 023)



models_corr_75_depth_2 <- list(lasso = model_lasso_75,
                               quadratic = model_quadratic_75,
                               rf = model_rf_75,
                               bart = model_bart_75,
                               svm = model_svm_75,
                               xgb = model_xgb_75,
                               nn = model_nn_75
)


# save(models_corr_75_depth_2, file="data/run models/models_corr_75_depth_2.Rda")



