library(caret)

# Running LOOCV -----------------------------------------------------------

load('data/summaryData_27_sites.Rda')
summaryData_27_sites
#xtable::xtable(summaryData_27_sites)

xtable::xtable(summaryData_27_sites[,c("Site ID", "Site","Years" ,"Species", "Soil Type" ,
                        "Altitude", "Weather Station", "Distance",
                        "Aspect" ,"Slope")])
load('data/run models/data25_depth_1.Rda')
data25_depth_1

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV",
                     savePredictions = 'final')

# RF

set.seed(022)
model_rf <- train(days~.,
                 data = data25_depth_1,
                 method = "rf",
                 #method = "ranger",
                 #ntree = 100,
                 prox=TRUE,
                 metric='RMSE',
                 allowParallel=TRUE,
                 importance = TRUE,
                 trControl = ctrl)

df_rf <- data.frame(site = summaryData_27_sites$Site,
                 actual = data25_depth_1$days,
                 predicted = model_rf$pred$pred)

ggplot(df, aes(actual,predicted)) +
  geom_point()  +
  geom_smooth(aes(x = actual, y = predicted), method = "lm", color = "steelblue",
              formula = y ~ x, fill = "steelblue", alpha = 0.2) +
  theme_bw() + xlim(-100, 50)+
  labs(x = "Observed", y = "Predicted")

# BART

model_bart <- train(days ~ .,
                    data = data25_depth_1,
                    method ="bartMachine",
                    trControl = ctrl,
                    tuneGrid = data.frame(num_trees = 500, k = 3, alpha = 0.1, beta = 0.1, nu = 4))


df_bart <- data.frame(site = summaryData_27_sites$Site,
                 actual = data25_depth_1$days,
                 predicted = model_bart$pred$pred)

ggplot(df_bart, aes(actual,predicted)) +
  geom_point()  +
  geom_smooth(aes(x = actual, y = predicted), method = "lm", color = "steelblue",
              formula = y ~ x, fill = "steelblue", alpha = 0.2) +
  theme_bw() + xlim(-100, 50)+
  labs(x = "Observed", y = "Predicted")


# SVM

model_svm <- train(days ~ .,
                   data = data25_depth_1,
                   method ="svmLinear",
                   trControl = ctrl,
                   tuneGrid = expand.grid(C = seq(0, 2, length = 27)))


df_svm <- data.frame(site = summaryData_27_sites$Site,
                      actual = data25_depth_1$days,
                      predicted = model_svm$pred$pred)

ggplot(df_svm, aes(actual,predicted)) +
  geom_point()  +
  geom_smooth(aes(x = actual, y = predicted), method = "lm", color = "steelblue",
              formula = y ~ x, fill = "steelblue", alpha = 0.2) +
  theme_bw() + xlim(-100, 50)+
  labs(x = "Observed", y = "Predicted")

# radial
model_svm <- train(days ~ .,
                   data = data25_depth_1,
                   method ="svmRadial",
                   trControl = ctrl)


df_svm <- data.frame(site = summaryData_27_sites$Site,
                     actual = data25_depth_1$days,
                     predicted = model_svm$pred$pred)

ggplot(df_svm, aes(actual,predicted)) +
  geom_point()  +
  geom_smooth(aes(x = actual, y = predicted), method = "lm", color = "steelblue",
              formula = y ~ x, fill = "steelblue", alpha = 0.2) +
  theme_bw() + xlim(-100, 50)+
  labs(x = "Observed", y = "Predicted")

# LR LASSO



