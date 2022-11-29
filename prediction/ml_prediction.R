###################### machine learning model ################################
source("setup.R")

# the final dataset is called final_data
# examine the dataset
str(final_data)

# examine the distribution of citations
summary(final_data)
summary(final_data$citation)
hist(final_data$citation,
     breaks = 400,
     xlim = c(0, 1500))


# data <- final_data[,c(8:13,26,27,30,33,34,41,44:45, 48,49, 62,63)]
# model_data <-
#   data %>% filter(aggre_21_ci < 1000 & page > 1)
# for the machine learning model
# filter the paper which has a "normal" citations (larger than 1300)
# in order to avoid the outlier influence
data <- final_data[, c(8:13, 26, 27, 30, 31, 33, 34, 41, 44:45, 48, 49, 63)]

model_data <-
  data %>% filter(citation < 1000 & citation > 0 & page > 0)

# select the variable we used
# model_data <-
#   data %>% select(
#     # citation,
#     author_number,
#     page,
#     reference_count,
#     impactf,
#     title_length,
#     abstract_pop,
#     # max_hindex,
#     # max_cite,
#     # totalcite_au1,
#     # totalcite_au2,
#     recency,
#     # m_cite,
#     # m_hindex,
#     hindex_au1,
#     hindex_au2,
#     superstar3,
#     minhindex,
#     num1,
#     num2,
#     # meanci_min,
#     # meanci_mean,
#     # meanci_max,
#     # mean1_ci,
#     # mean2_ci,
#     # cinew_max,
#     cinew_min,
#     # cinew_mean,
#     cite21_1,
#     cite21_2,
#     # cite21_3,
#     # cite21_4,
#     commentary,
#     # editorial,
#     # rejoinder,
#     diff,
#     five_ci,
#     agg_first_two
#   )

# remove na value
model_data <- model_data %>% filter(!is.na(agg_first_two))

# remove -inf value
model_data$diffauthor <- as.numeric(model_data$diffauthor)
model_data$agg_first_two <- as.numeric(model_data$agg_first_two)

model_data <- model_data[!is.infinite(rowSums(model_data)), ]

### linear regression
set.seed(42)
# partition data in training and test sample: 80% vs. 20%
trainIndex <-
  caret::createDataPartition(model_data$citation,
                             p = 0.8,
                             times = 1,
                             list = FALSE)
# convert list to data frames
df_train <- model_data[trainIndex, ] # training sample
df_test <- model_data[-trainIndex, ] # test sample

summary(df_train$citation)
summary(df_test$citation) # similar

# linear regression
lm_model <-
  lm(citation ~ .,
     data = df_train)

# the dott means that all rest variables
summary(lm_model)

d <-
  data.frame(actual = df_test$citation,
             predicted = predict(lm_model, df_test))

# plot actual vs. predicted
ggplot(d, aes(x = actual, y = predicted)) +
  geom_point(alpha = .5,
             size = 2.5,
             color = "darkred") +
  geom_smooth(method = "lm",
              color = "black",
              size = 1.25) +
  theme_minimal()

# R^2: variance explained
modelr::rsquare(lm_model, df_test)

# MAE: mean absolute error
modelr::mae(lm_model, df_test)

# MSE: mean square error
modelr::mse(lm_model, df_test)

# RMSE: root mean square error
modelr::rmse(lm_model, df_test)

######################### elastic net ##############################
# handle missing value
enet_data <- model_data[complete.cases(model_data), ]
set.seed(42)
# partition data in training and test sample: 80% vs. 20%
trainIndex <-
  caret::createDataPartition(enet_data$citation,
                             p = 0.8,
                             times = 1,
                             list = FALSE)
# convert list to data frames
df_train <- enet_data[trainIndex,] # training sample
df_test <- enet_data[-trainIndex,] # test sample


enet_model <-
  caret::train(
    citation ~ .,
    data = df_train,
    method = "enet",
    trControl = trainControl("cv", number = 10)
  )
# inspect model
enet_model$bestTune  # LASSO


######################### svm regression ###################
svm_model <-
  ksvm(citation ~ .,
       data = df_train,
       kernel = "vanilladot")

pred_svm <- predict(svm_model, df_test, type = "response")

# evaluate performance
mean(abs(df_test$citation - pred_svm)) # package kernlab

######################### random forest ###################
# run random forest model
set.seed(42)
data_rf <- na.omit(model_data)

# partition data in training and test sample: 80% vs. 20%
set.seed(42)
trainIndex <-
  caret::createDataPartition(data_rf$citation,
                             p = 0.8,
                             times = 1,
                             list = FALSE)

# convert list to data frames
df_train <- data_rf[trainIndex, ] # training sample
df_test <- data_rf[-trainIndex, ]

rf <- randomForest(citation ~ ., data = df_train)

# predicted on testing data
pred_rf <- predict(rf, df_test)

# see the performance
rf

################################# xgboost #########################

lab <- df_train[, "citation"] %>% as.matrix()
train_data <-
  data.matrix(df_train[, colnames(df_train) != "citation"])
xgb <-
  xgboost::xgboost(
    data =  train_data,
    label = lab,
    nrounds = 25,
    objective = "reg:squarederror",
    max_depth = 2,
    eta = .1
  )
y_pred <-
  predict(xgb, as.matrix(df_test[, colnames(df_test) != "citation"]))

# feature importance
importance_matrix <-
  xgb.importance(colnames(as.matrix(df_train[, colnames(df_train) != "citation"])), model = xgb)
xgb.plot.importance(importance_matrix[1:20])

rss <-
  sum((y_pred - df_test$citation) ^ 2)  ## residual sum of squares
tss <-
  sum((df_test$citation - mean(df_test$citation)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss


