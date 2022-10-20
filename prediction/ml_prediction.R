###################### machine learning model ################################
source("setup.R")

# examine the dataset
str(base_data_with_date)

# convert variable impactf to string
base_data_with_date$impactf <- as.numeric(base_data_with_date$impactf)

# examine the distribution of citations
summary(base_data_with_date)
summary(base_data_with_date$citation)
hist(base_data_with_date$citation,
     breaks = 400,
     xlim = c(0, 500))

# delete the outline which indicate citations > 0
model_data <-
  base_data_with_date %>% filter(citation < 1000 & citation > 0 & pages > 1) 

# select the variable we used
model_data <-
  model_data %>% select(
    citation,
    author_number,
    page,
    reference_count,
    impactf,
    title_length,
    keyword_pop,
    abstract_pop,
    # max_hindex,
    # max_cite,
    recency,
    # m_cite,
    # m_hindex,
    # hindex_au1,
    # hindex_au2,
    # superstar2,
    superstar3,
    # minhindex,
    # meanci_min,
    # meanci_mean,
    # meanci_max,
    # mean1_ci,
    # mean2_ci,
    # num1,
    # num2,
    # cinew_max,
    # cinew_min
    # cinew_mean
    cite21_1,
    cite21_2
    # cite21_3,
    # cite21_4
  )

# remove na value
# model_data <- na.omit(model_data)

# remove -inf value
model_data <- model_data[!is.infinite(rowSums(model_data)),]

### linear regression
set.seed(42)
# partition data in training and test sample: 80% vs. 20%
trainIndex <-
  caret::createDataPartition(model_data$citation,
                             p = 0.8,
                             times = 1,
                             list = FALSE)
# convert list to data frames
df_train <- model_data[trainIndex,] # training sample
df_test <- model_data[-trainIndex,] # test sample

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
enet_data <- model_data[complete.cases(model_data[,c(13:20,33:34)]),]
set.seed(42)
# partition data in training and test sample: 80% vs. 20%
trainIndex <- caret::createDataPartition(enet_data$citations, p=0.8, times=1, list=FALSE)
# convert list to data frames
df_train <- enet_data[trainIndex, ] # training sample
df_test <- enet_data[-trainIndex, ] # test sample


enet_model <-
  train(
    citations ~ author_number + page + reference_count + impactf + title_length + keyword_pop + abstract_pop + m_hindex + max_cite,
    data = df_train,
    method = "enet",
    trControl = trainControl("cv", number = 10)
  )
# inspect model
enet_model$bestTune  # LASSO


######################### svm regression ###################
svm_model <-
  ksvm(
    citations ~ .,
    data = df_train,
    kernel = "vanilladot"
  )

pred_svm <- predict(svm_model, df_test, type = "response")

# evaluate performance
mean(abs(df_test$citations - pred_svm)) # package kernlab

######################### random forest ###################
# run random forest model
set.seed(42)
data_rf <- na.omit(model_data)

# partition data in training and test sample: 80% vs. 20%
trainIndex <-
  caret::createDataPartition(data_rf$citations,
                             p = 0.8,
                             times = 1,
                             list = FALSE)

# convert list to data frames
df_train <- data_rf[trainIndex,] # training sample
df_test <- data_rf[-trainIndex,]

rf <- randomForest(citations ~ ., data = df_train)

# predicted on testing data
pred_rf <- predict(rf, df_test)

# see the performance
confusionMatrix(table(pred_rf, df_test$citations))



