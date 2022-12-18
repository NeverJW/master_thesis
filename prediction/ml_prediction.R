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

# for the machine learning model
# filter the paper which has a larger than 1000 citations
data <-
  final_data[, c(8:13, 26, 27, 30, 31, 33, 34, 41, 44:45, 49, 63)]

model_data <-
  data %>% filter(citation <= 1000 & citation > 0)

model_data <- model_data[order(model_data$month), ]
# remove na value
model_data <- na.omit(model_data)

# remove -inf value
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
  lm(log(citation) ~ .,
     data = df_train)

# the dott means that all rest variables
summary(lm_model)

d <-
  data.frame(actual = log(df_test$citation),
             predicted = predict(lm_model, df_test))

# calculate r squared
rss <- sum((d$predicted - d$actual) ^ 2)  ## residual sum of squares
tss <- sum((d$actual - mean(d$actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss

# plot actual vs. predicted
plot_lm <- ggplot(d, aes(x = actual, y = predicted)) +
  geom_point(alpha = .5,
             size = 1,
             color = "darkred") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "dark blue",
    linetype = "dashed",
    size = 0.7
  )  +   xlim(0, 10) + ylim(0, 15) +
  theme_minimal() + labs(x = NULL, y = NULL) +
  labs(title = "lm")

# R^2: variance explained
modelr::rsquare(lm_model, df_test)

# MAE: mean absolute error
modelr::mae(lm_model, df_test)
modelr::mae(lm_model, df_train)

# mae of training data
sum(abs(log(df_train$citation) - lm_model$fitted.values)) / length(log(df_train$citation))


######################### random forest ###################
# run random forest model
rf <- randomForest(citation ~ ., data = df_train)

# predicted on testing data
pred_rf <- predict(rf, df_test)

# see the performance
rf
importance(rf)
varImpPlot(rf)

d <-
  data.frame(actual = df_test$citation,
             predicted = pred_rf)
d$dif <- abs(d$actual - d$predicted)
plot.d <- d %>% arrange(dif) %>% top_n(-100)
plot.d_worst <- d %>% arrange(dif) %>% top_n(100)

# r squared
rss <-
  sum((pred_rf - df_test$citation) ^ 2)  ## residual sum of squares
tss <-
  sum((df_test$citation - mean(df_test$citation)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss

# MAE: mean absolute error
modelr::mae(rf, df_test)

# plot actual vs. predicted
rf_plot <- ggplot(d, aes(x = actual, y = predicted)) +
  geom_point(alpha = .5,
             size = 1,
             color = "darkred") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "dark blue",
    linetype = "dashed",
    size = 0.7
  )  +   xlim(0, 1000) +
  ylim(0, 1000) + labs(x = NULL, y = NULL) +
  theme_minimal() +
  labs(title = "random forest")

# mae of training data
sum(abs(df_train$citation - rf$predicted)) / length(df_train$citation)

ggplot(plot.d_worst, aes(x = 1:100)) + geom_line(aes(y = actual, color = "actual"),
                                                 lwd = 0.8,
                                                 linetype = 2) +
  geom_line(aes(y = predicted, color = "predicted"),
            lwd = 0.6,
            linetype = 2) +   theme_classic() + labs(x = "Sample - The worst 100 prediction",
                                                     y = "Citations",
                                                     color = "Legend") +
  scale_color_manual(values = c("actual" = "dark red", "predicted" = "blue"))

ggplot(plot.d, aes(x = 1:100)) + geom_line(aes(y = actual),
                                           color = "dark red",
                                           lwd = 0.8,
                                           linetype = 2) +
  geom_line(
    aes(y = predicted),
    color = "blue",
    lwd = 0.6,
    linetype = 2,
  ) +   theme_classic() + labs(x = "Sample - The best 100 prediction",
                               y = "Citations")

######################### svm regression ###################
svm_model <-
  ksvm(log(citation) ~ .,
       data = df_train,
       kernel = "rbfdot",
       epsilon = 1)

pred_svm <- predict(svm_model, df_test, type = "response")

d <-
  data.frame(actual = log(df_test$citation),
             predicted = pred_svm)

rss <- sum((d$predicted - d$actual) ^ 2)  ## residual sum of squares
tss <- sum((d$actual - mean(d$actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss

# mae
# MAE: mean absolute error
sum(abs(log(df_test$citation) - pred_svm)) / length(log(df_test$citation))
sum(abs(
  log(df_train$citation) - predict(svm_model, df_train, type = "response")
)) / length(log(df_train$citation))

# plot actual vs. predicted
svm_plot <- ggplot(d, aes(x = actual, y = predicted)) +
  geom_point(alpha = .5,
             size = 1,
             color = "darkred") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "dark blue",
    linetype = "dashed",
    size = 0.7
  ) +  xlim(0, 10) + ylim(0, 15) +
  theme_minimal() + labs(x = NULL, y = NULL) +
  labs(title = "svr")

ggplot() + geom_point(aes(x = df_test$agg_first_two, y = log(df_test$citation))) +
  geom_point(aes(x = df_test$agg_first_two, y = pred_svm), color = 'blue')


################################# xgboost #########################
lab <- df_train[, "citation"] %>% as.matrix()
train_data <-
  data.matrix(df_train[, colnames(df_train) != "citation"])

xgb <-
  xgboost::xgboost(
    data =  train_data,
    label = lab,
    nrounds = 40,
    objective = "reg:squarederror",
    max_depth = 2,
    eta = .1,
    eval_metric = "mae"
  )

y_pred <-
  predict(xgb, as.matrix(df_test[, colnames(df_test) != "citation"]))

# feature importance
importance_matrix <-
  xgb.importance(colnames(as.matrix(df_train[, colnames(df_train) != "citation"])), model = xgb)
xgb.plot.importance(importance_matrix)

# r squared
rss <-
  sum((y_pred - df_test$citation) ^ 2)  ## residual sum of squares
tss <-
  sum((df_test$citation - mean(df_test$citation)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss

# mae
sum(abs(df_test$citation - y_pred)) / length(df_test$citation)
xgb[["evaluation_log"]][["train_mae"]]

rss <-
  sum((predict(xgb, as.matrix(df_train[, colnames(df_train) != "citation"])) - df_train$citation) ^ 2)  ## residual sum of squares
tss <-
  sum((df_train$citation - mean(df_train$citation)) ^ 2)  ## total sum of squares
rsq <- 1 - rss / tss

# plot actual vs. predicted
d <-
  data.frame(actual = df_test$citation,
             predicted = y_pred)

xgboost_plot <- ggplot(d, aes(x = actual, y = predicted)) +
  geom_point(alpha = .5,
             size = 1,
             color = "darkred") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "dark blue",
    linetype = "dashed",
    size = 0.7
  )  + labs(x = NULL, y = NULL) +
  xlim(0, 1000) +
  ylim(0, 1000) +
  theme_minimal() +
  labs(title = "xgboost")

ggplot() + geom_point(aes(x = df_test$agg_first_two, y = df_test$citation)) +
  geom_point(aes(x = df_test$agg_first_two, y = y_pred), color = 'blue')

# visulize all models
yleft <-
  textGrob("predicted",
           rot = 90,
           gp = gpar(fontsize = 15, col = "dark red"))
bottom <-
  textGrob("actual", gp = gpar(fontsize = 15, col = "dark red"))

grid.arrange(
  xgboost_plot ,
  rf_plot +  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ),
  plot_lm,
  svm_plot +  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ),
  nrow = 2,
  ncol = 2,
  left = yleft,
  bottom = bottom
)
