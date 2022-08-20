###################### machine learning model ################################
# examine the dataset 
str(base_data)

# convert variable impactf to string
base_data$impactf <- as.numeric(base_data$impactf)

# examine the distribution of citations
summary(base_data$citations)
hist(base_data$citations, breaks=400, xlim = c(0,500))

# delete the outline which indicate citations >5000
model_data <- base_data %>% filter(citations<5000)

### linear regression 
set.seed(42)
# partition data in training and test sample: 80% vs. 20%
trainIndex <- caret::createDataPartition(model_data$citations, p=0.8, times=1, list=FALSE)
# convert list to data frames
df_train <- model_data[trainIndex, ] # training sample
df_test <- model_data[-trainIndex, ] # test sample

summary(df_train$citations)
summary(df_test$citations) # similar

# linear regression
lm_model <- lm(citations ~ author_number + page + reference_count + impactf + title_length + keyword_pop + abstract_pop + m_hindex + max_cite, data = df_train)

# the dott means that all rest variables
summary(lm_model)

d <- data.frame(actual=df_test$citations, predicted=predict(lm_model, df_test))
# plot actual vs. predicted
ggplot(d, aes(x=actual, y=predicted)) +
  geom_point(alpha=.5, size=2.5, color="darkred") +
  geom_smooth(method="lm", color="black", size=1.25) +
  theme_minimal()

