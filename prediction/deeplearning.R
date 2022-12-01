# check tensorflow
tf$constant("Hellow Tensorflow")
# reticulate::install_miniconda()
# devtools::install_github("rstudio/reticulate")
source("setup.R")

data <- final_data[, c(8:13, 26, 27, 30, 31, 33, 34, 41, 44:45, 48, 49, 63)]

model_data <-
  data %>% filter(citation < 4000 & citation > 0 & page > 0)

if(isTRUE(model_data$abstract_pop)){model_data$abstract_pop=1} else {model_data$abstract_pop=0}

deep_data <- as.matrix(model_data)
dimnames(deep_data) <- NULL
deep_data <- na.omit(deep_data)


set.seed(42)
ind <-
  sample(2, nrow(deep_data), replace = T, prob = c(.7, .3)) #从 1，2 中有放回抽取一个数，概率分别为（0.7，0.3）
training <- deep_data[ind == 1, c(1:9,11:18)]
test <- deep_data[ind == 2, c(1:9,11:18)]
trainingtarget <- deep_data[ind == 1, 10]
testtarget <- deep_data[ind == 2, 10]

df_train <- df_train %>% na.omit()
df_test <- df_test %>% na.omit()

training <- df_train[,-10] %>% as.matrix()
test <- df_test[,-10]%>% as.matrix()
trainingtarget <- df_train[10] %>% as.matrix()
testtarget <- df_test[10] %>% as.matrix()

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)
model <- keras_model_sequential() %>% 
  layer_dense(units = 30, activation = 'relu', input_shape = c(17)) %>%
  layer_dense(units = 15, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model)
model %>% compile(loss = 'mse', #损失函数
                  optimizer = 'rmsprop', #优化器
                  metrics = 'mae'#监控度量
)
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 300,
      batch_size = 32,
      validation_split = 0.3)
model %>% keras::evaluate(test, testtarget)


pred <- predict(model,test) 
mean((testtarget-pred)^2)

ev_data = data.frame("Item" = seq(1,length(pred)),
                     "Value" = c(testtarget,pred),
                     "Class" = rep(c("True","Pred"),each = length(pred)))
ggplot(ev_data) +
  geom_line(aes(Item,Value,col = Class,lty = Class)) +
  scale_color_aaas() +
  theme_bw() + 
  theme(panel.grid = element_blank())

rss <- sum((pred - testtarget) ^ 2)  ## residual sum of squares
tss <- sum((testtarget - mean(testtarget)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

mean(abs((testtarget-pred)/testtarget))*100
