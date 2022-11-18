# check tensorflow
tf$constant("Hellow Tensorflow")
# reticulate::install_miniconda()
# devtools::install_github("rstudio/reticulate")
source("setup.R")

data <- time_series_final[,c(8:12,30,33,44:47,62:63)]
# data$ab_pop <- if(data$abstract_pop==TRUE){}
data <- as.matrix(data)
dimnames(data) <- NULL
data <- na.omit(data)

set.seed(1234)
ind <-
  sample(2, nrow(data), replace = T, prob = c(.7, .3)) #从 1，2 中有放回抽取一个数，概率分别为（0.7，0.3）
training <- data[ind == 1, c(1:11, 13)]
test <- data[ind == 2, c(1:11, 13)]
trainingtarget <- data[ind == 1, 12]
testtarget <- data[ind == 2, 12]

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)
model <- keras_model_sequential() %>% 
  layer_dense(units = 30, activation = 'relu', input_shape = c(12)) %>%
  layer_dense(units = 15) %>%
  layer_dense(units = 1)

summary(model)
model %>% compile(loss = 'mse', #损失函数
                  optimizer = 'rmsprop', #优化器
                  metrics = 'mse'#监控度量
)
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 400,
      batch_size = 64,
      validation_split = 0.4)
model %>% keras::evaluate(test, testtarget)


# data %<>% mutate_if(is.factor, as.numeric)
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
