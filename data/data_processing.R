final_data <- final_data[,-c(1, 6, 9, 10, 12, 15, 19, 44:53)]

# whether the paper is the commentary paper
final_data$commentary <- NA
for (i in 1:nrow(final_data)) {
  if (str_detect(final_data$title[i], "Commentaries|Commentary|Comment") ==
      TRUE) {
    final_data$commentary[i] <- 1
  } else {
    final_data$commentary[i] <- 0
  }
  
}

count(final_data$commentary==1) # 38

for (i in 1:nrow(final_data)) {
  
  if (final_data$cinew_max[i] > 1500 & is.na(final_data$cinew_max[i])==FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 & is.na(final_data$cinew_max[i])==FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 & is.na(final_data$cinew_max[i])==FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 & is.na(final_data$cinew_max[i])==FALSE) {
    final_data$superstar3[i] <- 1
  } else {
    final_data$superstar3[i] <- 0
  }
  
}

# create the variable which indicates the differences of authors' citations
final_data$diffauthor <-
  final_data$cinew_max - final_data$cinew_min

# if there is a big difference between the authors then we will have a dummy variable
final_data$diff <- NA
for (i in 1:nrow(final_data)) {
  if (final_data$diffauthor[i] > 1500 &
      is.infinite(final_data$diffauthor[i]) == FALSE) {
    final_data$diff[i] <- 1
  } else {
    final_data$diff[i] <- 0
  }
  
}


# # create a normalization function
# normalize <- function(x) {
#   return ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
# }
# 
# model_data_stand <- as.data.frame(lapply(model_data[2:22], normalize))
# model_data_stand <- data.frame(citation = model_data$citation, model_data_stand)

# create the variables
final_data$aggre_21_ci <- rowSums(final_data[,50:61],na.rm = TRUE)
final_data <- final_data %>% filter(!is.na(aggre_21_ci))
# 
# # create target variable citations in the first 5 year after publication year
# final_data$five_ci <- NA 
#   for (i in 1:2805) {
#     if (final_data$year[i] <= 2017) {
#       final_data$five_ci[i] <- rowSums(final_data[,(final_data$year[i]-1960):(final_data$year[i]-1960+4)],na.rm = TRUE) 
#     } else {final_data$five_ci[i] <- NA}
#   }
# # 

# create the variable of total first two year citations
for (i in 1:2805) {
  if (final_data$year[i] != 2021) {
    final_data$agg_first_two[i] <-
      final_data[i, (final_data$year[i] - 1960)] + final_data[i, (final_data$year[i] - 1960 + 1)]
  } else {
    final_data$agg_first_two[i] <- NA
  }
}
final_data$agg_first_two <- as.numeric(final_data$agg_first_two)
