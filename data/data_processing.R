# select final data
final_data <- final_data[, -c(1, 6, 9, 10, 12, 15, 19, 44:53)]

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

count(final_data$commentary == 1) # 39

# superstar dummy variable
for (i in 1:nrow(final_data)) {
  if (final_data$cinew_max[i] > 1500 &
      is.na(final_data$cinew_max[i]) == FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 &
             is.na(final_data$cinew_max[i]) == FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 &
             is.na(final_data$cinew_max[i]) == FALSE) {
    final_data$superstar3[i] <- 1
  } else if (final_data$cinew_max[i] > 1500 &
             is.na(final_data$cinew_max[i]) == FALSE) {
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

# create the variables
final_data$aggre_21_ci <- rowSums(final_data[, 50:61], na.rm = TRUE)
final_data <- final_data %>% filter(!is.na(aggre_21_ci))

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

# convert abstract to dummy variable
for (i in 1:2675) {
  if (isTRUE(final_data$abstract_pop[i])) {
    final_data$abstract_pop[i] = 1
  }
}

# descriptive statistics
labs <- c(
  'number of authors',
  'page',
  'reference counts',
  'journal impact factor',
  'length of title',
  'abstract popularity - dummy',
  'mean of hindex',
  'maximum of author citations',
  'publication month',
  'citations(Y)',
  'recency',
  'minimum of hindex',
  'maximum of citations in 2021',
  'superstar - dummy',
  'commentary - dummy',
  'difference of author citations - dummy',
  'citations in the first two years'
)


sumtable(
  model_data,
  summ = c('notNA(x)',
           'mean(x)',
           'median(x)',
           'min(x)', 'max(x)'),
  summ.names = list(c('N', 'Mean', 'Median', 'Min', 'Max')),
  labels = labs
)
