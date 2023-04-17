check_normality <- function(df) {
  result <- data.frame(variable = character(),
                       tested = logical(),
                       p_value = numeric(),
                       normal = logical(),
                       stringsAsFactors = FALSE)
  
  for (i in 1:ncol(df)) {
    variable_name <- names(df)[i]
    tested <- TRUE
    p_value <- NA
    normal <- NA
    
    if (!is.numeric(df[[i]])) {
      tested <- FALSE
    } else if (length(unique(df[[i]])) == 1) {
      normal <- FALSE
    } else {
      normality_test <- shapiro.test(df[[i]])
      p_value <- normality_test$p.value
      normal <- normality_test$p.value >= 0.05
    }
    
    result[i,] <- list(variable_name, tested, p_value, normal)
  }
  
  return(result)
}


chisq_test <- function(df, target) {
  
  # Identify categorical variables
  cat_vars <- sapply(df_factors, is.factor)
  
  target <- "Attrition"
  # Create contingency tables for each categorical variable
  contingency_tables <- lapply(names(df)[cat_vars], function(x) {
    table(df[[x]], df[[target]])
  })
  
  # Run chi-square test for each contingency table
  chi_square_tests <- lapply(contingency_tables, function(x) {
    chisq.test(x)
  })
  
  # Extract p-values from each test
  p_values <- sapply(chi_square_tests, function(x) {
    x$p.value
  })
  
  # Combine variable names and p-values into a data frame
  result_df <- data.frame(variable = names(df)[cat_vars], p_value = p_values)
  
  # Order results by ascending p-value
  result_df <- result_df[order(result_df$p_value), ]
  
  # Return results
  return(result_df)
}

to_factor <- function(df_subset) {
  df[] <- lapply(df, function(x) as.factor(x))
  return(df)
}

remove_unique_1 <- function(df) {
  # make a copy of the original dataframe
  new_df <- df
  
  # loop through each column
  for (col in colnames(df)) {
    # check if the column has a unique value of 1
    if (length(unique(df[[col]])) == 1 ) {
      # if so, remove the column from the new dataframe
      new_df[[col]] <- NULL
      cat(sprintf("Removed column %s because it had a unique value of 1\n", col))
    } 
  }
  # return the new dataframe
  return(new_df)
}

ttest_result <- t.test(df$MonthlyRate, df$Adj_HourlyRate, alternative = "two.sided", var.equal = FALSE)
ttest_result

wilcox.test(YearsAtCompany ~ Attrition, data = df, alternative = "two.sided")

# Calculate quartiles
q1 <- quantile(df1$MonthlyIncome, 0.25)
q2 <- quantile(df1$MonthlyIncome, 0.5)
q3 <- quantile(df1$MonthlyIncome, 0.75)

# Create factor levels based on quartiles
df1$MonthlyIncomeCategory <- cut(df1$MonthlyIncome, 
                                 breaks = c(-Inf, q1, q2, q3, Inf), 
                                 labels = c("Low", "Medium", "High", "Very High"))

# Convert to factor
df1$MonthlyIncomeCategory <- factor(df1$MonthlyIncomeCategory)



df_subset <- df[, c("Attrition","JobLevel", "MonthlyIncome", "TotalWorkingYears", 
                    "PercentSalaryHike", "PerformanceRating", 
                    "YearsAtCompany", "YearsInCurrentRole", 
                    "YearsWithCurrManager" )]

## Creates a matrix with all correlations greater than threshhold
cor_pairs <- function(cor_matrix, threshold = 0.75) {
  cor_matrix[lower.tri(cor_matrix)] <- NA
  pairs <- which(cor_matrix > threshold & cor_matrix < 1, arr.ind = TRUE)
  pairs <- data.frame(Var1 = rownames(cor_matrix)[pairs[,1]],
                      Var2 = colnames(cor_matrix)[pairs[,2]],
                      Correlation = cor_matrix[pairs])
  pairs <- pairs[!duplicated(paste(pmin(pairs$Var1, pairs$Var2), pmax(pairs$Var1, pairs$Var2))), ]
  return(pairs)
}
df_num <- df_numeric(df_subset)
# Get the correlation matrix
cor_matrix <- cor(df_num)

cor_over <- cor_pairs(cor_matrix)
# Find the top 10 correlations
top_cor <- sort(cor_matrix[upper.tri(cor_matrix)], decreasing = TRUE)[1:10]

# Plot the heatmap
ggplot(data = melt(cor_matrix)[upper.tri(cor_matrix), ], aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Top 10 Correlations", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Variables compared to Attrition Correlelation Matrix
df_subset <- df[, c("Attrition", "JobLevel", "MonthlyIncome", "TotalWorkingYears", 
                    "PercentSalaryHike", "PerformanceRating", 
                    "YearsAtCompany", "YearsInCurrentRole", 
                    "YearsWithCurrManager")]

df_subset$Attrition <- as.integer(df_subset$Attrition == "Yes")




########################################

convert_char_to_factor <- function(df) {
  char_vars <- sapply(df, is.character)
  df[, char_vars] <- lapply(df[, char_vars], as.factor)
  return(df)
}

df_model <- convert_char_to_factor(df_model)
# Create a duplicate of the original data frame
df_scaled <- df_model

# Select the variables to scale
vars_to_scale <- c("MonthlyIncome")

# Scale the selected variables in the duplicate data frame
df_scaled[, vars_to_scale] <- scale(df_scaled[, vars_to_scale])

library(caret)

set.seed(123) # for reproducibility
train_index <- createDataPartition(df_model$Attrition, p = 0.7, list = FALSE)
train_data <- df_scaled[train_index, ]
test_data <- df_scaled[-train_index, ]

ctrl <- trainControl(method = "cv", number = 5)
knn_model <- train(Attrition ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 10)

predictions <- predict(knn_model, newdata = test_data)
