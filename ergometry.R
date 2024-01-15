library(dplyr)
library(doBy)
library(effectsize)
library(ggplot2)

# Upload data
work_dir <- getwd()
route <- file.path(work_dir, "ergometry.csv")
ergometry <- read.csv(route)

# Transform variables into factors
ergometry$FC_real_max <- as.numeric(ergometry$FC_real_max)
ergometry$X.FC_max._teoric <- as.numeric(ergometry$X.FC_max._teoric)
ergometry$FC_basal <- as.numeric(ergometry$FC_basal)
ergometry$DPM <- as.numeric(ergometry$DPM)
ergometry$CI <- as.numeric(ergometry$CI)
ergometry$Exercise_min <- as.numeric(ergometry$Exercise_min)
ergometry$METs <- as.numeric(ergometry$METs)

# EDA
makeDataReport(ergometria)

# Make two datasets for basal and final
basal <- ergometry[ergometry$Time == "Initial", ]
final <- ergometry[ergometry$Time == "Final", ]

summary(basal$METs)
IQR(basal$METs)

summary(final$METs)
IQR(final$METs)

# Function to calculate mean, standard deviation, median, IQR and min-max in basal ergometry results..
summary_ergo_basal <- function(basal) {
  numeric_columns <- Filter(is.numeric, basal)
  summary_stats <- summary(numeric_columns)
  
  means <- apply(numeric_columns, 2, mean)
  sds <- apply(numeric_columns, 2, sd)
  medians <- sapply(numeric_columns, median)
  iqrs <- sapply(numeric_columns, IQR)
  mins <- apply(numeric_columns, 2, min)
  maxs <- apply(numeric_columns, 2, max)
  
  results_basal <- data.frame(
    Variable = names(means),
    Mean = means,
    SD = sds,
    Median = medians,
    IQR = iqrs,
    Min = mins,
    Max = maxs
  )
  
  return(results_basal)
}

basal_results <- summary_ergo_basal(basal)
print(basal_results)

summary(basal)

# Function to calculate mean, standard deviation, median, IQR and min-max in final ergometry results.
summary_ergo_final <- function(final) {
  numeric_columns <- Filter(is.numeric, final)
  summary_stats <- summary(numeric_columns)
  
  means <- apply(numeric_columns, 2, mean)
  sds <- apply(numeric_columns, 2, sd)
  medians <- sapply(numeric_columns, median)
  iqrs <- sapply(numeric_columns, IQR)
  mins <- apply(numeric_columns, 2, min)
  maxs <- apply(numeric_columns, 2, max)
  
  results_final <- data.frame(
    Variable = names(means),
    Mean = means,
    SD = sds,
    Median = medians,
    IQR = iqrs,
    Min = mins,
    Max = maxs
  )
  
  return(results_final)
}

final_results <- summary_ergo_final(final)
print(final_results)

# Basal and final summary
summary(basal)
summary(final)

# Normality tests

# We delete patient 1, 2 and 7 because they didn't finish the program.
basal <- subset(basal, !(Patient %in% c("Patient 1", "Patient 2", "Patient 7")))

# Normality histogram plots
ggplot(ergometry, aes(x=FC_real_max))+
  geom_histogram(color="black", fill="white", bins = 30) +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=FC_basal))+
  geom_histogram(color="black", fill="white", bins = 30) +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=X.FC_max._teoric))+
  geom_histogram(color="black", fill="white", bins = 30) +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=DPM))+
  geom_histogram(color="black", fill="white", bins = 10) +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=CI))+
  geom_histogram(color="black", fill="white") +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=Exercise_min))+
  geom_histogram(color="black", fill="white") +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

ggplot(ergometry, aes(x=METs))+
  geom_histogram(color="black", fill="white") +
  facet_grid(Timepoint ~ .) +
  theme_minimal()

# Function to perfom normality Shapiro-test for the numeric variables.
normality_test <- function(basal, final, variables, alpha = 0.05) {
  results <- data.frame(Variable = character(), P_valor_Basal = numeric(), P_valor_Final = numeric(), Normality = character(), stringsAsFactors = FALSE)
  
  for (variable in variables) {
    test_result_basal <- shapiro.test(basal[[variable]])
    test_result_final <- shapiro.test(final[[variable]])
    
    normality_basal <- ifelse(test_result_basal$p.value > alpha, "Normal", "No normal")
    normality_final <- ifelse(test_result_final$p.value > alpha, "Normal", "No normal")
    
    results <- rbind(results, data.frame(Variable = variable, 
                                               P_valor_Basal = test_result_basal$p.value, 
                                               P_valor_Final = test_result_final$p.value,
                                               Normality = paste(normality_basal, "/", normality_final)))
  }
  
  print(results)
}

variables_to_analyze <- c("FC_real_max", "X.FC_max._teoric", "FC_basal", "DPM", "CI", "Exercise_min", "METs")
normality_test(basal, final, variables_to_analyze)

# STATISTICAL TESTS FOR PAIRED DATA TO COMPARE MEANS IN BASAL-FINAL
# t-test or wilcoxon test

calculate_t_test_for_variables <- function(basal, final, variables_of_interest, alpha = 0.05) {
  results <- data.frame(Variable = character(), Test = character(), P_value = numeric(), Significant = character(), Diff_means = numeric(), CI_lower = numeric(), CI_upper = numeric(), stringsAsFactors = FALSE)
  
  for (variable in variables_of_interest) {
    variable_basal <- basal[[variable]]
    variable_final <- final[[variable]]
    
    shapiro_basal <- shapiro.test(variable_basal)
    shapiro_final <- shapiro.test(variable_final)
    
    if (shapiro_basal$p.value > alpha & shapiro_final$p.value > alpha) {
      t_test_result <- t.test(variable_basal, variable_final, paired = TRUE)
      test_used <- "T-Test"
    } else {
      wilcox_result <- wilcox.test(variable_basal, variable_final, paired = TRUE, correct = TRUE)
      test_used <- "Wilcoxon"
    }
    
    is_significant <- ifelse(t_test_result$p.value < alpha, "Yes", "No")
    
    diff_means <- mean(variable_final - variable_basal)
    ci <- t.test(variable_final - variable_basal, conf.level = 1 - alpha)$conf.int
    
    results <- rbind(results, data.frame(Variable = variable, Test = test_used, 
                                         P_value = t_test_result$p.value, 
                                         Significant = is_significant, 
                                         Diff_Means = diff_means,
                                         CI_Lower = ci[1],
                                         CI_Upper = ci[2]))
      }
  
  print(results)
}

results <- calculate_t_test_for_variables(basal, final, variables_to_analyze)
results

#COHEN'S D

calculate_cohens_d <- function(basal, final, variables_of_interest) {
  results <- data.frame(Variable = character(), Cohens_D = numeric(), stringsAsFactors = FALSE)
  
  for (variable in variables_of_interest) {
    variable_basal <- basal[[variable]]
    variable_final <- final[[variable]]
    
    cohens_d <- effectsize::cohens_d(variable_final, variable_basal)
    
    results <- rbind(results, data.frame(Variable = variable, Cohens_D = cohens_d))
  }
  
  print(results)
}

cohens_d_results <- calculate_cohens_d(basal, final, variables_to_analyze)
print(cohens_d_results)

