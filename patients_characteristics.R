# PATIENTS CHARACTERISTICS

library(readxl)
library(forcats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)


# Upload the file
work_dir <- getwd()
route <- file.path(work_dir, "patients_characteristics.xlsx")
patients_characteristics <- read_excel(route)

str(patients_characteristics)
colnames(patients_characteristics)

# Transform variables into categorical
categorical_vars <- c("Patient","Sex","Tobacco","Alcohol","Fagerström","Diabetes","HBP","Dyslipidemia","Obesity","Stress","Sedentarism","Early family history")
patients_characteristics[categorical_vars] <- lapply(patients_characteristics[categorical_vars], fct_inorder)

# Function to obtain categorical summaries
get_variable_summary <- function(patients_characteristics, categorical_vars) {
  summary_list <- lapply(patients_characteristics[categorical_vars], function(variable) {
    freq_table <- table(variable)
    percent_table <- prop.table(freq_table) * 100
    
    summary_df <- data.frame(
      Levels = as.character(names(freq_table)),
      Frequency = as.numeric(freq_table),
      Percentage = as.numeric(percent_table)
    )
    
    return(summary_df)
  })
  
  names(summary_list) <- categorical_vars
  return(summary_list)
}

summary_result <- get_variable_summary(patients_characteristics, categorical_vars)

# Plot cardiovascular risk factors
risk_factors <- c("Tobacco","Alcohol","Diabetes","HBP","Dyslipidemia","Obesity","Stress","Sedentarism","Early family history")

for (variable in risk_factors) {
  patients_characteristics[[variable]] <- factor(
    patients_characteristics[[variable]],
    levels = unique(patients_characteristics[[variable]])
  )
}

plots <- lapply(risk_factors, function(variable) {
  ggplot(patients_characteristics, aes(x = !!sym(variable), fill = !!sym(variable))) +
    geom_bar(show.legend = FALSE) +
    scale_fill_brewer(palette = "Spectral") +
    labs(title = paste("Frequency of", variable),
         x = "Response",
         y = "Frequency") +
    theme_minimal()
})

# Print the grid
do.call(grid.arrange, plots)


# Numeric variables
patients_characteristics$Age <- as.numeric(patients_characteristics$Age)
summary(patients_characteristics$Age)
sd(patients_characteristics$Age)

summary(patients_characteristics$`TA sist INICIAL`)
summary(patients_characteristics$`TA dias INICIAL`)

patients_characteristics$`PES INICIAL` <- as.numeric(patients_characteristics$`PES INICIAL`)
summary(patients_characteristics$`PES INICIAL`)

patients_characteristics$TALLA <- as.numeric(patients_characteristics$TALLA)
summary(patients_characteristics$TALLA)

patients_characteristics$`IMC INICIAL` <- as.numeric(patients_characteristics$`IMC INICIAL`)
summary(patients_characteristics$`IMC INICIAL`)

patients_characteristics$`LDL INICIAL` <- as.numeric(patients_characteristics$`LDL INICIAL`)
summary(patients_characteristics$`LDL INICIAL`)

patients_characteristics$`TG INICIAL` <- as.numeric(patients_characteristics$`TG INICIAL`)
summary(patients_characteristics$`TG INICIAL`)
