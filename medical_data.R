# MEDICAL DATA

library(dataMaid)
library(summarytools)

# Upload the file
work_dir <- getwd()
route <- file.path(work_dir, "medical_data.csv")
medical_data <- read.csv(route, sep = ";")

# Data exploration
str(medical_data)
colnames(medical_data)

# Transform variables into factors
variables_to_factor <- c("Patient","Diagnosis","Debut_diagnosis","Previous_admissions","Location","Affected_vessels","Valve_replacement","AF_presence","Devices")

medical_data <- medical_data %>%
  mutate_at(vars(variables_to_factor), factor)

# Transform FEVE result into numeric
medical_data$FEVE <- as.numeric(medical_data$FEVE)

# EDA
makeDataReport(medical_data)

# Data summary
summary(medical_data)

sd(medical_data$FEVE)

