# ALERTS SENT DASHBOARD

library(dplyr)
library(data.table)
library(ggplot2)

# Upload data
work_dir <- getwd()
route <- file.path(work_dir, "alerts_sent_dashboard.csv")
alerts_sent_dashboard <- read.csv(route)

# Look at the variables
str(alerts_sent_dashboard)

# Transformation of variables into factors
alerts_sent_dashboard$target <- as.factor(alerts_sent_dashboard$target)
alerts_sent_dashboard$extraParameters.description <- as.factor(alerts_sent_dashboard$extraParameters.description)
alerts_sent_dashboard$extraParameters.title <- as.factor(alerts_sent_dashboard$extraParameters.title)

# Data summary
summary(alerts_sent_dashboard)

mean(table(alerts_sent_dashboard$target))
sd(table(alerts_sent_dashboard$target))

#Summary of all the alerts sent
alerts_count <- table(alerts_sent_dashboard$extraParameters.description)
alerts_count

# % of each type of alert
proportion <- prop.table(table(alerts_sent_dashboard$extraParameters.description))*100
alarm <- c("Exercise too easy", "Had some symptom", "150 min not reached", "2 days not reached", "Change in intensity")

alerts <- as.data.frame(cbind(alarm, alerts_count, proportion))
alerts$alarm <- as.factor(alerts$alarm)
alerts$proportion <- as.numeric(alerts$proportion)

# Plot of distribution of each alert
ggplot(alerts, aes(x = alarm, y = proportion, fill = alarm)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Distribution of alerts sent to clinics", x = "Alert", y = "Count") +
  geom_text(aes(label = paste0(round(proportion,1),"%")), vjust = -0.5) +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(limits = c(0, 55)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# TITLE

# Number of alerts sent of each type
title_n <- table(alerts_sent_dashboard$extraParameters.title)
# %
title_prop <- prop.table(table(alerts_sent_dashboard$extraParameters.title))*100
title <- as.data.frame(cbind(title_n, title_prop))


# TYPE OF ALERTS PER PATIENT

# % of each alert per patient
prop.table(table(alerts_sent_dashboard$target, alerts_sent_dashboard$extraParameters.description))*100

percent1 <- alerts_sent_dashboard %>%
  group_by(target, extraParameters.description) %>%
  summarise(Freq = n()) %>%
  group_by(extraParameters.description) %>%
  mutate(Percent = Freq/sum(Freq)*100)

percent1$target <- factor(percent1$target, levels = rev(c("Patient 1", "Patient 2", "Patient 3", "Patient 4", "Patient 5", "Patient 6", "Patient 7", "Patient 8", "Patient 9", "Patient 10", "Patient 11", "Patient 12", "Patient 13","Patient 14")))

# Plot distribution of alerts sent by each patient
ggplot(percent1, aes(x = target, y = Percent, fill = extraParameters.description)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of alerts sent by each patient", x = " ", y = "% of each alert", fill = c("Alerts")) +
  geom_text(aes(label = sprintf("%.f%%", Percent)), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral", labels = c("Exercise too easy", "Patient experienced some symptom", "150 min of aerobic exercise not done", "2 days of strength exercise not done", "Change in intensity of the exercise")) +
  theme(legend.position = "right", legend.text = element_text(size = 8))
