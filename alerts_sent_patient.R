# ALERTS SENT PATIENT

library(dplyr)
library(data.table)
library(ggplot2)

#Load the data
work_dir <- getwd()
route <- file.path(work_dir, "alerts_sent_patient.csv")
alerts_sent_patient <- read.csv(route)

#Factorize patient labs
labs_patient <- c("Patient 1","Patient 2","Patient 3","Patient 4","Patient 5","Patient 6","Patient 7","Patient 8","Patient 9","Patient 10","Patient 11","Patient 12","Patient 13","Patient 14")
useruuid <- c("24d11abb-38ef-45d4-a2c4-14d57d6d0352","b868b06c-83f5-4045-b2e5-b8dcaa50677d","714a3d8a-e7e3-482a-839a-158db77049c0","c02607b2-d163-427b-8763-bb820ec5c918","d1bb6f9c-ca32-4f15-a276-70fa9094cd5d","dd826b23-5148-4708-aaea-ff039f3713e7","0de1d20b-90e8-4291-b954-30da0bd6b6d1","a8b24537-732b-4c59-bf5c-9c99ed98413b","2749286a-847f-49cd-8b41-f85442d36f8b","f84eb306-a4d3-40de-bd91-d4754b22dc4b","7bf0ed40-9203-429c-bce1-c7aa46c1c368","9a66690b-212a-4219-9f6c-91cbb48b6c37","3d40c3ac-f30f-4d0b-97cf-d5cb9dce0481","3b08f415-af04-424b-b0e1-042b930c679a")
alerts_sent_patient$user_uuid <- factor(alerts_sent_patient$user_uuid, levels = useruuid, labels = labs_patient)

#Omit NA's
alerts_sent_patient <- na.omit(alerts_sent_patient)

#Factorize code
alerts_sent_patient$code <- factor(alerts_sent_patient$code, labels = c("150min","anxiety","fatigue","goodsession","imc","predimedhigh","strengthnd","tobacco"))

#Variables summary
colnames(alerts_sent_patient)
summary(alerts_sent_patient)
dim(alerts_sent_patient)

#Number of alerts per patient
table(alerts_sent_patient$user_uuid)

#Recount of alerts (code)
code_n <- table(alerts_sent_patient$code)
code_n

mean(code_n)
sd(code_n)

#Summary of all the alerts sent
alerts_count <- table(alerts_sent_patient$code)
alerts_count

proportion <- prop.table(table(alerts_sent_patient$code))*100
proportion
alarm <- c("150 min","Anxiety very high","Exercise fatigue","Good session","BMI","PREDIMED","Strength ND","Tobacco")
alarm

alerts <- as.data.frame(cbind(alarm, alerts_count, proportion))
alerts$alarm <- as.factor(alerts$alarm)
alerts$proportion <- as.numeric(as.character(alerts$proportion))

#Plot total alerts sent
ggplot(alerts, aes(x = alerts$alarm, y = alerts$proportion, fill = alerts$alarm)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Distribution of alerts sent to patients", x = "Alert", y = "Count") +
  geom_text(aes(label = paste0(round(proportion,1),"%")), vjust = -0.5) +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(limits = c(0, 80)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# TYPE OF ALERTS PER PATIENT

# % of each alert 
prop.table(table(alerts_sent_patient$user_uuid, alerts_sent_patient$code))*100

percent1 <- alerts_sent_patient %>%
  group_by(user_uuid, code) %>%
  summarise(Freq = n()) %>%
  group_by(code) %>%
  mutate(Percent = Freq/sum(Freq)*100)

percent1$user_uuid <- factor(percent1$user_uuid, levels = rev(c("Patient 1", "Patient 2", "Patient 3", "Patient 4", "Patient 5", "Patient 6", "Patient 7", "Patient 8", "Patient 9", "Patient 10", "Patient 11", "Patient 12", "Patient 13","Patient 14")))

# Plot distribution of alerts sent to each patient
ggplot(percent1, aes(x = user_uuid, y = Percent, fill = code)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of alerts sent to each patient", x = " ", y = "% of each alert", fill = c("Alerts")) +
  geom_text(aes(label = sprintf("%.f%%", Percent)), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral", labels = c("150 min","Anxiety very high","Exercise fatigue","Good session","BMI","PREDIMED","Strength ND","Tobacco")) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

