
# Upload datafile
work_dir <- getwd()
route <- file.path(work_dir, "physio_visit.csv")
physio_visit <- read.csv(route, sep = ";")

# We create a new dataset for the IPAQ test results, excluding patient 1 and 2 because they didn't finish de CRP
ipaq <- physio_visit[!(physio_visit$Patient %in% c("Patient 1", "Patient 2")), ]
ipaq$IPAQ.result <- as.factor(ipaq$IPAQ.result)

# Separe IPAQ basal results
ipaq_basal <- ipaq[ipaq$Visit == "1", ]
ipaq_basal$IPAQ.result <- as.factor(ipaq_basal$IPAQ.result)
summary(ipaq_basal)

# Separe IPAQ final results
ipaq_final <- physio_visit[physio_visit$Visit == "2", ]
ipaq_final$IPAQ.result <- as.factor(ipaq_basal$IPAQ.result)

summary(ipaq_final)

table(ipaq_basal$IPAQ.result)
prop.table(table(ipaq_basal$IPAQ.result))*100

table(ipaq_final$IPAQ.result)
prop.table(table(ipaq_final$IPAQ.result))*100

basal_final <- rbind(ipaq_basal, ipaq_final)
contingency_table <- table(basal_final$IPAQ.result, basal_final$Visit)
contingency_table
contingency_matrix <- as.matrix(contingency_table)

colnames(contingency_matrix) <- c("Basal","Final")
contingency_matrix

# McNemar's test to compare categorical results in physical exercise
intense <- contingency_matrix[1:2, ]
result_mcNemar_intense <- mcnemar.test(intense)
print(result_mcNemar_intense)

low <- contingency_matrix[c(1, 3), ]
result_mcNemar_low <- mcnemar.test(low)
print(result_mcNemar_low)

moderate <- contingency_matrix[2:3, ]
result_mcNemar_moderate <- mcnemar.test(moderate)
print(result_mcNemar_moderate)

#Total means
mean(ipaq_basal$IPAQ.test)
sd(ipaq_basal$IPAQ.test)
summary(ipaq_basal$IPAQ.test)

mean(na.omit(ipaq_final$IPAQ.test))
sd(na.omit(ipaq_final$IPAQ.test))

#Statistic test to compare means
ipaq_final <- ipaq_final[ipaq_final$Patient != "Patient 2", ]

shapiro.test(ipaq_basal$IPAQ.test) #Normality test
shapiro.test(ipaq_final$IPAQ.test)

t.test(ipaq_basal$IPAQ.test, ipaq_final$IPAQ.test)

#Aerobic exercise summary
mean(na.omit(physio_visit$Aerobic_Frequency))
sd(na.omit(physio_visit$Aerobic_Frequency))
summary(na.omit(physio_visit$Aerobic_Frequency))

mean(na.omit(physio_visit$Aerobic_borg))
sd(na.omit(physio_visit$Aerobic_borg))
summary(na.omit(physio_visit$Aerobic_borg))

mean(na.omit(physio_visit$Aerobic_time))
sd(na.omit(physio_visit$Aerobic_time))
summary(na.omit(physio_visit$Aerobic_time))

#Strength exercise summary
mean(na.omit(physio_visit$Strength_Frequency))
sd(na.omit(physio_visit$Strength_Frequency))
summary(na.omit(physio_visit$Strength_Frequency))

mean(na.omit(physio_visit$Strength_borg))
sd(na.omit(physio_visit$Strength_borg))
summary(na.omit(physio_visit$Strength_borg))

mean(na.omit(physio_visit$Strength_time))
sd(na.omit(physio_visit$Strength_time))
summary(na.omit(physio_visit$Strength_time))
