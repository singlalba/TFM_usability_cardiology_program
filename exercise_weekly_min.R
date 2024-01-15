library(lubridate)
library(dplyr)
library(ggplot2)
library(stats)
library(factoextra)
library(car)
library(ggrepel)
library(compareGroups)

work_dir <- getwd()
route <- file.path(work_dir, "exercise_weekly_minutes.csv")
exercise_weekly_minutes <- read.csv(route, sep = ",")

# Mostrar las primeras filas del dataframe
head(exercise_weekly_minutes)

#Create function to tranform week number of the year into week date.
getMondayOfWeek <- function(year, week) {
  first_day_of_year <- ymd(paste0(year, "-01-01"))
  days_to_monday <- (week - 1) * 7 + 2
  monday_of_week <- first_day_of_year + days_to_monday
  return(monday_of_week)
}

exercise_weekly_minutes <- mutate(exercise_weekly_minutes, monday_of_week = format(getMondayOfWeek(2023, week_number), "%d-%m-%Y"))

#Factorize patient labs
labs_patient <- c("Patient 1","Patient 2","Patient 3","Patient 4","Patient 5","Patient 6","Patient 7","Patient 8","Patient 9","Patient 10","Patient 11","Patient 12","Patient 13","Patient 14")
useruuid <- c("24d11abb-38ef-45d4-a2c4-14d57d6d0352","b868b06c-83f5-4045-b2e5-b8dcaa50677d","714a3d8a-e7e3-482a-839a-158db77049c0","c02607b2-d163-427b-8763-bb820ec5c918","d1bb6f9c-ca32-4f15-a276-70fa9094cd5d","dd826b23-5148-4708-aaea-ff039f3713e7","0de1d20b-90e8-4291-b954-30da0bd6b6d1","a8b24537-732b-4c59-bf5c-9c99ed98413b","2749286a-847f-49cd-8b41-f85442d36f8b","f84eb306-a4d3-40de-bd91-d4754b22dc4b","7bf0ed40-9203-429c-bce1-c7aa46c1c368","9a66690b-212a-4219-9f6c-91cbb48b6c37","3d40c3ac-f30f-4d0b-97cf-d5cb9dce0481","3b08f415-af04-424b-b0e1-042b930c679a")
exercise_weekly_minutes$user_uuid <- factor(exercise_weekly_minutes$user_uuid, levels = useruuid, labels = labs_patient)

#Omit NA's
exercise_weekly_minutes <- na.omit(exercise_weekly_minutes)

#Factorize categorical variables
exercise_weekly_minutes$week_number <- as.factor(exercise_weekly_minutes$week_number)
exercise_weekly_minutes$monday_of_week <- as.factor(exercise_weekly_minutes$monday_of_week)

#At this point we add manually the week number of the programe since it's different for each patient and it depends on the start date.
work_dir <- getwd()
route <- file.path(work_dir, "program_week.csv")
program_week <- read.csv(route, sep = ";")

exercise_weekly_minutes <- cbind(exercise_weekly_minutes, program_week$program_week)

colnames(exercise_weekly_minutes)[colnames(exercise_weekly_minutes) == "program_week$program_week"] <- "program_week"

exercise_weekly_minutes$program_week <- as.factor(exercise_weekly_minutes$program_week)

#We delete those entrys where program week is 11 because they are a mistake. 
exercise_weekly_minutes <- subset(exercise_weekly_minutes, program_week != "11")

#Sessions per week of the program
table(exercise_weekly_minutes$program_week)
mean(table(exercise_weekly_minutes$program_week))
sd(table(exercise_weekly_minutes$program_week))

#Registers of sessions per patient
table(exercise_weekly_minutes$user_uuid)

ggplot(exercise_weekly_minutes, aes(x = user_uuid)) +
  geom_bar(fill = "#3288BD") +
  coord_flip() +
  labs(title = "Number of sessions of each patient", x = "Patients", y = "Sessions") +
  theme_minimal() +
  scale_x_discrete(limits=rev) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_text(stat = 'count', aes(label = after_stat(count)),
            vjust = 0.5, hjust = -0.7, color = "black", size = 3)

ggplot(exercise_weekly_minutes, aes(x = program_week)) +
  geom_bar(fill = "#3288BD") +
  coord_flip() +
  labs(title = "Number of sessions for week", x = "Week", y = "Sessions") +
  theme_minimal() +
  scale_x_discrete(limits=rev) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_text(stat = 'count', aes(label = after_stat(count)),
            vjust = 0.5, hjust = -0.7, color = "black", size = 3)

## AEROBIC EXERCISE

#Mean and SD of aerobic exercise minutes per week
aerobic_min_mean <- aggregate(exercise_weekly_minutes$exercise_minutes ~ exercise_weekly_minutes$program_week, 
                               data = exercise_weekly_minutes, 
                               FUN = function(x) c(Mean = mean(x), SD = sd(x)))

aerobic_min_mean <- data.frame(aerobic_min_mean$`exercise_weekly_minutes$exercise_minutes`)
aerobic_min_mean$program_week <- c("1":"10")

colnames(aerobic_min_mean) <- c("aerobic_min_mean", "SD","program_week")

ggplot(aerobic_min_mean, aes(x = program_week, y = aerobic_min_mean)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  geom_text(aes(label = sprintf("%.1f", aerobic_min_mean)), vjust = -1) +
  ggtitle("Evolution of aerobic exercise minutes by week") +
  labs(x = "Week number", y = "Exercise minutes") +
  scale_x_continuous(breaks = 1:10) +  
  theme_minimal() +
  scale_y_continuous(limits = c(27,60))

#Mean and SD of aerobic exercise minutes per patient
aerobic_min_mean_patient <- aggregate(exercise_weekly_minutes$exercise_minutes ~ exercise_weekly_minutes$user_uuid, 
                              data = exercise_weekly_minutes, 
                              FUN = function(x) c(Mean = mean(x), SD = sd(x)))
aerobic_min_mean_patient <- as.data.frame(aerobic_min_mean_patient)

names(aerobic_min_mean_patient) <- c("Patient_ID", "Aerobic_Mean")

#Mean of aerobic exercise minutes per week per patient
aerobic_min_mean_sd_patient <- aggregate(exercise_weekly_minutes$exercise_minutes,
                                         by = list(user_uuid = exercise_weekly_minutes$user_uuid,
                                                   program_week = exercise_weekly_minutes$program_week),
                                         FUN = function(x) c(mean = mean(x), sd = sd(x)))
write.csv(aerobic_min_mean_sd_patient, "aerobic_min_mean_sd_patient.csv")

# Merge the mean and standard deviation columns to the original data frame
exercise_weekly_minutes <- merge(exercise_weekly_minutes, aerobic_min_mean_sd_patient, by = c("user_uuid", "program_week"))

#STRENGTH EXERCISE

#Mean and SD of strength exercise minutes per week
strength_min_mean <- aggregate(exercise_weekly_minutes$force_minutes ~ exercise_weekly_minutes$program_week, 
                              data = exercise_weekly_minutes, 
                              FUN = function(x) c(Mean = mean(x), SD = sd(x)))

strength_min_mean <- data.frame(strength_min_mean$`exercise_weekly_minutes$force_minutes`)
strength_min_mean$program_week <- c("1":"10")

colnames(strength_min_mean) <- c("strength_min", "SD","program_week")

#Mean and SD of strength exercise minutes per patient
strength_min_mean_patient <- aggregate(exercise_weekly_minutes$force_minutes ~ exercise_weekly_minutes$user_uuid, 
                               data = exercise_weekly_minutes, 
                               FUN = function(x) c(Mean = mean(x), SD = sd(x)))

strength_min_mean_patient <- data.frame(strength_min_mean_patient)
strength_min_mean_patient

colnames(strength_min_mean_patient) <- c("strength_min", "SD","program_week")


ggplot(strength_min_mean, aes(x = program_week, y = strength_min)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  geom_text(aes(label = sprintf("%.1f", strength_min)), vjust = -1) +
  ggtitle("Evolution of strength exercise minutes by week") +
  labs(x = "Week number", y = "Exercise minutes") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  scale_y_continuous(limits = c(3,13.5))


#Mean of strength exercise minutes per week per patient
strength_min_mean_sd_patient <- aggregate(exercise_weekly_minutes$force_minutes,
                                         by = list(user_uuid = exercise_weekly_minutes$user_uuid,
                                                   program_week = exercise_weekly_minutes$program_week),
                                         FUN = function(x) c(mean = mean(x), sd = sd(x)))
write.csv(strength_min_mean_sd_patient, "strength_min_mean_sd_patient")

#COMARISON BETWEEN MEANS

# Differences by weeks
exercise_weekly_minutes$program_week <- as.factor(exercise_weekly_minutes$program_week)

#Aerobic ex
ggplot(exercise_weekly_minutes, aes(x = program_week, y = exercise_minutes, color = program_week)) +
  geom_boxplot() +
  theme_bw()

#Normality
model_lm <- lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes)

res_df <- data.frame(Residuos = residuals(model_lm))

# Histogram
ggplot(res_df, aes(x = Residuos)) +
  geom_histogram(binwidth = 5, fill = "blue4", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram", x = "Residual", y = "Freq") +
  theme_minimal()

# Q-Q plot of residuals
qqnorm(residuals(lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes)))
qqline(residuals(lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes)), col = 2)

# Shapiro-Wilk
shapiro.test(residuals(lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes)))
#No normality

# Homogeneity of variances
plot(lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes), 1) 

# Levene test
leveneTest(residuals(lm(exercise_minutes ~ program_week, data = exercise_weekly_minutes)) ~ exercise_weekly_minutes$program_week)
# No homogeneity.

#Non parametric test Kruskall wallis

aerobic_kruskal <- kruskal.test(exercise_minutes ~ program_week, data = exercise_weekly_minutes)
aerobic_kruskal

#Force min
ggplot(exercise_weekly_minutes, aes(x = program_week, y = force_minutes, color = program_week)) +
  geom_boxplot() +
  theme_bw()

#Normality
model_lm2 <- lm(force_minutes ~ program_week, data = exercise_weekly_minutes)

res_df2 <- data.frame(Residuos = residuals(model_lm2))

# Histogram
ggplot(res_df2, aes(x = Residuos)) +
  geom_histogram(binwidth = 5, fill = "blue4", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram", x = "Residual", y = "Freq") +
  theme_minimal()

# Q-Q plot of residuals
qqnorm(residuals(lm(force_minutes ~ program_week, data = exercise_weekly_minutes)))
qqline(residuals(lm(force_minutes ~ program_week, data = exercise_weekly_minutes)), col = 2)

# Shapiro-Wilk
shapiro.test(residuals(lm(force_minutes ~ program_week, data = exercise_weekly_minutes)))
#No normality

# Homogeneity of variances
plot(lm(force_minutes ~ program_week, data = exercise_weekly_minutes), 1) 

# Levene test
library(car)
leveneTest(residuals(lm(force_minutes ~ program_week, data = exercise_weekly_minutes)) ~ exercise_weekly_minutes$program_week)
# There's Homogeneity.

force_kruskal <- kruskal.test(force_minutes ~ program_week, data = exercise_weekly_minutes)
force_kruskal

# Post-hoc pairwise wilcox test
class(exercise_weekly_minutes$exercise_minutes)
class(exercise_weekly_minutes$`program_week$program_week`)

aerobic_posthoc <- pairwise.wilcox.test(exercise_weekly_minutes$exercise_minutes,exercise_weekly_minutes$program_week, p.adjust.method = "bonferroni")
warnings()
force_posthoc <- pairwise.wilcox.test(exercise_weekly_minutes$force_minutes, exercise_weekly_minutes$program_week, p.adjust.method = "bonferroni")

# Print the results
print(aerobic_posthoc)
print(force_posthoc)

# ----------------------------------------------------------------------

#CLUSTERING

# Create the dataset
# Mean of aerobic exercise minutes per patient
aerobic <- aggregate(exercise_weekly_minutes$exercise_minutes ~ exercise_weekly_minutes$user_uuid, 
                                      data = exercise_weekly_minutes, 
                                      FUN = function(x) c(Mean = mean(x)))

names(aerobic) <- c("Patient", "Aerobic_Mean")

#Mean and SD of strength exercise minutes per patient
strength <- aggregate(exercise_weekly_minutes$force_minutes ~ exercise_weekly_minutes$user_uuid, 
                     data = exercise_weekly_minutes, 
                     FUN = function(x) c(Mean = mean(x)))

names(strength) <- c("Patient", "Strength_Mean")

exercise_means_patient <- merge(aerobic, strength, by = "Patient")
exercise_means_patient <- exercise_means_patient[, -which(names(exercise_means_patient) == "Patient")]

exercise_means_patient

# K-means clustering combined with PCA

pca1 <- prcomp(exercise_means_patient, scale = TRUE)
summary(pca1)

fviz_pca_biplot(pca1,              # Visualize PCA via biplot
                        col.var = "black",
                        col.ind = "steelblue",
                        alpha.var = 0.6,
                        repel = TRUE,
                        labelsize = 2) + 
  labs(x = "PC1 (66.6%)",
       y = "PC2 (33.3%)")

pca_scores1 <- data.frame(pca1$x[, 1:2])
pca_scores1

#Determine number of clusters
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Perform cluster

set.seed(123)

kmeans_clust1 <- kmeans(pca_scores1, centers = 2)
kmeans_clust1

fviz_pca_ind(pca1,                 # Visualize clusters
                     habillage = kmeans_clust1$cluster,
                     repel = TRUE,
                     addEllipses = TRUE,
                     ellipse.type = "convex",
                     labelsize = 2) +
  guides(color = guide_legend(override.aes = list(label = ""))) +
  labs(x = "PC1 (66.6%)", y = "PC2 (33.3%)")

fviz_pca_biplot(pca1,              # Visualize clusters in biplot
                        col.var = "black",
                        alpha.var = 0.6,
                        label = "all",
                        habillage = kmeans_clust1$cluster,
                        repel = TRUE,
                        addEllipses = TRUE,
                        ellipse.type = "convex",
                        labelsize = 2) +
  guides(color = guide_legend(override.aes = list(label = ""))) +
  labs(x ="PC1 (66.6%)", y = "PC2 (33.3%)")

# -------------------------------------------------------------------

# Add cluster classification in the original dataset
exercise_means_patient$Cluster <- kmeans_clust1$cluster

# Cluster summary
cluster_summary <- aggregate(. ~ Cluster, data = exercise_means_patient, mean)
cluster_summary

# Turn cluster clasification into factor
exercise_means_patient$Cluster <- as.factor(kmeans_clust1$cluster)


#  ------------------------------------------------------------------------

# Mann Whitney test to compare medians between clusters
wilcox_aerobic <- wilcox.test(Aerobic_Mean ~ Cluster, data = exercise_means_patient)
wilcox_strength <- wilcox.test(Strength_Mean ~ Cluster, data = exercise_means_patient)

# Visualize results
print(wilcox_aerobic)
print(wilcox_strength)

# Median and sd
summary_stats_aerobic <- aggregate(cbind(Aerobic_Mean, Strength_Mean) ~ Cluster, 
                                   data = exercise_means_patient,
                                   FUN = function(x) c(Mean = mean(x), Median = median(x), SD = sd(x)))

print(summary_stats_aerobic)

# --------------------------------------------------------------------------

# Differences by patients
exercise_weekly_minutes$user_uuid <- as.factor(exercise_weekly_minutes$user_uuid)

#Aerobic ex
ggplot(exercise_weekly_minutes, aes(x = user_uuid, y = exercise_minutes, color = user_uuid)) +
  geom_boxplot() +
  theme_bw()

#Normality
model_lm3 <- lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)

res_df3 <- data.frame(Residuos = residuals(model_lm3))

# Histogram
ggplot(res_df3, aes(x = Residuos)) +
  geom_histogram(binwidth = 5, fill = "blue4", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram", x = "Residual", y = "Freq") +
  theme_minimal()

# Q-Q plot of residuals
qqnorm(residuals(lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)))
qqline(residuals(lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)), col = 2)

# Shapiro-Wilk
shapiro.test(residuals(lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)))
#No normality

# Homogeneity of variances
plot(lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes), 1) 

# Levene test
leveneTest(residuals(lm(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)) ~ exercise_weekly_minutes$program_week)
# Homogeneity.



# --------------------------------------------------------------------------------


#Non parametric test Kruskall wallis

aerobic_kruskal3 <- kruskal.test(exercise_minutes ~ user_uuid, data = exercise_weekly_minutes)
aerobic_kruskal3

#Force min
ggplot(exercise_weekly_minutes, aes(x = user_uuid, y = force_minutes, color = user_uuid)) +
  geom_boxplot() +
  theme_bw()

#Normality
model_lm4 <- lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes)

res_df4 <- data.frame(Residuos = residuals(model_lm4))

# Histogram
ggplot(res_df4, aes(x = Residuos)) +
  geom_histogram(binwidth = 5, fill = "blue4", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram", x = "Residual", y = "Freq") +
  theme_minimal()

# Q-Q plot of residuals
qqnorm(residuals(lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes)))
qqline(residuals(lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes)), col = 2)

# Shapiro-Wilk
shapiro.test(residuals(lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes)))
#No normality

# Homogeneity of variances
plot(lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes), 1) 

# Levene test
library(car)
leveneTest(residuals(lm(force_minutes ~ user_uuid, data = exercise_weekly_minutes)) ~ exercise_weekly_minutes$program_week)
# There's Homogeneity.

force_kruskal2 <- kruskal.test(force_minutes ~ user_uuid, data = exercise_weekly_minutes)
force_kruskal2

# Post-hoc pairwise wilcox test
class(exercise_weekly_minutes$exercise_minutes)
class(exercise_weekly_minutes$`program_week$program_week`)

aerobic_posthoc <- pairwise.wilcox.test(exercise_weekly_minutes$exercise_minutes,exercise_weekly_minutes$program_week, p.adjust.method = "bonferroni")
warnings()
force_posthoc <- pairwise.wilcox.test(exercise_weekly_minutes$force_minutes, exercise_weekly_minutes$program_week, p.adjust.method = "bonferroni")

# Print the results
print(aerobic_posthoc)
print(force_posthoc)