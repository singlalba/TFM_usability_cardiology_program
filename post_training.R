library(dataMaid)

# Upload data
work_dir <- getwd()
route <- file.path(work_dir, "post_training.csv")
post_training <- read.csv(route, sep = ";")

str(post_training)

colnames(post_training)

# Variable transformations to factors
post_training$Patient <- factor(post_training$Patient)
post_training$aerobic_ex <- factor(post_training$aerobic_ex)
post_training$Location <- factor(post_training$Location)
post_training$whic_aerobic_ex <- factor(post_training$whic_aerobic_ex)
post_training$other_aerobic <- factor(post_training$other_aerobic)
post_training$how_was_bike <- factor(post_training$how_was_bike)
post_training$strength_ex <- factor(post_training$strength_ex)
post_training$how_was_strength <- factor(post_training$how_was_strength)
post_training$balance_ex <- factor(post_training$balance_ex)
post_training$breathing_exercise <- factor(post_training$breathing_exercise)
post_training$stretching <- factor(post_training$stretching)
post_training$symptom <- factor(post_training$symptom)
post_training$which_symptom <- factor(post_training$which_symptom)

makeDataReport(post_entrenament)

# AEROBIC EXERCISE
summary(post_training$aerobic_ex)
prop.table(table(post_training$aerobic_ex))*100

table(post_training$how_was_bike)

locations <- na.omit(post_training$Location)
summary(post_training$Location)
prop.table(table(locations))*100

summary(post_training$borg_aerobic)

summary(post_training$whic_aerobic_ex)
prop.table(table(post_training$whic_aerobic_ex))*100

# STRENGTH EXERCISE

summary(post_training$strength_ex)

post_training$borg_strength <- as.double(post_training$borg_strength)

summary(na.omit(post_training$borg_strength))
sd(na.omit(post_training$borg_strength))
IQR(na.omit(post_training$borg_strength))

summary(post_training$how_was_strength)

#BALANCE
table(post_training$balance_ex)

#STRETCHING
table(post_training$strength_ex)

#BREATHING
table(post_training$breathing_exercise)
prop.table(table(post_training$breathing_exercise))*100
