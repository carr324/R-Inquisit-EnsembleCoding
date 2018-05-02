require(lmerTest)
require(dplyr)
require(tidyr)

rawData <- read.csv("ensemble2_data_clean.csv")
rawDemog <- read.csv("ensemble2_data_demographics.csv")

# Filter data to main results during valid face trials:
cleanData <- rawData %>% 
  filter(blockcode == "gridcompare_block",
         values.trial_retry == 0,
         !is.na(correct_var_num)) %>%
  select(-trialcode)

# Get quick summary for subjects on # observations and 
# performance when looking at mean vs. variance of faces ...
# All subjects should have 100 valid trials:
sbjSummary <- group_by(cleanData, subject) %>%
  summarize(
    count = n(),
    mean_correct_var = mean(correct_var_num, na.rm = TRUE),
    mean_correct_avg = mean(correct_avg_num, na.rm = TRUE)
  )

# Do a quick check on subject demographics
cleanDemog <- group_by(rawDemog, subject) %>%
  summarize(
    age = mean(age_response),
    gender = unique(gender_response)
  )

# Remove subjects who didn't finish the study from data/demog files:
exclSbj <- sbjSummary$subject[sbjSummary$count!=100]
cleanData <- filter(cleanData, !(subject) %in% exclSbj)
cleanDemog <- filter(cleanDemog, !(subject) %in% exclSbj)
sbjSummary <- filter(sbjSummary, !(subject) %in% exclSbj)

####################
### FINAL N = 5000 observations (50 subjects * 100 valid observations):
####################

# Check final subject demographics ...
# M (SD) age = 39.48 (11.51) years; 23 females:
mean(cleanDemog$age); sd(cleanDemog$age)
length(cleanDemog$gender[cleanDemog$gender=="Female"])

cleanData$subject <- as.factor(cleanData$subject)

# Subjects NOT significantly better than chance at variance; 
# BUT THEY ARE significantly above chance when coding their responses
# according to the MEANS of trust from the faces:
t.test(sbjSummary$mean_correct_var, mu = 0.5)
t.test(sbjSummary$mean_correct_avg, mu = 0.5)

# correct_var_num = 0 (incorrect) vs. 1 (correct), according to differences
#    in VARIANCES between face grids
# correct_avg_num = 0 (incorrect) vs. 1 (correct), according to differences
#    in MEANS between face grids

# Intercept is only trending for variance:
glmer1 <- glmer(correct_var_num ~ 1 + (1|subject), 
                family = "binomial",
                data = cleanData)
summary(glmer1)

# Intercept is very significant for mean:
glmer2 <- glmer(correct_avg_num ~ 1 + (1|subject), 
                family = "binomial",
                data = cleanData)
summary(glmer2)

# It seems like subjects are just responding to MEAN differences
# in the face grids, even though we gave them instruction to respond
# to differences in VARIANCE ...

# Easiness columns were calculated as absolute value of the difference (either
# in var or avg) between grid 1 & grid 2 (higher numbers = easier):
glmer3 <- glmer(correct_var_num ~ easiness_var * easiness_avg + (1|subject), 
                family = "binomial",
                data = cleanData)
summary(glmer3)

# Easiness doesn't seem to matter as much in this study ...

glmer4 <- glmer(correct_avg_num ~ easiness_var * easiness_avg + (1|subject), 
                family = "binomial",
                data = cleanData)
summary(glmer4)