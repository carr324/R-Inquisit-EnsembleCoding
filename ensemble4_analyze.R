require(lmerTest)
require(dplyr)
require(tidyr)

rawData <- read.csv("ensemble4_data_clean.csv")

# Use subjects in the condition where both race and gender were
# mixed to create the trust scores ...

# Subjects that didn't finish the experiment are tracked in the 
# "finished" column (0 = didn't finish all ratings; 1 = finished all ratings):

# N = 411 subjects that finished the study
n_sbj = length(unique(rawData$subject[rawData$finished==1]))

# N = 101 subjects in mixAll condition (that finished the study)
cleanData_mixAll <- rawData %>% 
  filter(finished == 1, blockcode == "ratingBlock_mixAll") %>%
  select(subject, condition, response, trialcode, stimulusitem4)

mixAllSummary <- group_by(cleanData_mixAll, stimulusitem4) %>%
  summarize(
    mean_trust = mean(response, na.rm = TRUE)
  )

mixAllSummary$zscore_trust <- scale(mixAllSummary$mean_trust, center = TRUE, scale = TRUE)

write.csv(mixAllSummary, "ensemble4_trustScores.csv", row.names = FALSE)