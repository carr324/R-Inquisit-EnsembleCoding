require(lmerTest)
require(dplyr)
require(tidyr)
require(ggplot2)
require(extrafont)

rawData <- read.csv("ensemble5_data_clean.csv")
rawDemog <- read.csv("ensemble5_data_demographics.csv")

# Divide data for task (grid trials) and surveys (scales):
taskData <- rawData %>% filter(blockcode == "gridcompare_block")
scaleData <- rawData %>% filter(blockcode == "imsems_block" |
                                  blockcode == "sdo7_block" |
                                  blockcode == "atb_block")
thermData <- read.csv("ensemble5_therm_survey.csv") %>% 
  select(subject, therm_slider_black_response, therm_slider_white_response)

# Get quick summary for subjects on # observations and avg. performance ...
# All subjects should have 120 valid trials:
sbjSummary <- group_by(taskData, subject) %>%
  summarize(
    count = n(),
    mean_correct = mean(correct_presp, na.rm = TRUE)
  )

# Do a quick check on subject demographics:
cleanDemog <- group_by(rawDemog, subject) %>%
  summarize(
    age = mean(age_response),
    gender = unique(gender_response)
  )

# Remove subjects who didn't finish all 120 grid trials:
exclSbj <- sbjSummary$subject[sbjSummary$count!=120]

taskData <- filter(taskData, !(subject %in% exclSbj))
scaleData <- filter(scaleData, !(subject %in% exclSbj))
thermData <- filter(thermData, !(subject %in% exclSbj))
cleanDemog <- filter(cleanDemog, !(subject %in% exclSbj))
sbjSummary <- filter(sbjSummary, !(subject %in% exclSbj))

####################
### FINAL N = 14880 observations (124 subjects * 120 valid observations):
####################

# Check final subject demographics ...
# M (SD) age = 39.13 (11.83) years; 70 females:
mean(cleanDemog$age, na.rm=T); sd(cleanDemog$age, na.rm=T)
length(cleanDemog$gender[cleanDemog$gender=="Female"])

# Get summary dataframe by trial type (white, black, mixed):
trialSummary <- group_by(taskData, subject, trialcode) %>%
  summarize(
    count = n(),
    mean_correct = mean(correct_presp, na.rm = TRUE)
  )

taskData$subject <- as.factor(taskData$subject)
taskData$grid_sd <- apply(taskData[,14:17], 1, sd)

# Overall performance relative to chance? (very significant):
t.test(sbjSummary$mean_correct, mu = 0.5)

# By trial type? (significantly above chance for all types; doesn't
# appear to be much difference in strength of the effects by trial type):
t.test(trialSummary$mean_correct[trialSummary$trialcode=="gridcompare_trial_black"], mu = 0.5)
t.test(trialSummary$mean_correct[trialSummary$trialcode=="gridcompare_trial_white"], mu = 0.5)
t.test(trialSummary$mean_correct[trialSummary$trialcode=="gridcompare_trial_mixed"], mu = 0.5)

# Trial type doesn't seem to matter:
glmer1 <- glmer(correct_presp ~ trialcode + (1|subject), 
                family = "binomial",
                data = taskData)
summary(glmer1)

# Easiness column was calculated as absolute value of the difference 
# between grid faces vs. compare face (higher numbers = easier) ...
# Subjects are more accurate on easier trials, but no other effects:
glmer2 <- glmer(correct_presp ~ trialcode * easiness + (1|subject), 
                family = "binomial",
                data = taskData)
summary(glmer2)

# Does grid_sd make a difference? (only a marginal effect): 
glmer3 <- glmer(correct_presp ~ trialcode * grid_sd + (1|subject), 
                          family = "binomial",
                          data = taskData)
summary(glmer3)

# Incorporate scale data & compare to ensemble performance:
taskData <- plyr::join(taskData, thermData)

trialSummary <- group_by(taskData, subject, trialcode) %>%
  summarize(
    count = n(),
    mean_correct = mean(correct_presp, na.rm = TRUE),
    therm_black = mean(therm_slider_black_response),
    therm_white = mean(therm_slider_white_response)
  )

# Do people's responses on the Black/White feeling thermometers affect
# their performances on White/Black/Mixed trials?

# Yes for lmer1 = significant interaction:
lmer1 <- lmer(mean_correct ~ trialcode * scale(therm_black) + (1|subject), 
              data = trialSummary)
anova(lmer1)

# No for lmer2 (nothing significant):
lmer2 <- lmer(mean_correct ~ trialcode * scale(therm_white) + (1|subject), 
              data = trialSummary)
anova(lmer2)

# Read in reference .csv's to score other scales:
atb_scoring <- read.csv("atb_scoring.csv")[,c(1,2,4)]
names(atb_scoring)[1] <- "trialcode"

imsems_scoring <- read.csv("imsems_scoring.csv")[,c(1,2,4)]
names(imsems_scoring)[1] <- "trialcode"

sdo7_scoring <- read.csv("sdo7_scoring.csv")[,c(1,2,6)]
names(sdo7_scoring)[1] <- "trialcode"

# Combine scale scoring df's to merge with data df:
scale_scoring_df <- rbind(atb_scoring, imsems_scoring, sdo7_scoring)

scaleData <- plyr::join(scaleData, scale_scoring_df) %>% 
  arrange(subject, trialcode)

# Correct for reverse-coding in scales:
scaleData$response_code <- 999
scaleData$response_code[scaleData$reverse_code==0] <- scaleData$response[scaleData$reverse_code==0]

scaleData$response_code[scaleData$scale_name=="atb"&scaleData$reverse_code==1] <- 8 - scaleData$response[scaleData$scale_name=="atb"&scaleData$reverse_code==1]
scaleData$response_code[scaleData$scale_name=="ems"&scaleData$reverse_code==1] <- 10 - scaleData$response[scaleData$scale_name=="ems"&scaleData$reverse_code==1]
scaleData$response_code[scaleData$scale_name=="ims"&scaleData$reverse_code==1] <- 10 - scaleData$response[scaleData$scale_name=="ims"&scaleData$reverse_code==1]
scaleData$response_code[scaleData$scale_name=="sdo"&scaleData$reverse_code==1] <- 8 - scaleData$response[scaleData$scale_name=="sdo"&scaleData$reverse_code==1]

# Create summary dataframe for each subject, by scale:
scaleSummary <- group_by(scaleData, subject, scale_name) %>% 
  summarize(mean_score = mean(response_code)) %>%
  spread(scale_name, mean_score)

scaleSummary$subject <- as.factor(scaleSummary$subject)

# Join to trialSummary dataframe from before with the thermometer scores:
finalSummary <- merge(trialSummary, scaleSummary) %>% arrange(subject)

# Look at how the scales affect ensemble performance by trial type ...

# ATB: Yes, main effect of ATB & interaction
# IMS: Yes, main effect of IMS & interaction
# EMS: Only main effect of EMS
# SDO: Yes, main effect of SDO & interaction
lmer3 <- lmer(mean_correct ~ trialcode * scale(atb) + (1|subject), 
              data = finalSummary)
anova(lmer3)

lmer4 <- lmer(mean_correct ~ trialcode * scale(ims) + (1|subject), 
              data = finalSummary)
anova(lmer4)

lmer5 <- lmer(mean_correct ~ trialcode * scale(ems) + (1|subject), 
              data = finalSummary)
anova(lmer5)

lmer6 <- lmer(mean_correct ~ trialcode * scale(sdo) + (1|subject), 
              data = finalSummary)
anova(lmer6)

# Collapse trialcode to just "single-race" vs. "mixed-race" and rerun:
finalSummary$trialcode_new <- "Mixed-Race Trial"
finalSummary$trialcode_new[finalSummary$trialcode!="gridcompare_trial_mixed"] <- "Single-Race Trial"
finalSummary$trialcode_new <- as.factor(finalSummary$trialcode_new)

# All important effects replicate:
lmer7 <- lmer(mean_correct ~ trialcode_new * scale(atb) + (1|subject),
              data = finalSummary)
anova(lmer7)

lmer8 <- lmer(mean_correct ~ trialcode_new * scale(ims) + (1|subject),
              data = finalSummary)
anova(lmer8)

lmer9 <- lmer(mean_correct ~ trialcode_new * scale(ems) + (1|subject),
              data = finalSummary)
anova(lmer9)

lmer10 <- lmer(mean_correct ~ trialcode_new * scale(sdo) + (1|subject),
               data = finalSummary)
anova(lmer10)

# PLOTS:
ggplot(data = finalSummary, 
       aes(x = atb, y = mean_correct, color = trialcode, fill = trialcode)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nAttitudes Towards Blacks (ATB) Scale\n(higher numbers = more positive attitudes)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = atb, y = mean_correct, color = trialcode_new, fill = trialcode_new)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nAttitudes Towards Blacks (ATB) Scale\n(higher numbers = more positive attitudes)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = ims, y = mean_correct, color = trialcode, fill = trialcode)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nInternal Motivation to Respond Without Prejudice\n(higher numbers = more internal motivation)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = ims, y = mean_correct, color = trialcode_new, fill = trialcode_new)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nInternal Motivation to Respond Without Prejudice\n(higher numbers = more internal motivation)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = ems, y = mean_correct, color = trialcode, fill = trialcode)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nExternal Motivation to Respond Without Prejudice\n(higher numbers = more external motivation)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = ems, y = mean_correct, color = trialcode_new, fill = trialcode_new)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nExternal Motivation to Respond Without Prejudice\n(higher numbers = more external motivation)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = sdo, y = mean_correct, color = trialcode, fill = trialcode)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nSDO Score\n(higher numbers = more preference for hierarchy)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = sdo, y = mean_correct, color = trialcode_new, fill = trialcode_new)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nSDO Score\n(higher numbers = more preference for hierarchy)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = therm_black, y = mean_correct, color = trialcode, fill = trialcode)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nFeeling Thermometer for Blacks\n(higher numbers = warmer feelings)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data = finalSummary, 
       aes(x = therm_black, y = mean_correct, color = trialcode_new, fill = trialcode_new)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  stat_smooth(method = "lm", size = 1, alpha = 0.35) + 
  theme_classic() +
  xlab("\nFeeling Thermometer for Blacks\n(higher numbers = warmer feelings)") +
  ylab("Mean Proportion of Ensemble Trials Correct\n") +
  theme_classic() + 
  theme(text = element_text(family="Gill Sans MT"), 
        axis.text.x  = element_text(size=12), 
        axis.text.y  = element_text(size=12), 
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.title = element_blank(),
        legend.position = "top")













