#####################
#####################
# Anonymous
# Fall 2026
# Spanish Verbs Project — Preprocessing
##################### 
# Input: Raw data files
#   - data/behavioral_Exp{1,2}-trials.csv
#
# Output: experimental data with participants and trails removed following
#         Baayen & Milin (2010): Participants with errror >15% removed and
#         Trials with rt < 5ms >5000ms removed. Final data is split into 2
#         and 3 suffixed datasets
#
#   - output/df_{2,3}suff-clean.csv
# 
##################### 

# Load necessary packages
library(dplyr)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########
# Import Data
#########

# Subject Data
df_trials_1 <- read.csv('../data/behavioral_Exp1-trials.csv', header = TRUE)
df_trials_2 <- read.csv('../data/behavioral_Exp2-trials.csv', header = TRUE)

df_trials_1$exp <- 1
df_trials_2$exp <- 2

df_trials <- rbind(df_trials_1, df_trials_2)

total_trials <- nrow(df_trials) 
total_trials
#total data points: 74412

totalParticipants <- length(unique(df_trials$workerid))
totalParticipants 
#total participants: 53

exp1_unique <- unique(df_trials$workerid[df_trials$exp == 1])
length(exp1_unique)
# 30 participants in Exp 1

exp2_unique <- unique(df_trials$workerid[df_trials$exp == 2])
length(exp2_unique)
# 23 participants in Exp 2

#Import target word Variables
suff2_var <- read.csv('../materials/2suff_stim_variables.csv', header = TRUE)
suff3_var <- read.csv('../materials/3suff_stim_variables.csv', header = TRUE)
df_bigrams <- read.csv('../materials/bigram_avg_freq.csv', header = TRUE)


#########
#########
# Clean Data
#########
#########

###
# Participant Exclusion
###

# Get accuracy
df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | 
                                  (response == "j" & trial_type == "target"), 1, 0)) %>%
  summarise(overall_accuracy = mean(correctTrial, na.rm = TRUE))
# Overall accuracy: 93.6%

# Overall accuracy of real word responses
df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | 
                                  (response == "j" & trial_type == "target"), 1, 0)) %>%
  filter(trial_type == "target") %>%
  summarise(overall_accuracy = mean(correctTrial, na.rm = TRUE))
# Overall accuracy: 92.7%

# Exclude participants if they had an error rate of >15%
df <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | 
                                  (response == "j" & trial_type == "target"), 1, 0)) %>%
  group_by(workerid) %>%
  filter(mean(correctTrial == 0) <= 0.151)

participants_post_errorRate = length(unique(df$workerid))
print(totalParticipants - participants_post_errorRate)
# 4 participants excluded


excluded_participants <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | 
                                  (response == "j" & trial_type == "target"), 1, 0)) %>%
  group_by(workerid) %>%
  summarise(error_rate = mean(correctTrial == 0)) %>%
  filter(error_rate > 0.151)

# Get unique values of excluded groups and their error rates
unique_excluded_groups <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | (response == "j" & trial_type == "target"), 1, 0)) %>%
  filter(workerid %in% excluded_participants$workerid) %>%
  group_by(workerid) %>%
  summarise(error_rate = mean(correctTrial == 0)) %>%
  distinct()

# Print excluded participants and their error rates
print(unique_excluded_groups)
# workerid error_rate
#  46       0.292
#  136      0.209
#  137      0.194
#  149      0.167


###
# Trial Exclusion
###

df_trials_n <- df %>% nrow()

# Exclude responses that were shorter than 5ms or longer than 5000ms
df <- df %>%
  filter(rt >= 5 & rt <= 5000)

df_trials_n2 <- df %>% nrow()

print(df_trials_n -df_trials_n2)
# 579 trials are less then 5ms or more than 5000ms

print(1- df_trials_n2/df_trials_n)
#this lead to the exclusion of <1% of the data


###
# Final Cleaning
###

df <- df %>%
  select(-error, -proliferate.condition, -correctTrial) %>%
  rename(trial_num = slide_number_in_experiment)

#Counting of trials started at 9 
df$trial_num <- df$trial_num - 8

# Note that the trial numbers will be off by "1" for every break preceding that trial 
#   and will potentially be off by any number of key presses during each break. 

#########
# Merge Variable Information
#########

df_target <- df %>%
  filter(trial_type == "target")

df_distractor <- df %>%
  filter(trial_type == "distractor")

df_bigrams <- df_bigrams %>%
  select(-X) %>%
  rename(word = target)
  
df_target <- left_join(df_target, df_bigrams, by="word")

# Add in length variable
df_target <- df_target %>% 
  mutate(word_length = nchar(word))


#########
# Add in variables based on number of suffixes
#########

df_target_2 <- df_target %>%
  filter(suffix %in% c('aba', 'ábamos', 'aban', 'abas', 
                       'ía', 'ías', 'ían', 'íamos',
                       'aré', 'arás', 'ará', 'aremos', 'arán',
                       'eré', 'erás', 'erá', 'eremos', 'erán',
                       'iré', 'irás', 'irá', 'iremos', 'irán'
                       ))

df_target_2 <- left_join(df_target_2, suff2_var, by=c("word", "lemma", "suffix")) 
nrow(df_target_2)
# 23,633 total trials for 2 suffixed dataset

write.csv(df_target_2,"../output/df_2suff-clean.csv", row.names = FALSE)


df_target_3 <- df_target %>%
  filter(suffix %in% c('aría', 'aríamos', 'arían', 'arías', 
                       'ería', 'eríamos', 'erían', 'erías', 
                       'iría', 'iríamos', 'irían', 'irías'))

df_target_3 <- left_join(df_target_3, suff3_var, by=c("word", "lemma", "suffix")) 
nrow(df_target_3)
# 10,496 total trials for 3 suffixed dataset

write.csv(df_target_3,"../output/df_3suff-clean.csv", row.names = FALSE)

