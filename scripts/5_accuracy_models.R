#####################
#####################
# Anonymous
# Fall 2026
##################### 
# Run Accuracy models
##################### 

source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "setup_r_libs.R"))

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lmerTest)

# Import custom plotting function
source("plotting_functions.R")

# Load data
df_TwoSuff <- read.csv("../output/df_2suff-clean.csv")
df_ThreeSuff <- read.csv("../output/df_3suff-clean.csv")

# Factor variables
df_TwoSuff$workerid <- factor(df_TwoSuff$workerid)
df_TwoSuff$word     <- factor(df_TwoSuff$word)
df_TwoSuff$exp      <- factor(df_TwoSuff$exp)
contrasts(df_TwoSuff$exp) <- c(0.5, -0.5)
df_TwoSuff$trial_num     <- as.numeric(scale(df_TwoSuff$trial_num))

df_ThreeSuff$workerid <- factor(df_ThreeSuff$workerid)
df_ThreeSuff$word     <- factor(df_ThreeSuff$word)
df_ThreeSuff$exp      <- factor(df_ThreeSuff$exp)
contrasts(df_ThreeSuff$exp) <- c(0.5, -0.5)
df_ThreeSuff$trial_num     <- as.numeric(scale(df_ThreeSuff$trial_num))

# Add correct answer column
df_TwoSuff <- df_TwoSuff %>%
  mutate(correct = as.numeric(response != "f"))

df_ThreeSuff <- df_ThreeSuff %>%
  mutate(correct = as.numeric(response != "f"))

# Load PCA of variables
TwoSuff_control_pca <- readRDS("../output/PCA/2suff_controlModel_controlVar_df.rds")
ThreeSuff_control_pca <- readRDS("../output/PCA/3suff_controlModel_controlVar_df.rds")

TwoSuff_TP_pca <- readRDS("../output/PCA/2suff_MorphFullModel_TPVar_df.rds")
ThreeSuff_TP_pca <- readRDS("../output/PCA/3suff_MorphFullModel_TPVar_df.rds")

##################
##################
## 2 Suffixed Data
##################
##################

########
# Control
########

# Merge control variables with data
TwoSuff_control_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = "word") %>%
  dplyr::select("word", "workerid", "correct", "trial_num", "PC1", "PC2", "PC3", "exp")

# Run Model
TwoSuff_control = glmer(correct ~ exp + trial_num + PC1 + PC2 + PC3 
                          + (1|workerid) + (1|word),
                          data = TwoSuff_control_var,
                          family = binomial)

summary(TwoSuff_control)
#            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.18604    0.16425  25.486  < 2e-16 ***
#   exp1         0.01259    0.27414   0.046   0.9634    
# trial_num   -0.06287    0.03423  -1.837   0.0662 .  
# PC1         -0.48287    0.07720  -6.255 3.99e-10 ***
#   PC2         -0.66664    0.08324  -8.009 1.16e-15 ***
#   PC3          0.78398    0.09886   7.930 2.19e-15 ***

TwoSuff_control_AIC <- AIC(TwoSuff_control)
TwoSuff_control_AIC
# 7326.243


########
# Paradigmatic Model
########

# Merge control variables with data
TwoSuff_Paradigm_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "correct", "trial_num", "exp", "PC1", "PC2", "PC3", "RE")

TwoSuff_Paradigm = glmer(correct ~ exp + trial_num + PC1 + PC2 + PC3 + RE 
                           + (1|workerid) + (1|word),
                           data = TwoSuff_Paradigm_var,
                           family = binomial)

summary(TwoSuff_Paradigm)
#            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.66783    0.22442  20.799  < 2e-16 ***
#   exp1         0.01264    0.27448   0.046  0.96327    
# trial_num   -0.06254    0.03423  -1.827  0.06771 .  
# PC1         -0.50121    0.07600  -6.595 4.26e-11 ***
#   PC2         -0.65398    0.08181  -7.994 1.31e-15 ***
#   PC3          0.70514    0.09980   7.065 1.60e-12 ***
#   RE          -0.88491    0.27090  -3.267  0.00109 ** 

TwoSuff_Paradigm_AIC <- AIC(TwoSuff_Paradigm)
TwoSuff_Paradigm_AIC
# 7317.814

########
# Morphemic Simple
########

TwoSuff_MorphSimple_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "correct", "PC1", "PC2", "PC3", "stem_wholeSuffix_TP", "exp", "trial_num")

TwoSuff_MorphSimple = glmer(correct ~ exp + trial_num + PC1 + PC2 + PC3 + stem_wholeSuffix_TP
                              + (1|workerid) + (1|word),
                              data = TwoSuff_MorphSimple_var,
                              family = binomial)

summary(TwoSuff_MorphSimple)
# The Model is Singular

TwoSuff_MorphSimple = glmer(correct ~ exp + trial_num + scale(PC1) + scale(PC2) + scale(PC3) + scale(stem_wholeSuffix_TP) 
                            + (1|workerid) + (1|word),
                            data = TwoSuff_MorphSimple_var,
                            family = binomial)

summary(TwoSuff_MorphSimple)
# Model failed to converge
