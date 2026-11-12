#####################
#####################
# Anonymous
# Fall 2026
##################### 
# Run Response Time Models
#
# Input: Data, with trials/subjects removed (see preprocessing)
#   - output/df_2suff-clean.csv
#   - output/df_3suff-clean.csv

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

# Drop incorrect responses
df_TwoSuff <- df_TwoSuff %>%
  filter(response == "j")

df_ThreeSuff<- df_ThreeSuff %>%
  filter(response == "j")

# Factor variables
df_TwoSuff$workerid            <- factor(df_TwoSuff$workerid)
df_TwoSuff$word                <- factor(df_TwoSuff$word)
df_TwoSuff$exp                 <- factor(df_TwoSuff$exp)
contrasts(df_TwoSuff$exp)      <- c(0.5, -0.5)
df_TwoSuff$trial_num           <- as.numeric(scale(df_TwoSuff$trial_num))
df_TwoSuff$RE                  <- scale(df_TwoSuff$RE)
df_TwoSuff$stem_wholeSuffix_TP <- scale(df_TwoSuff$stem_wholeSuffix_TP)

df_ThreeSuff$workerid            <- factor(df_ThreeSuff$workerid)
df_ThreeSuff$word                <- factor(df_ThreeSuff$word)
df_ThreeSuff$exp                 <- factor(df_ThreeSuff$exp)
contrasts(df_ThreeSuff$exp)      <- c(0.5, -0.5)
df_ThreeSuff$trial_num           <- as.numeric(scale(df_ThreeSuff$trial_num))
df_ThreeSuff$RE                  <- scale(df_ThreeSuff$RE)
df_ThreeSuff$stem_wholeSuffix_TP <- scale(df_ThreeSuff$stem_wholeSuffix_TP)


# Load PCA of variables
TwoSuff_control_pca <- readRDS("../output/PCA/2suff_controlModel_controlVar_df.rds")
ThreeSuff_control_pca <- readRDS("../output/PCA/3suff_controlModel_controlVar_df.rds")

TwoSuff_TP_pca <- readRDS("../output/PCA/2suff_MorphFullModel_TPVar_df.rds")
ThreeSuff_TP_pca <- readRDS("../output/PCA/3suff_MorphFullModel_TPVar_df.rds")

# Set GLMM response family
glmm_family <- Gamma(link = "log")


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
  dplyr::select("word", "workerid", "rt", "trial_num", "PC1", "PC2", "PC3", "exp")

# Run Model
TwoSuff_control = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 
                                + (1|workerid) + (1|word),
                          data = TwoSuff_control_var,
                          family = glmm_family)

summary(TwoSuff_control)
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  7.101658   0.018246   389.2   <2e-16 ***
#   exp1        -0.415610   0.036075   -11.5   <2e-16 ***
#   trial_num   -0.038577   0.002014   -19.2   <2e-16 ***
#   PC1          0.039265   0.003087    12.7   <2e-16 ***
#   PC2          0.009966   0.003403     2.9   0.0034 ** 
#   PC3         -0.005058   0.003935    -1.3   0.1986     

TwoSuff_control_AIC <- AIC(TwoSuff_control)
TwoSuff_control_AIC
# 321,121.4

########
# Paradigmatic Model
########

# Merge control variables with data
TwoSuff_Paradigm_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "rt", "trial_num", "exp", "PC1", "PC2", "PC3", "RE")

TwoSuff_Paradigm = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 + RE 
                                  + (1|workerid) + (1|word),
                           data = TwoSuff_Paradigm_var,
                           family = glmm_family)

summary(TwoSuff_Paradigm)
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  7.101529   0.018243   389.3  < 2e-16 ***
#   exp1        -0.415637   0.036074   -11.5  < 2e-16 ***
#   trial_num   -0.038591   0.002014   -19.2  < 2e-16 ***
#   PC1          0.039966   0.003087    12.9  < 2e-16 ***
#   PC2          0.009776   0.003387     2.9  0.00389 ** 
#   PC3         -0.002816   0.004031    -0.7  0.48484    
#   RE           0.008182   0.003509     2.3  0.01972 *  
 
TwoSuff_Paradigm_AIC <- AIC(TwoSuff_Paradigm)
TwoSuff_Paradigm_AIC
# 321,118

########
# Morphemic Simple
########

TwoSuff_MorphSimple_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "rt", "PC1", "PC2", "PC3", "stem_wholeSuffix_TP", "exp", "trial_num")

TwoSuff_MorphSimple = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 + stem_wholeSuffix_TP 
                                    + (1|workerid) + (1|word),
                              data = TwoSuff_MorphSimple_var,
                              family = glmm_family)

summary(TwoSuff_MorphSimple)
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          7.104311   0.018248   389.3  < 2e-16 ***
#   exp1                -0.415604   0.036077   -11.5  < 2e-16 ***
#   trial_num           -0.038590   0.002014   -19.2  < 2e-16 ***
#   PC1                  0.077038   0.009518     8.1 5.78e-16 ***
#   PC2                  0.019903   0.004105     4.8 1.24e-06 ***
#   PC3                 -0.037831   0.008734    -4.3 1.48e-05 ***
#   stem_wholeSuffix_TP  0.052256   0.012478     4.2 2.82e-05 ***

TwoSuff_MorphSimple_AIC <- AIC(TwoSuff_MorphSimple)
TwoSuff_MorphSimple_AIC
# 321,106.1

########
# Morphemic Full
########

TwoSuff_MorphFull_var <- left_join(df_TwoSuff, TwoSuff_control_pca, by = c("word")) %>%
                         left_join(TwoSuff_TP_pca, by = c("word")) %>%
                         dplyr::select("word", "workerid", "rt", "PC1", "PC2", "PC3", 
                                      "PC1_TP", "PC2_TP", "exp", "trial_num") %>%
                         rename("PC1_control" = "PC1",
                               "PC2_control" = "PC2",
                               "PC3_control" = "PC3")

TwoSuff_MorphFull = glmmTMB(rt ~ exp + trial_num + PC1_control + PC2_control + PC3_control
                                 + PC1_TP + PC2_TP + (1|workerid) + (1|word), 
                            data = TwoSuff_MorphFull_var,
                            family = glmm_family)

summary(TwoSuff_MorphFull)
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  7.101836   0.018240   389.4  < 2e-16 ***
#   exp1        -0.415603   0.036082   -11.5  < 2e-16 ***
#   trial_num   -0.038585   0.002014   -19.2  < 2e-16 ***
#   PC1_control  0.050624   0.004109    12.3  < 2e-16 ***
#   PC2_control  0.010447   0.003443     3.0 0.002407 ** 
#   PC3_control -0.010199   0.004096    -2.5 0.012779 *  
#   PC1_TP      -0.008855   0.003019    -2.9 0.003356 ** 
#   PC2_TP       0.022488   0.006334     3.6 0.000385 ***

TwoSuff_MorphFull_AIC <- AIC(TwoSuff_MorphFull)
TwoSuff_MorphFull_AIC
# 321,108.3

##################
##################
## 3 Suffixed Data
##################
##################

########
# Control
########

# Merge control variables with data
ThreeSuff_control_var <- left_join(df_ThreeSuff, ThreeSuff_control_pca, by = "word") %>%
  dplyr::select("word", "workerid", "rt", "trial_num", "PC1", "PC2", "PC3", "exp")

# Run Model
ThreeSuff_control = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 
                                  + (1|workerid) + (1|word),
                       data = ThreeSuff_control_var,
                       family = glmm_family)

summary(ThreeSuff_control)
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  7.162284   0.018745   382.1   <2e-16 ***
#   exp1        -0.380638   0.036278   -10.5   <2e-16 ***
#   trial_num   -0.038608   0.003145   -12.3   <2e-16 ***
#   PC1          0.046265   0.004955     9.3   <2e-16 ***
#   PC2         -0.012011   0.005744    -2.1   0.0365 *  
#   PC3          0.003368   0.006789     0.5   0.6199    

ThreeSuff_control_AIC <- AIC(ThreeSuff_control)
ThreeSuff_control_AIC
# 142,673.8

########
# Paradigmatic Model
########

# Merge control variables with data
ThreeSuff_Paradigm_var <- left_join(df_ThreeSuff, ThreeSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "rt", "trial_num", "exp", "PC1", "PC2", "PC3", "RE")

ThreeSuff_Paradigm = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 + RE 
                                  + (1|workerid) + (1|word),
                        data = ThreeSuff_Paradigm_var,
                        family = glmm_family)

summary(ThreeSuff_Paradigm)
#              Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  7.162401   0.018734   382.3   <2e-16 ***
#   exp1        -0.380594   0.036278   -10.5   <2e-16 ***
#   trial_num   -0.038612   0.003145   -12.3   <2e-16 ***
#   PC1          0.045929   0.004927     9.3   <2e-16 ***
#   PC2         -0.011363   0.005722    -2.0   0.0471 *  
#   PC3          0.005549   0.006906     0.8   0.4217    
#   RE          -0.008540   0.005766    -1.5   0.1385    
ThreeSuff_Paradigm_AIC <- AIC(ThreeSuff_Paradigm)
ThreeSuff_Paradigm_AIC
# 142,673.6

########
# Morphemic Simple
########

ThreeSuff_MorphSimple_var <- left_join(df_ThreeSuff, ThreeSuff_control_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "rt", "PC1", "PC2", "PC3", "stem_wholeSuffix_TP", "exp", "trial_num")

ThreeSuff_MorphSimple = glmmTMB(rt ~ exp + trial_num + PC1 + PC2 + PC3 + stem_wholeSuffix_TP 
                                      + (1|workerid) + (1|word),
                           data = ThreeSuff_MorphSimple_var,
                           family = glmm_family)

summary(ThreeSuff_MorphSimple)
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          7.167047   0.018784   381.6  < 2e-16 ***
#   exp1                -0.380592   0.036272   -10.5  < 2e-16 ***
#   trial_num           -0.038622   0.003145   -12.3  < 2e-16 ***
#   PC1                  0.106375   0.021423     5.0 6.85e-07 ***
#   PC2                 -0.034854   0.009745    -3.6 0.000348 ***
#   PC3                  0.057370   0.019913     2.9 0.003964 ** 
#   stem_wholeSuffix_TP  0.085630   0.029718     2.9 0.003960 ** 

ThreeSuff_MorphSimple_AIC <- AIC(ThreeSuff_MorphSimple)
ThreeSuff_MorphSimple_AIC
# 142,667.7

########
# Morphemic Full
########

ThreeSuff_MorphFull_var <- left_join(df_ThreeSuff, ThreeSuff_control_pca, by = c("word")) %>%
  left_join(ThreeSuff_TP_pca, by = c("word")) %>%
  dplyr::select("word", "workerid", "rt", "PC1", "PC2", "PC3", 
                "PC1_TP", "PC2_TP", "PC3_TP", "exp", "trial_num") %>%
  rename("PC1_control" = "PC1",
         "PC2_control" = "PC2",
         "PC3_control" = "PC3")

ThreeSuff_MorphFull = glmmTMB(rt ~ exp + trial_num + PC1_control + PC2_control + PC3_control
                                    + PC1_TP + PC2_TP + PC3_TP + (1|workerid) + (1|word), 
                         data = ThreeSuff_MorphFull_var,
                         family = glmm_family)

summary(ThreeSuff_MorphFull)
#             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  7.162396   0.018632   384.4  < 2e-16 ***
#   exp1        -0.380585   0.036284   -10.5  < 2e-16 ***
#   trial_num   -0.038652   0.003144   -12.3  < 2e-16 ***
#   PC1_control  0.064329   0.006094    10.6  < 2e-16 ***
#   PC2_control -0.008003   0.006059    -1.3  0.18654    
#   PC3_control  0.002748   0.006424     0.4  0.66876    
#   PC1_TP       0.002574   0.005463     0.5  0.63752    
#   PC2_TP       0.033272   0.006994     4.8 1.97e-06 ***
#   PC3_TP       0.015982   0.005937     2.7  0.00711 **

ThreeSuff_MorphFull_AIC <- AIC(ThreeSuff_MorphFull)
ThreeSuff_MorphFull_AIC
# 142,649

########
# Plotting
########

TwoSuff_plot = generate_AIC_comparison_plot_val(
  c(TwoSuff_control_AIC, TwoSuff_Paradigm_AIC, TwoSuff_MorphSimple_AIC, TwoSuff_MorphFull_AIC),
  c("Baseline", "Global Paradigmatic",  "Morphemic Simple", "Morphemic Full"),
  "2 Suffixed Data"
)
TwoSuff_plot
ggsave(filename = "../figures/TwoSuff_model_comparison.png", plot = TwoSuff_plot, width = 8, height = 6, dpi = 300)

ThreeSuff_plot = generate_AIC_comparison_plot_val(
  c(ThreeSuff_control_AIC, ThreeSuff_Paradigm_AIC, ThreeSuff_MorphSimple_AIC, ThreeSuff_MorphFull_AIC),
  c("Baseline", "Global Paradigmatic",  "Morphemic Simple", "Morphemic Full"),
  "3 Suffixed Data"
)

ThreeSuff_plot
ggsave(filename = "../figures/ThreeSuff_model_comparison.png", plot = ThreeSuff_plot, width = 8, height = 6, dpi = 300)
