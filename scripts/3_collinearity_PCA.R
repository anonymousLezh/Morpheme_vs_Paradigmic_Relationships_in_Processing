#####################
#####################
# Anonymous
# Fall 2026
# Spanish Verbs Project — Collinearity & PCA Script
##################### 
# The script calculates kappa value for variables of all models. If kappa >= 10
#   PCA is run over the variables and kappa is recalculated
# 
# Input: csv files with all variables
#     - materials/2suff_stim_variables.csv: 2 suffixed dataset stims + variables
#     - materials/3suff_stim_variables.csv: 3 suffixed dataset stims + variables
#     - materials/bigram_avg_freq.csv: bigram freq of all stims
#
# Output: .rds (R data objects) with saved PCAs of the variables per model. Each 
#       
#     - /output/PCA/TwoSuff_control_PCA_data.rds
# 
##################### 

# Load necessary packages
library(dplyr)
library(tidyverse)
library(zeallot) # for %<-% operator that unpacks function output
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import custom plotting function
source("plotting_functions.R")

#########
# Import Data
#########

#Import target word Variables
suff2_var <- read.csv('../materials/2suff_stim_variables.csv', header = TRUE)
suff3_var <- read.csv('../materials/3suff_stim_variables.csv', header = TRUE)
df_bigrams <- read.csv('../materials/bigram_avg_freq.csv', header = TRUE) %>%
  select(-X) %>%
  rename("word" = "target")

suff2_var <- left_join(suff2_var, df_bigrams, by="word")
suff3_var <- left_join(suff3_var, df_bigrams, by="word")

# add in length variable
suff2_var$word_length <- nchar(suff2_var$word)
suff3_var$word_length <- nchar(suff3_var$word)

suff2_var$log_bigram_freq <- log(as.numeric(suff2_var$avg_bigram_freq))
suff3_var$log_bigram_freq <- log(as.numeric(suff3_var$avg_bigram_freq))

#########
#########
# Define Functions
#########
#########

# Input: 
#   df: dataframe with variable values. Each column represents a variable and 
#       each row represents a stimulus. 
#   columns: list of columns that you want to compute collinearity over
# Output: collinearity kappa value 
check_collinearity <- function(df, columns) {
  # Select specified columns
  selected_df <- df %>%
    ungroup() %>%
    select(all_of(columns)) %>%
    unique()
  
  # Convert to matrix
  predictor_matrix <- as.matrix(selected_df)
  
  # Add intercept
  intercept_matrix <- cbind(1, predictor_matrix)
  
  # Scale to unit length
  unit_length_matrix <- apply(intercept_matrix, 2, function(col) col / sqrt(sum(col^2)))
  
  # Compute collinearity
  collinearity_check <- kappa(unit_length_matrix)
  
  # Return the result
  return(collinearity_check)
}

# Run PCA over specified variables
# Input: 
#   - dataset: dataframe with variable values. Each column represents a variable and 
#       each row represents a stimulus. 
#   - pca_variables: list of column names over which PCA will be run
# 
# Output: list with following items:
#   - pc_df: dataframe with component values for each stimulus
#   - pca_result: loadings and other components of the PCA
run_pca <- function(dataset, pca_variables) {
  
  # Subset word (stimulus) column and list of variables over which to run PCA
  pca_data_clean <- dataset %>%
    select(all_of(c("word", pca_variables))) %>%
    drop_na()
  
  # Run the PCA
  # Note that variables are centered and scaled
  pca_result <- pca_data_clean %>%
    select(-word) %>%
    prcomp(center = TRUE, scale. = TRUE)
  
  # Combine component values with the words (stimi)
  pc_df <- pca_data_clean %>%
    select(word) %>%
    bind_cols(as.data.frame(pca_result$x))
  
  # Return original and combined dfs as list
  return(list(
    pc_df,
    pca_result
  ))
  # return(list(
  #   data = pc_df,
  #   pca_object = pca_result
  # ))
}


#########
#########
# Run Collinearity on models
#########
#########

#########
# 2 suffix Control
#########

# Check kappa value for control model
TwoSuff_control <- check_collinearity(suff2_var, 
                                      c("word_log_freq", 
                                        "log_bigram_freq", 
                                        "word_length"))
TwoSuff_control
# 11.85737

# Run PCA on variables
c(TwoSuff_control_PCA_df, TwoSuff_control_PCA_results) %<-% 
  run_pca(suff2_var, c("word_log_freq", 
                       "log_bigram_freq", 
                       "word_length"))
# Check kappa on components
TwoSuff_control_PCA <- check_collinearity(TwoSuff_control_PCA_df, 
                                          c("PC1", "PC2", "PC3"))
TwoSuff_control_PCA
# 1

saveRDS(TwoSuff_control_PCA_df, file = "../output/PCA/2suff_controlModel_controlVar_df.rds")
saveRDS(TwoSuff_control_PCA_results, file = "../output/PCA/2suff_controlModel_controlVar_results.rds")

TwoSuff_control_PCA_results
#                   PC1        PC2        PC3
# word_log_freq   -0.71038918 -0.2078030  0.6724322
# log_bigram_freq -0.02619655 -0.9469495 -0.3203129
# word_length      0.70332138 -0.2451622  0.6672590
#########
# 3 suffix Control
#########

# Check kappa value for control model
ThreeSuff_control <- check_collinearity(suff3_var, 
                                        c("word_log_freq", 
                                          "log_bigram_freq", 
                                          "word_length"))
ThreeSuff_control
# 15.05504

# Run PCA on variables
c(ThreeSuff_control_PCA_df, ThreeSuff_control_PCA_results) %<-% 
  run_pca(suff3_var, c("word_log_freq", 
                       "log_bigram_freq", 
                       "word_length"))

# Check kappa of components
ThreeSuff_control_PCA <- check_collinearity(ThreeSuff_control_PCA_df, 
                                            c("PC1", "PC2", "PC3"))
ThreeSuff_control_PCA
# 1

saveRDS(ThreeSuff_control_PCA_df, file = "../output/PCA/3suff_controlModel_controlVar_df.rds")
saveRDS(ThreeSuff_control_PCA_results, file = "../output/PCA/3suff_controlModel_controlVar_results.rds")

ThreeSuff_control_PCA_results
#                   PC1          PC2        PC3
# word_log_freq   -0.6808666  0.268358283 -0.6814723
# log_bigram_freq  0.1922315  0.963312871  0.1872842
# word_length      0.7067303 -0.003484864 -0.7074745


#########
# 2 suffix Paradigmatic Model 
#########

# Check Kappa
TwoSuff_Paradigm <- check_collinearity(suff2_var, 
                                      c("word_log_freq", 
                                        "log_bigram_freq", 
                                        "word_length", 
                                        "RE"))
TwoSuff_Paradigm
# 12.75


TwoSuff_Paradigm_PCA_df <- left_join(TwoSuff_control_PCA_df, suff2_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "RE")

# Check kappa of PC and RE
TwoSuff_Paradigm_PCA <- check_collinearity(TwoSuff_Paradigm_PCA_df, 
                                          c("PC1", "PC2", "PC3", "RE"))
TwoSuff_Paradigm_PCA
# 4.15828
# Kappa is low enough that we can keep RE out of PCA and get interpretable results

# PCA run over control variables seperately, so they are identical to control model
# note: Scale of components and RE is different

#########
# 3 suffix Paradigmatic Model
#########

# Check Kappa
ThreeSuff_Paradigm <- check_collinearity(suff3_var, 
                                       c("word_log_freq", 
                                         "log_bigram_freq", 
                                         "word_length", 
                                         "RE"))
ThreeSuff_Paradigm
# 15.28975


ThreeSuff_Paradigm_PCA_df <- left_join(ThreeSuff_control_PCA_df, suff3_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "RE")

# Check kappa of components + RE
ThreeSuff_Paradigm_PCA <- check_collinearity(ThreeSuff_Paradigm_PCA_df, 
                                           c("PC1", "PC2", "PC3", "RE"))
ThreeSuff_Paradigm_PCA
# 3.983635
# Kappa is low enough that we can keep RE out of PCA and get interpretable results

# PCA run over control variables seperately, so they are identical to control model
# note: Scale of components and RE is different

#########
# 2 suffix Simple Morphemic
#########

# Check Kappa
TwoSuff_MorphSimple <- check_collinearity(suff2_var, 
                                         c("word_log_freq", 
                                           "log_bigram_freq", 
                                           "word_length", 
                                           "stem_wholeSuffix_TP"))
TwoSuff_MorphSimple
# 14.5861


TwoSuff_MorphSimple_PCA_df <- left_join(TwoSuff_control_PCA_df, suff2_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "stem_wholeSuffix_TP")

#Check kappa with PC + transition probability
TwoSuff_MorphSimple_PCA <- check_collinearity(TwoSuff_MorphSimple_PCA_df, 
                                           c("PC1", "PC2", "PC3", "stem_wholeSuffix_TP"))
TwoSuff_MorphSimple_PCA
# 30.31469
# The kappa is even worse, control variables orthogonalized in such a way that 
# they increase collinearity

# The directionality of variables for this model are uninterpretable 

#########
# 3 suffix Simple Morphemic
#########

# Check Kappa
ThreeSuff_MorphSimple <- check_collinearity(suff3_var, 
                                       c("word_log_freq", 
                                         "log_bigram_freq", 
                                         "word_length", 
                                         "stem_wholeSuffix_TP"))
ThreeSuff_MorphSimple
# 18.14564

ThreeSuff_MorphSimple_PCA_df <- left_join(ThreeSuff_control_PCA_df, suff3_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "stem_wholeSuffix_TP")

ThreeSuff_MorphSimple_PCA <- check_collinearity(ThreeSuff_MorphSimple_PCA_df, 
                                           c("PC1", "PC2", "PC3", "stem_wholeSuffix_TP"))
ThreeSuff_MorphSimple_PCA
# 41.66405
# The kappa is even worse, control variables orthogonalized in such a way that 
# they increase collinearity

# The directionality of variables for this model are uninterpretable 


#########
# 2 suffix Full Morphemic Model
#########

# Check Kappa
TwoSuff_MorphFull <- check_collinearity(suff2_var, 
                                         c("word_log_freq", 
                                           "log_bigram_freq", 
                                           "word_length", 
                                           "stem_a1_TP", 
                                           "a1_a2_TP"))
TwoSuff_MorphFull
# 35.79531

# Attempt 1: Only run PCA on control variables
TwoSuff_MorphFull_PCA_df <- left_join(TwoSuff_control_PCA_df, suff2_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "stem_a1_TP", "a1_a2_TP")

TwoSuff_MorphFull_PCA <- check_collinearity(TwoSuff_MorphFull_PCA_df, 
                                           c("PC1", "PC2", "PC3", "stem_a1_TP", "a1_a2_TP"))
TwoSuff_MorphFull_PCA
# 37.82984
# This made the kappa slightly worse

# Attempt 2: PCA control variables and transition probabilities seperately 

# Rename control variables and save to separate dataframe
TwoSuff_MorphFull_ControlPCA_df <- TwoSuff_control_PCA_df %>%
  rename("PC1_control" = "PC1",
         "PC2_control" = "PC2",
         "PC3_control" = "PC3")

# Run PCA on transition probabilities
c(TwoSuff_MorphFull_TPPCA_df, TwoSuff_MorphFull_TPPCA_results) %<-% 
  run_pca(suff2_var, c("stem_a1_TP", "a1_a2_TP"))

TwoSuff_MorphFull_TPPCA_df <- TwoSuff_MorphFull_TPPCA_df %>%
  rename("PC1_TP" = "PC1",
         "PC2_TP" = "PC2")
colnames(TwoSuff_MorphFull_TPPCA_results$rotation) <- c("PC1_TP", "PC2_TP")
colnames(TwoSuff_MorphFull_TPPCA_results$x)        <- c("PC1_TP", "PC2_TP")

# Join the two seperate sets of components
TwoSuff_MorphFull_PCA_df <- left_join(TwoSuff_MorphFull_ControlPCA_df, 
                                     TwoSuff_MorphFull_TPPCA_df, by=("word"))

# Check kappa
TwoSuff_MorphFull_PCA <- check_collinearity(TwoSuff_MorphFull_PCA_df, 
                                         c("PC1_control",
                                           "PC2_control", 
                                           "PC3_control", 
                                           "PC1_TP",
                                           "PC2_TP"))
TwoSuff_MorphFull_PCA
# 3.086141
# This reduces collinearity sufficiently. 
# However note the scales are different across the control and TP components

saveRDS(TwoSuff_MorphFull_TPPCA_df, file = "../output/PCA/2suff_MorphFullModel_TPVar_df.rds")
saveRDS(TwoSuff_MorphFull_TPPCA_results, file = "../output/PCA/2suff_MorphFullModel_TPVar_results.rds")

TwoSuff_MorphFull_TPPCA_results
#               PC1_TP    PC2_TP
# stem_a1_TP  0.7071068 0.7071068
# a1_a2_TP   -0.7071068 0.7071068

#########
# 3 suffix Full Morphemic Model
#########

# Check Kappa
ThreeSuff_MorphFull <- check_collinearity(suff3_var, 
                                        c("word_log_freq", 
                                          "log_bigram_freq", 
                                          "word_length", 
                                          "stem_a1_TP", 
                                          "a1_a2_TP",
                                          "a2_a3_TP"))
ThreeSuff_MorphFull
# 59.45279

# Attempt 1: Only run PCA on control variables
ThreeSuff_MorphFull_PCA_df <- left_join(ThreeSuff_control_PCA_df, suff3_var, by=("word")) %>%
  select("word", "PC1", "PC2", "PC3", "stem_a1_TP", "a1_a2_TP", "a2_a3_TP")

ThreeSuff_MorphFull_PCA <- check_collinearity(ThreeSuff_MorphFull_PCA_df, 
                                            c("PC1", "PC2", "PC3", "stem_a1_TP", "a1_a2_TP", "a2_a3_TP"))
ThreeSuff_MorphFull_PCA
# 54.19802
# This made the kappa better, but it's still large

# Attempt 2: PCA control variables and transition probabilities seperately 

# Rename control variables and save to separate dataframe
ThreeSuff_MorphFull_ControlPCA_df <- ThreeSuff_control_PCA_df %>%
  rename("PC1_control" = "PC1",
         "PC2_control" = "PC2",
         "PC3_control" = "PC3")

# Run PCA on transition probabilities
c(ThreeSuff_MorphFull_TPPCA_df, ThreeSuff_MorphFull_TPPCA_results) %<-% 
  run_pca(suff3_var, c("stem_a1_TP", "a1_a2_TP", "a2_a3_TP"))

ThreeSuff_MorphFull_TPPCA_df <- ThreeSuff_MorphFull_TPPCA_df %>%
  rename("PC1_TP" = "PC1",
         "PC2_TP" = "PC2",
         "PC3_TP" = "PC3")

colnames(ThreeSuff_MorphFull_TPPCA_results$rotation) <- c("PC1_TP", "PC2_TP", "PC3_TP")
colnames(ThreeSuff_MorphFull_TPPCA_results$x)        <- c("PC1_TP", "PC2_TP", "PC3_TP")

# Join the two seperate sets of components
ThreeSuff_MorphFull_PCA_df <- left_join(ThreeSuff_MorphFull_ControlPCA_df, 
                                        ThreeSuff_MorphFull_TPPCA_df, by=("word"))

# Check kappa
ThreeSuff_MorphFull_PCA <- check_collinearity(ThreeSuff_MorphFull_PCA_df, 
                                            c("PC1_control",
                                              "PC2_control", 
                                              "PC3_control", 
                                              "PC1_TP",
                                              "PC2_TP",
                                              "PC3_TP"))
ThreeSuff_MorphFull_PCA
# 2.328934
# This reduces collinearity sufficiently. 
# However note the scales are different across the control and TP components


saveRDS(ThreeSuff_MorphFull_TPPCA_df, file = "../output/PCA/3suff_MorphFullModel_TPVar_df.rds")
saveRDS(ThreeSuff_MorphFull_TPPCA_results, file = "../output/PCA/3suff_MorphFullModel_TPVar_results.rds")

ThreeSuff_MorphFull_TPPCA_results
#               PC1_TP       PC2_TP        PC3_TP
# stem_a1_TP -7.071068e-01 0.000000e+00 -7.071068e-01
# a1_a2_TP    7.071068e-01 1.290207e-15 -7.071068e-01
# a2_a3_TP   -9.123139e-16 1.000000e+00  9.123139e-16
#########
# Plots
#########

TwoSuff_control_plot <-  generate_PCA_loadings_plot(TwoSuff_control_PCA_results, 
                                                    c("#ececec", "#c3c4c3", "#717370"), 
                                                    c("Word Freq.", "Bigram Freq.", "word length"),
                                                    "Control Variables Loadings — 2 Suffixed Data")
  
TwoSuff_control_plot
ggsave(filename = "../figures/TwoSuff_Control_PCA.png", plot = TwoSuff_control_plot, width = 8, height = 6, dpi = 300)


ThreeSuff_control_plot <-  generate_PCA_loadings_plot(ThreeSuff_control_PCA_results, 
                                                    c("#ececec", "#c3c4c3", "#717370"), 
                                                    c("Word Freq.", "Bigram Freq.", "word length"),
                                                    "Control Variables Loadings — 3 Suffixed Data")

ThreeSuff_control_plot
ggsave(filename = "../figures/ThreeSuff_Control_PCA.png", plot = ThreeSuff_control_plot, width = 8, height = 6, dpi = 300)


TwoSuff_MorphFull_TP_plot <-  generate_PCA_loadings_plot(TwoSuff_MorphFull_TPPCA_results,
                                                           c("#A45BA4", "#D49CC6"),
                                                           c("Stem to 1st Suffix TP", "1st to 2nd Suffix TP"),
                                                           "TP Variables for Morphemic (Full) Model \n 2 Suffixed Data")

TwoSuff_MorphFull_TP_plot
ggsave(filename = "../figures/TwoSuff_MorphFull_TP_PCA.png",
       plot = TwoSuff_MorphFull_TP_plot, width = 8, height = 6, dpi = 300)


ThreeSuff_MorphFull_TP_plot <-  generate_PCA_loadings_plot(ThreeSuff_MorphFull_TPPCA_results, 
                                                      c("#A45BA4", "#D49CC6", "#DABDCF"), 
                                                      c("Stem to 1st Suff. TP", "1st to 2nd Suff. TP", "2nd to 3rd Suff. TP"),
                                                      "TP Variables for Morphemic (Full) Model \n 3 Suffixed Data")

ThreeSuff_MorphFull_TP_plot
ggsave(filename = "../figures/ThreeSuff_MorphFull_TP_PCA.png", 
       plot = ThreeSuff_MorphFull_TP_plot, width = 8, height = 6, dpi = 300)


