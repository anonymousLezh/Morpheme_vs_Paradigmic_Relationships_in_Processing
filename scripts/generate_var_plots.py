############
# Anonymous
# Fall 2026
# Generate histograms of variables
############

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df_2suff = pd.read_csv("../materials/2suff_stim_variables.csv")
df_3suff = pd.read_csv("../materials/3suff_stim_variables.csv")

print(df_2suff.head())
print(df_2suff.columns.tolist()) 


def generate_histogram(df, plot_var, plot_color, title, x_title):
    plt.figure(figsize=(8, 5))
    ax = sns.histplot(df[plot_var], 
                kde=True, 
                bins=20, 
                alpha=0.9,
                color=plot_color, 
                edgecolor='black')

    ax.lines[0].set_color('black')
    plt.grid(axis='y', linestyle='--', alpha=0.7)
    plt.title(title)
    plt.xlabel(x_title)
    plt.ylabel('Count')

    return(plt)

# ['word', 'lemma', 'suffix', 'lemma_log_freq', 'suffix_log_freq', 'RE', 'word_log_freq', 'stem_wholeSuffix_TP', 'stem_a1_TP', 'a1_a2_TP', 'stem_a2_TP', 'IC_token_freq', 'IC_log_freq', 'stem-a1_a2_TP']

TwoSuff_freq_plot = generate_histogram(df_2suff, "word_log_freq", "#939598", "2 suffix — Whole Word log Frequency", "log frequency") 
TwoSuff_freq_plot.savefig('../figures/variable_distributions/TwoSuff_freq_plot.png')

ThreeSuff_freq_plot = generate_histogram(df_3suff, "word_log_freq", "#939598", "3 suffix — Whole Word log Frequency", "log frequency") 
ThreeSuff_freq_plot.savefig('../figures/variable_distributions/ThreeSuff_freq_plot.png')



TwoSuff_RE_plot = generate_histogram(df_2suff, "RE", "#557BA9", "2 suffix — Relative Entropy", "Relative Entropy") 
TwoSuff_RE_plot.savefig('../figures/variable_distributions/TwoSuff_RE_plot.png')

ThreeSuff_RE_plot = generate_histogram(df_3suff, "RE", "#557BA9", "3 suffix — Relative Entropy", "Relative Entropy") 
ThreeSuff_RE_plot.savefig('../figures/variable_distributions/ThreeSuff_RE_plot.png')


TwoSuff_TP_Simple_plot = generate_histogram(df_2suff, "stem_wholeSuffix_TP", "#6B3A6D", "2 suffix — Transition Probability Simple", "Transition Probability") 
TwoSuff_TP_Simple_plot.savefig('../figures/variable_distributions/TwoSuff_TP-simple_plot.png')

ThreeSuff_TP_Simple_plot = generate_histogram(df_3suff, "stem_wholeSuffix_TP", "#6B3A6D", "3 suffix — Transition Probability Simple", "Transition Probability") 
ThreeSuff_TP_Simple_plot.savefig('../figures/variable_distributions/ThreeSuff_TP-simple_plot.png')



TwoSuff_TP_StemA1_plot = generate_histogram(df_2suff, "stem_a1_TP", "#A45BA4", "2 suffix — Stem to First Suffix Transition Probability", "Transition Probability") 
TwoSuff_TP_StemA1_plot.savefig('../figures/variable_distributions/TwoSuff_TP-Stem-A1_plot.png')

ThreeSuff_TP_StemA1_plot = generate_histogram(df_3suff, "stem_a1_TP", "#A45BA4", "3 suffix — Stem to First Suffix Transition Probability", "Transition Probability") 
ThreeSuff_TP_StemA1_plot.savefig('../figures/variable_distributions/ThreeSuff_TP-Stem-A1_plot.png')



TwoSuff_TP_A1A2_plot = generate_histogram(df_2suff, "a1_a2_TP", "#A45BA4", "2 suffix — First to Second Suffix Transition Probability", "Transition Probability") 
TwoSuff_TP_A1A2_plot.savefig('../figures/variable_distributions/TwoSuff_TP-A1-A2_plot.png')

ThreeSuff_TP_A1A2_plot = generate_histogram(df_3suff, "a1_a2_TP", "#A45BA4", "3 suffix — First to Second Suffix Transition Probability", "Transition Probability") 
ThreeSuff_TP_A1A2_plot.savefig('../figures/variable_distributions/ThreeSuff_TP-A1-A2_plot.png')


ThreeSuff_TP_A2A3_plot = generate_histogram(df_3suff, "a2_a3_TP", "#A45BA4", "3 suffix — Second to Third Suffix Transition Probability", "Transition Probability") 
ThreeSuff_TP_A2A3_plot.savefig('../figures/variable_distributions/ThreeSuff_TP-A2-A3_plot.png')