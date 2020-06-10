


rm(list=ls())
options(scipen = 999) # don't use scientific notation

library(lme4)
library(ggplot2)


setwd('/Users/jenniferfouquier/lozupone_lab/autism_project/microbial_analysis/09_core_metrics_results/longitudnal/analysis_over_time')

metadata = read.csv("/Users/jenniferfouquier/lozupone_lab/autism_project/microbial_analysis/09_core_metrics_results/longitudnal/analysis_over_time/jf_data_time_unifrac.csv")


subject = metadata$Subject
unweighted_unifrac = metadata$unweighted
weighted_unifrac = metadata$weighted


time = metadata$time
asd = metadata$ASD

## EUCLIDEAN SEVERITY + UNWEIGHTED UNIFRAC

# REDUCED MODEL
model.reduced = lmer(time ~ (1|subject), REML=FALSE)
print("model.reduced")
print(summary(model.reduced))

# FULL MODEL
model.full = lmer(time ~ weighted_unifrac*asd+(1|subject), REML=FALSE)
print("model.full")
print(summary(model.full))

print("###### ANOVA ######")
model_anova = anova(model.full, model.reduced)
print(model_anova)
