rm(list=ls())
options(scipen = 999)

library(vegan)
library(lme4)
library(ggplot2)
setwd('/Users/jenniferfouquier/lozupone_lab/autism_project/microbial_analysis/09_core_metrics_results/longitudnal/analysis_over_time')

# data_labels = c('ASD:12.1-12.2','ASD:14.1-14.2','ASD:14.1-ASD.14.3','ASD:14.2-ASD.14.3','ASD:22.1-ASD.22.2','ASD:22.1-ASD.22.3','ASD:22.2-22.3','ASD:24.1-24.2','ASD:27.1-ASD.27.2','ASD:7.1-7.2','ASD:7.1-7.3','ASD:7.2-7.3','NT:13.1-13.2','NT:15.2-15.3','NT:30.1-30.3','NT:31.1-31.2','NT:32.1-32.2','NT:32.1-32.3','NT:32.2-32.3','NT:38.1-38.2','NT:39.1-39.2','NT:40.1-40.2')

# data_labels = c('ASD:12.1-12.2','ASD:14.1-14.2','ASD:14.1-ASD.14.3','ASD:14.2-ASD.14.3','ASD:22.1-ASD.22.2','ASD:22.1-ASD.22.3','ASD:22.2-22.3','ASD:24.1-24.2','ASD:27.1-ASD.27.2','ASD:7.1-7.2','ASD:7.1_7.3','ASD:7.2-7.3','NT:13S.1B_NT.13S.2','NT:15S.2_NT.15S.3','NT:30C.1_NT.30C.3','NT:31C.1_NT.31C.2','NT:32C.1_NT.32C.2.run1','NT:32C.1_NT.32C.3','NT:32C.2.run1_NT.32C.3','NT:38C.1.ASU_NT.38C.2B','NT:39C.1_NT.39C.2','NT:40C.1_NT.40C.2')
time = c(13,4,9.23,5.23,3.67,9.43,5.76,5.3,3.2,2.97,8.77,5.8,12.6,5.8,10.9,5.9,5.47,9.93,4.46,3.17,3.5,5.77)
unweighted = c(0.28888010905,0.237725547555,0.219268979401,0.246654960048,0.26444005287,0.217830322322,0.295260240101,0.213708084429,0.352721786583,0.243386345878,0.300530476453,0.263668155227,0.404871573717,0.162277769558,0.407502942507,0.400133352185,0.322639847342,0.148802308947,0.296833201005,0.258670981306,0.26719913335,0.319388073218)
weighted = c(0.250405399,0.143320292,0.221448669,0.300183276,0.106750785,0.301022682,0.28752638,0.119843726,0.246105238,0.092315999,0.205174906,0.239619462,0.208862652,0.215882681,0.359664757,0.244236711,0.132461586,0.055818548,0.146787394,0.176045582,0.248022271,0.485492135)


# library(RColorBrewer)
# display.brewer.all()
# display.brewer.pal(n=9, name = 'Blues')
# display.brewer.pal(n=9, name = 'GnBu')


colors = c('darkseagreen1',	'forestgreen',	'forestgreen',	'forestgreen',	'cornflowerblue',	'cornflowerblue',	'cornflowerblue',	'darkolivegreen2',	'mediumorchid4',	'deepskyblue1',	'deepskyblue1',	'deepskyblue1',	'darkorange',	'firebrick1',	'firebrick3',	'lightpink',	'deeppink3',	'deeppink3',	'deeppink3',	'brown1',	'gold2',	'darkgoldenrod2')

# colors = c('#fff5eb', '#fee6ce', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#a63603', '#7f2704',  '#c6dbef', '#9ecae1','#6baed6','#4292c6', '#2171b5', '#08519c', '#08306b')

unweighted = c(0.28888010905,0.237725547555,0.219268979401,0.246654960048,0.26444005287,0.217830322322,0.295260240101,0.213708084429,0.352721786583,0.243386345878,0.300530476453,0.263668155227,0.404871573717,0.162277769558,0.407502942507,0.400133352185,0.322639847342,0.148802308947,0.296833201005,0.258670981306,0.26719913335,0.319388073218)
subject = c('12','14','14','14','22','22','22','24','27','7','7','7','13','15','30','31','32','32','32','38','39','40')



df = data.frame(data_labels, time, unweighted, weighted, subject)
print(df)
write.csv(df, file='time-input-data.csv')

metric1 = time
metric2 = unweighted

xtext = 'Change in Time (Months)'
ytext = 'Unweighted UniFrac Distance'
pdf_name = "beta_div_related_to_delta_time_pdfs_072419.pdf"

# value lists for each severity metric

# REDUCED MODEL
model.reduced = lmer(metric1 ~ (1|subject), REML=FALSE);print(summary(model.reduced))

# FULL MODEL
model.full = lmer(metric1 ~ metric2+(1|subject), REML=FALSE);print(summary(model.full))

model_anova = anova(model.full, model.reduced)
estimate = model.full@pp$delb[2]; p = model_anova$`Pr(>Chisq)`[2]; chisq = model_anova$Chisq[2]; df = model_anova$`Chi Df`[2]

print(estimate); print(chisq); print(p)

correlation_test = ""
correlation_test = cor.test(metric1, metric2,  method = "pearson", use = "complete.obs")
p = as.numeric(correlation_test$p.value)
r = as.numeric(correlation_test$estimate)
print(cor.test)


pdf_name = paste0(xtext, '_', ytext, '_', round(p, digits=5), '_', round(r, digits=2), '_', pdf_name)
pdf(pdf_name)
plot(metric1, metric2, pch=19, cex=3, xlab=xtext, ylab=ytext, col=colors)
try(abline(lm(metric2 ~ metric1), lwd=4), silent=TRUE)
# text(metric1, metric2, labels=data_labels, cex=0.7, pos=4, col=colors)

dev.off()

# create_graphs(time, unweighted, subject, 'Change in Time (Months)', 'Weighted UniFrac Distance', data_labels, colors)