rm(list=ls())
options(scipen = 999)

install.packages("lme4")
install.packages("vegan")
install.packages("ggplot2")

library(vegan)
library(lme4)
library(ggplot2)

# time in months
time = c(13,4,9.23,5.23,3.67,9.43,5.76,5.3,3.2,2.97,8.77,5.8,12.6,5.8,10.9,
         5.9,5.47,9.93,4.46,3.17,3.5,5.77)
unweighted = c(0.28888010905,0.237725547555,0.219268979401,0.246654960048,
               0.26444005287,0.217830322322,0.295260240101,0.213708084429,
               0.352721786583,0.243386345878,0.300530476453,0.263668155227,
               0.404871573717,0.162277769558,0.407502942507,0.400133352185,
               0.322639847342,0.148802308947,0.296833201005,0.258670981306,
               0.26719913335,0.319388073218)
weighted = c(0.250405399,0.143320292,0.221448669,0.300183276,0.106750785,
             0.301022682,0.28752638,0.119843726,0.246105238,0.092315999,
             0.205174906,0.239619462,0.208862652,0.215882681,0.359664757,
             0.244236711,0.132461586,0.055818548,0.146787394,0.176045582,
             0.248022271,0.485492135)

colors = c('darkseagreen1',	'forestgreen',	'forestgreen',	'forestgreen',
           'cornflowerblue',	'cornflowerblue',	'cornflowerblue',	
           'darkolivegreen2',	'mediumorchid4',	'deepskyblue1',	'deepskyblue1',	
           'deepskyblue1',	'darkorange',	'firebrick1',	'firebrick3',	
           'lightpink',	'deeppink3',	'deeppink3',	'deeppink3',	'brown1',	
           'gold2',	'darkgoldenrod2')

subject = c('12','14','14','14','22','22','22','24','27','7','7','7','13','15',
            '30','31','32','32','32','38','39','40')

metric1 = time
metric2 = unweighted

xtext = 'Change in Time (Months)'
ytext = 'Unweighted UniFrac Distance'
pdf_name = "beta_div_related_to_delta_time_pdfs_20200531.pdf"

# REDUCED MODEL
model.reduced = lmer(metric1 ~ (1|subject), REML=FALSE)
print(summary(model.reduced))
# FULL MODEL
model.full = lmer(metric1 ~ metric2+(1|subject), REML=FALSE)
print(summary(model.full))

model_anova = anova(model.full, model.reduced)
estimate = model.full@pp$delb[2]; p = model_anova$`Pr(>Chisq)`[2]
chisq = model_anova$Chisq[2]; df = model_anova$`Chi Df`[2]

print(estimate); print(chisq); print(p)

correlation_test = ""
correlation_test = cor.test(metric1, metric2,  method = "pearson", 
                            use = "complete.obs")
p = as.numeric(correlation_test$p.value)
r = as.numeric(correlation_test$estimate)
print(cor.test)

pdf_name = paste0(xtext, '_', ytext, '_', round(p, digits=5), '_', 
                  round(r, digits=2), '_', pdf_name)
pdf(pdf_name)
plot(metric1, metric2, pch=19, cex=3, xlab=xtext, ylab=ytext, col=colors)
try(abline(lm(metric2 ~ metric1), lwd=4), silent=TRUE)
dev.off()



# Change in weighted unifrac between sampling days


metric1 = time
metric2 = weighted

xtext = 'Change in Time (Months)'
ytext = 'Weighted UniFrac Distance'
pdf_name = "beta_div_vs_delta_time_20200531.pdf"

# REDUCED MODEL
model.reduced = lmer(metric1 ~ (1|subject), REML=FALSE)
print(summary(model.reduced))
# FULL MODEL
model.full = lmer(metric1 ~ metric2+(1|subject), REML=FALSE)
print(summary(model.full))

model_anova = anova(model.full, model.reduced)
estimate = model.full@pp$delb[2]; p = model_anova$`Pr(>Chisq)`[2]
chisq = model_anova$Chisq[2]; df = model_anova$`Chi Df`[2]

print(estimate); print(chisq); print(p)

correlation_test = ""
correlation_test = cor.test(metric1, metric2,  method = "pearson", 
                            use = "complete.obs")
p = as.numeric(correlation_test$p.value)
r = as.numeric(correlation_test$estimate)
print(cor.test)

pdf_name = paste0(xtext, '_', ytext, '_', round(p, digits=5), '_', 
                  round(r, digits=2), '_', pdf_name)
pdf(pdf_name)
plot(metric1, metric2, pch=19, cex=3, xlab=xtext, ylab=ytext, col=colors)
try(abline(lm(metric2 ~ metric1), lwd=4), silent=TRUE)
dev.off()





