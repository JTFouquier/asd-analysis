

rm(list=ls()) # clear env
options(scipen = 999) # don't use scientific notation

# add library load script

library(lme4)
library(ggplot2)
library(grid)
library(Matrix)
library(ggsn)

pdf.title = "_LME_gi_ASD_20190729.pdf"

metadata = read.csv("5-ASD.csv")

subject = metadata$Subject

getSeverityLabels <- function(metric){

  if (metric == 'euclidean_gi'){
    y.label = "Gastrointestinal Distress"
    plot.title = "Gastrointestinal Distress"
  }
  return(list(y.label, plot.title))
} 

getMicrobiomeLabels <- function(metric){
  if (metric == "unweighted_unifrac"){ x.label = "Unweighted UniFrac Distance"}
  else if (metric == "weighted_unifrac"){ x.label = "Weighted Unifrac Distance" }
  else if (metric == "absolute_delta_shannon"){ x.label = "Change in Shannon Diversity" }
  else if (metric == "absolute_delta_faith"){ x.label = "Change in Faith's PD" }
  else if (metric == "absolute_delta_observed_otus"){ x.label = "Change in Observed OTUs" }
  else if (metric == "absolute_delta_evenness"){ x.label = "Change in Evenness" }
  return(x.label)
}


p_list = c()

for (microbiome.metric in c('unweighted_unifrac', 'weighted_unifrac', 
                            'absolute_delta_evenness', 'absolute_delta_faith', 
                            'absolute_delta_observed_otus', 
                            'absolute_delta_shannon')) {

    for (gi.metric in c('euclidean_gi')) {

    exp_var = unlist(metadata[microbiome.metric])
    res_var = unlist(metadata[gi.metric])
    
    x.label = getMicrobiomeLabels(microbiome.metric)
    label.list = getSeverityLabels(gi.metric)

    y.label = label.list[1]
    plot.title = label.list[2]
    
    # REDUCED MODEL
    model.reduced = lmer(res_var ~ (1+exp_var|subject), REML=FALSE)

    # FULL MODEL
    model.full = lmer(res_var ~ exp_var + (1 + exp_var|subject), REML=FALSE)

    # get values from ANOVA result
    model_anova = anova(model.full, model.reduced)
    estimate = model.full@pp$delb[2]; p = model_anova$`Pr(>Chisq)`[2]; chisq = model_anova$Chisq[2]; df = model_anova$`Chi Df`[2]

    print(x.label)
    print(y.label)
    print(estimate)
    print(chisq)
    print(round(p, 4))
    
    p_list = c(p_list, p)
    
    # PLOT WITH ONE LINEAR REGRESSION LINE
    if (p<0.05) {
      my_plot = ggplot(metadata, aes(exp_var, res_var)) +
        geom_point(aes(group=Subject_Str, colour=Subject_Str), size=6) +
        geom_smooth(method="lm", se=FALSE, size=1.9, color='gray61') +
        labs(x=x.label, y=y.label, fill="Individual") + 
        scale_color_manual(values=c("indianred1", "#FB61D7", "goldenrod2", 
                                    "#53B400", "seagreen2", "#00B6EB",  "#A58AFF", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")) +
        ggtitle(paste(plot.title, ' p=', round(p, 4))) +
        guides(color=guide_legend(title="Individual")) +
        theme(axis.title=element_text(size=16, face= "bold"), 
              plot.title = element_text(size = 24, face = "bold"),
              legend.title = element_text(size = 14),
              axis.text=element_text(size=13)) 
      
      print(my_plot)
      # view plot in Rstudio plot panel/frame in addition to saving to PDF
      # start a PDF file, print file to the PDF file, the dev.off() to close. File will be named 
      # using the x & y labels in addition to what is written below
      pdf(paste(x.label, "_", y.label, "_", round(p, 4), pdf.title, sep=""))  #open a pdf device
      print(my_plot)
      dev.off()  
      
    }

  }

}
print(p_list)
print(p.adjust(p_list, method='fdr'))
remove(p_list)