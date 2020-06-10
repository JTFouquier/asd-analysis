

rm(list=ls()) # clear env
options(scipen = 999) # don't use scientific notation

# add library load script

library(lme4)
library(ggplot2)
library(grid)
library(Matix)
library(ggsn)

pdf.title = "_LME_severity_20190729.pdf"

# USE for comparison to Euclidean distance matrices. These are pairwise comparisons
metadata = read.csv("1.csv")

subject = metadata$Subject

# make sure to change axis labels here. They are also used to name the PDF file
# absolute_irritability	absolute_hyperactivity	absolute_stereotypy	
# absolute_inapp_speech	absolute_lethargy	irritability	hyperactivity	stereotypy	inapp_speech	lethargy	weighted_unifrac	unweighted_unifrac	euclideasn_severity	euclidean_diet	absolute_delta_shannon	absolute_delta_faith	absolute_delta_observed_otus	absolute_delta_evenness				


# can microbiome metrics (beta div or alpha div) explain the response variable
# in our case the autism severity


getSeverityLabels <- function(metric){

  if (metric == "absolute_hyperactivity"){
    y.label = "Change in Hyperactivity"
    plot.title = "Hyperactivity"
  }
  else if (metric == "absolute_inapp_speech"){
    y.label = "Change in Inappropriate Speech"
    plot.title = "Inappropriate Speech"
  }
  else if (metric == "absolute_irritability"){
    y.label = "Change in Irritability"
    plot.title = "Irritability"
  }
  else if (metric == 'absolute_lethargy'){
    y.label = "Change in Lethargy"
    plot.title = "Lethargy"
  }
  else if (metric == 'absolute_stereotypy'){
  y.label = "Change in Stereotypy"
  plot.title = "Stereotypy"
  }
  else if (metric == 'euclidean_behavior'){
    y.label = "Autism Behavior EDM"
    plot.title = "Autism Behavior"
  }
  return(list(y.label, plot.title))
} 

getMicrobiomeLabels <- function(metric){
  if (metric == "unweighted_unifrac"){ x.label = "Unweighted UniFrac Distance"}
  else if (metric == "weighted_unifrac"){ x.label = "Weighted Unifrac Distance" }
  return(x.label)
}


for (microbiome.metric in c('unweighted_unifrac', 'weighted_unifrac')) {
    p_list = c()
  
    for (behavior.metric in c('absolute_hyperactivity', 
                              'absolute_inapp_speech',  'absolute_irritability', 'absolute_lethargy', 
                              'absolute_stereotypy')) {
      
    exp_var = unlist(metadata[microbiome.metric])
    res_var = unlist(metadata[behavior.metric])
    
    x.label = getMicrobiomeLabels(microbiome.metric)
    label.list = getSeverityLabels(behavior.metric)
    
    y.label = label.list[1]
    plot.title = label.list[2]
    euclidean_diet = metadata$euclidean_diet
    euclidean_gi = metadata$euclidean_gi

    # REDUCED MODEL
    model.reduced = lmer(res_var ~  (1+exp_var|subject), 
                         REML=FALSE)

    # FULL MODEL
    model.full = lmer(res_var ~ exp_var + (1 + exp_var|subject), 
                      REML=FALSE)

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
                                    "#53B400", "seagreen2", "#00B6EB",  "#A58AFF")) +
        # ggtitle(paste(plot.title, ' p=', round(p, 3))) +
        ggtitle(paste(plot.title)) +
        guides(color=guide_legend(title="Individual")) +
        theme(axis.title=element_text(size=16, face= "bold"), 
              plot.title = element_text(size = 24, face = "bold"),
              legend.title = element_text(size = 14),
              axis.text=element_text(size=13)) 
      
      print(my_plot)
      # view plot in Rstudio plot panel/frame in addition to saving to PDF
      # start a PDF file, print file to the PDF file, the dev.off() to close. File will be named 
      # using the x & y labels in addition to what is written below
      pdf(paste(x.label, "_", y.label, "_", round(p, 3), pdf.title, sep=""))  #open a pdf device
      print(my_plot)
      dev.off()
    }
  }
    print(p_list)
    print(p.adjust(p_list, method='fdr'))
    remove(p_list)
}

