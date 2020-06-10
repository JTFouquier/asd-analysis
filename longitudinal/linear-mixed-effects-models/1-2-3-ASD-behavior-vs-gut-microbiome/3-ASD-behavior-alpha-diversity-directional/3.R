

rm(list=ls()) # clear env
options(scipen = 999) # don"t use scientific notation

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

library(lme4)
library(ggplot2)
library(grid)
library(Matix)
library(ggsn)

pdf.title = "_LME_ASD_severity_20190729.pdf"

# USE for comparison to Euclidean distance matrices. These are pairwise comparisons
metadata = read.csv("3.csv")

subject = metadata$Subject

getSeverityLabels <- function(metric){

  if (metric == "hyperactivity"){
    y.label = "Hyperactivity"
    plot.title = "Hyperactivity"
  }
  else if (metric == "inapp_speech"){
    y.label = "Inappropriate Speech"
    plot.title = "Inappropriate Speech"
  }
  else if (metric == "irritability"){
    y.label = "Irritability"
    plot.title = "Irritability"
  }
  else if (metric == "lethargy"){
    y.label = "Lethargy"
    plot.title = "Lethargy"
  }
  else if (metric == "stereotypy"){
    y.label = "Stereotypy"
    plot.title = "Stereotypy"
  }
  return(c(y.label, plot.title))
} 
getMicrobiomeLabels <- function(metric){
  if (metric == "evenness"){ x.label = "Evenness" }
  else if (metric == "shannon"){ x.label = "Shannon" }
  else if (metric == "observed_otus"){ x.label = "Observed OTUs" }
  else if (metric == "faith"){ x.label = "Faith's PD" }
  
  return(x.label)
}


for (microbiome.metric in c("evenness", "faith", "observed_otus", "shannon")) {
  p_list = c()
  
  for (behavior.metric in c("hyperactivity", "inapp_speech", "irritability", "lethargy", "stereotypy")) {

    exp_var = unlist(metadata[microbiome.metric])
    res_var = unlist(metadata[behavior.metric])
    
    x.label = getMicrobiomeLabels(microbiome.metric)
    label.list = getSeverityLabels(behavior.metric)
    
    y.label = label.list[1]
    plot.title = label.list[2]

    # REDUCED MODEL
    model.reduced = lmer(res_var ~  (1+exp_var|subject), REML=FALSE)
  
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
        geom_smooth(method="lm", se=FALSE, size=1.9, color="gray61") +
        labs(x=x.label, y=y.label, fill="Individual") + 
        scale_color_manual(values=c("indianred1", "#FB61D7", "goldenrod2", 
                                    "#53B400", "seagreen2", "#00B6EB",  "#A58AFF")) +
        ggtitle(paste(plot.title)) +
        # ggtitle(paste(plot.title, " p=", round(p, 3))) +
        
        guides(color=guide_legend(title="Individual")) +
        theme(axis.title=element_text(size=16, face= "bold"), 
              plot.title = element_text(size = 24, face = "bold"),
              legend.title = element_text(size = 14),
              axis.text=element_text(size=13)) 
      
      # view plot in Rstudio plot panel/frame in addition to saving to PDF
      # start a PDF file, print file to the PDF file, the dev.off() to close. File will be named 
      # using the x & y labels in addition to what is written below
      pdf(paste(x.label, "_", y.label, "_", round(p, 3), pdf.title, sep=""))  #open a pdf device
      print(my_plot)
      dev.off()
    }

  }
  print(p_list)
  print(p.adjust(p_list, method="fdr"))
  remove(p_list)
}

