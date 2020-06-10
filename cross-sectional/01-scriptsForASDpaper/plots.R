
library(ggplot2)
library(ggsignif)
## where the files will be saved that are produce by ths script
setwd("/Users/Nancy/Documents/Autism_Project/qiime2redo/mergedTables/combinedCohortsAnalysis/final_analysis/genus")

# for genus table 
metaData = read.csv("/Users/Nancy/Documents/Autism_Project/qiime2redo/mergedTables/combinedCohortsAnalysis/final_analysis/combineCohorts_metadata.genus.csv")

# loop through columns of csv that have my taxa counts
taxa_abundance_start = 33
# for genus table
taxa_abundance_end = 45
#taxa_abundance_end = 34 


#taxa_abundance_start = 35
#taxa_abundance_end = 76

#pdf("AZregressions_ASD~microbiome.pdf",4,4)
#pdf("COregressions_ASD~microbiome.pdf",4,4)
 #pdf("regressions_ASD~microbiome_genus.pdf",4,4)
 #pdf("genusPlots.pdf",4,4)
 pdf("plotsTEST.pdf",4,4)

for (i in taxa_abundance_start:taxa_abundance_end) {
  factor = unlist(metaData[i])
  
 
 g = ggplot(metaData, aes(x=ASD, y=factor, fill= Cohort)) + 
   geom_jitter(alpha=0.2) + 
   geom_boxplot(outlier.shape = NA) + 
   labs(x= "ASD", y= (colnames(metaData)[i])) +
   theme(axis.title.y = element_text(size = 4)) 
   

  print(g)
  
}
dev.off()
