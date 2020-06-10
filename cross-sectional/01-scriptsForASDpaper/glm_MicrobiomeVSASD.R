#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("afex")
#install.packages("data.table")
### these packages will give me a pvalue in the anova summary
#library(data.table)
#library(lmerTest)
#library(afex)
#library(lme4)
rm(list = ls())
## where the files will be saved that are produced by ths script
setwd("/Users/Nancy/Documents/Autism_Project/qiime2redo/mergedTables/combinedCohortsAnalysis/final_analysis/genus")

# for genus table 
metaData = read.csv("/Users/Nancy/Documents/Autism_Project/qiime2redo/mergedTables/combinedCohortsAnalysis/final_analysis/combineCohorts_metadata.genus.csv")
#metaData = read.csv("/Users/Nancy/Documents/Autism_Project/qiime2redo/mergedTables/combinedCohortsAnalysis/randomForest1016/tableRandomForest/combineCohorts_metadata_withImportantASVs.csv")


# loop through columns of csv that have my taxa counts
taxa_abundance_start = 33
# for genus table
taxa_abundance_end = 73
# for ASV table
#taxa_abundance_end = 45
#taxa_abundance_end = 49

# fixed effects of the model
ASDyes = "ASD"
GIScore = "GI Score"
location = "change between AZ and CO"

# vectors to hold data that will be written to file 
taxa = c()
variable = c()
sigP = c()
t_value = c()


#start forloop
for (i in taxa_abundance_start:taxa_abundance_end) {
  #print(colnames(metaData)[i])
  factor = unlist(metaData[i])
  

  full.glmm = glm(factor ~ ASD+ConstipationDiarrheaAbdominalPain_GI_Score+Cohort, data=metaData) 
              

  #print("full model summary" )
  fullSummary = summary(full.glmm)
 # print(fullSummary)
  fixedEffects = (coef(fullSummary))
  #print(fixedEffects)
  
  # vector holding ASD and GI score labels
  variable = c(variable, ASDyes, GIScore, location )  

  
  ## this forloop will obtain the pvalue and tvalue for each taxa in file, these values will later be written to a file
    for (j in 2:4)
    {
      
      pvalue = round((fixedEffects[j,4]), 2)
      
      if (pvalue < 0.05)
      {
        sigPvalue = paste ("* ", pvalue)
        sigP = c(sigP, sigPvalue)
      }
      else 
      {
        sigP = c(sigP, pvalue)
      }

      tvalue = round((fixedEffects[j,3]), 2)
      t_value = c(t_value, tvalue)
      if (j == 2)
      {
        # only print the taxon name once
        taxa = c(taxa, colnames(metaData)[i], " ", " ")
       # loopImportance = c(loopImportance, importance[i], " ", " ")
        
      }

    }
  

  
  stats.results = data.frame(taxa, variable, sigP, t_value)
  stats.results$fdr.p = p.adjust(stats.results$sigP, method = "fdr")
  stats.results$bonferroni.p = p.adjust(stats.results$sigP, method = "bonferroni")
  # for genus table
write.table(x = stats.results, file = "importantFeatureGenusLevelTable_pvalue_tvalue_genus_TEST.tsv", col.names = TRUE, row.names = FALSE, append = FALSE, sep = "\t")
#write.table(x = stats.results, file = "importantFeatureASVLevelTable_pvalue_tvalue.tsv", col.names = TRUE, row.names = FALSE, append = FALSE, sep = "\t")
 
}# close outer forloop


