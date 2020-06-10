library(FSA)    #package that has dunnTest() in it. for details: https://rcompanion.org/rcompanion/d_06.html
library(plyr)   #package for median summary by group
setwd("/Users/Nancy/Documents/Autism_Project/UCD_and_ASU.cohorts/analysis_extractionLocationAndSiblings/Nov.2017/combineCohortsAnalysis/analysis_usingRandomIndexScript/CombinedCohorts/organizingFiles2019")
inf <- ("/Users/Nancy/Documents/Autism_Project/UCD_and_ASU.cohorts/analysis_extractionLocationAndSiblings/Nov.2017/combineCohortsAnalysis/analysis_usingRandomIndexScript/CombinedCohorts/combineCohorts_metadata_R.tsv" )#path to data file
#read.table(file path, header=TRUE - the first row in file are headers, sep - separation character, strip.white = TRUE - removes spaces before/after cell input)
df <- read.table(inf, header=TRUE, sep="\t", strip.white=TRUE)
#df$ConstipationDiarrheaAbdominalPain_GI_Score_all_integers <- as.factor(df$ConstipationDiarrheaAbdominalPain_GI_Score_all_integers)
#stats - only need to change column variable i.e. ConstipationDiarrheaAbdominalPain_GI_Score_all_integers
#group summary
GIS <- ddply(df,~sub_cohort, summarise, mean=mean(ConstipationDiarrheaAbdominalPain_GI_Score, na.rm = TRUE),
      sd=sd(ConstipationDiarrheaAbdominalPain_GI_Score, na.rm = TRUE),
      median=median(ConstipationDiarrheaAbdominalPain_GI_Score, na.rm = TRUE))
# AgeS<- ddply(df,~sub_cohort, summarise, mean=mean(Age_In_Years), 
#              sd=sd(Age_In_Years),
#              median=median(Age_In_Years))
print(GIS)
#kruskal-wallace: is there a different group?
kw <- kruskal.test(data=df, ConstipationDiarrheaAbdominalPain_GI_Score~sub_cohort)
print (kw)
#dunns test: what groups are signficantly different?
dt <- dunnTest(df$ConstipationDiarrheaAbdominalPain_GI_Score~df$sub_cohort, method="bh")
print (dt)

write.table(GIS, 'gastrointestinal_test.tsv', quote=F, sep='\t')
write.table(AgeS, 'age_test.tsv', quote=F, sep='\t')
