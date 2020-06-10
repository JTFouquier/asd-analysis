

setwd("~/lozupone_lab/autism_project/data-from-nancy")

library("readxl")
my_data <- read_excel("df-for-pearson-correlation.xlsx")
# pearson's corr test
corr = cor.test(my_data$pc1, my_data$otus)
