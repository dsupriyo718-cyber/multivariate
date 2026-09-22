rm(list=ls(all=TRUE))
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
#BiocManager::install("limma")
library(limma)
library(gclus)
library(Biobase)
library(dplyr)

getwd()
setwd("C:/Users/User/Desktop/4thYearAllPrac/Practical_404/P_3_RNA_Seq_GSE152418")
DataSet<- read.csv(file="GSE152418_series_matrix.csv", header = TRUE, sep=",")
head(DataSet)
tail(DataSet)

ID=DataSet[,1]
head(ID)
DataS=DataSet[,-1]
rownames(DataS)<- ID
head(DataS)
dim(DataS)
Data <- voom(DataS)
#View(Data)

Data <- as.matrix(Data)
group <- factor(c(rep('Case',16),rep('control',17)))
design.mat<-model.matrix(~ group)
limma <- lmFit(Data, design.mat)
limma.e<- eBayes(limma)
results <- topTable(limma.e, coef=2,  adjust="BH", n = nrow(Data), sort.by = "none")
head(results)
write.csv(results,file="Limma_results_GSE152418.csv")


##Select Upregulated genes

Upregulated_data<- results %>% filter(  adj.P.Val <0.001 & logFC >2)
dim(Upregulated_data)
head(Upregulated_data)
write.csv(Upregulated_data,"Upregulated_data_GSE152418.csv")

##Select Downregulated genes
Downregulated_data<- results %>% filter(  adj.P.Val <0.001 & logFC < "-2")
dim(Downregulated_data)
head(Downregulated_data)
write.csv(Downregulated_data,"Downregulated_data_GSE152418.csv")

