setwd("D:\\4th year\\410-20250620T124058Z-1-001\\practice")
data<- read.csv("geneset.csv")
head(data)

library(dplyr)
upregulated<- data%>% filter(adj.P.Val<0.05 & logFC>2)
write.csv(upregulated,file="up_regulated_genes.csv")

downregulated<- data%>% filter(adj.P.Val<0.05 & logFC<2)
write.csv(downregulated,file="down_regulated_genes.csv")

deg<- rbind(upregulated, downregulated)
write.csv(deg,file="Differentially_expressed_genes.csv")
