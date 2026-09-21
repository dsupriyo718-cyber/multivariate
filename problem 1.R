setwd("D:\\410")
data=read.csv("Gene_table.csv")
data
install.packages("dplyr")
library(dplyr)
upregulated=data%>%filter(adj.P.Val<0.05&logFC>2)
write.csv(upregulated,file="up_regulated_genes.csv")

downregulated=data%>%filter(adj.P.Val<0.05&logFC< -2)
write.csv(upregulated,file="down_regulated_genes.csv")

deg=rbind(upregulated,downregulated)
write.csv(deg,file="differentially_express_genes.csv")
