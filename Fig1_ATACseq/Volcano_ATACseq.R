
rm(list=ls(envir=globalenv()), envir=globalenv())

###################### install packages

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EnhancedVolcano")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("DESeq2")


if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(dplyr)) install.packages('dplyr')

library(ggplot2)
library(tidyverse)
library(dplyr)
library(EnhancedVolcano) 
library(DESeq2) 



###################### Set Working Directory 

setwd("~/Desktop/")



###################### read data

info <- read.delim("Mouse_Info.txt", header = T, stringsAsFactors = F, colClasses = rep("character",3))
colnames(info) <- c("MouseName", "StimType", "OF.15min") 

count <- read.csv("Peak.csv", header = T) 
gene <- count[,1] 
count <- count[,6:17] 
colnames(count) <- info$MouseName
row.names(count) <- gene  


 
################# Select data

typ <- "Stimx3.2wks"
typ1 <- info[5:8,]

typ <- "Stimx10.2wks"
typ1 <- info[9:12,] 


data <-  rbind(info[1:4,], typ1)
count_filtered = count[, colnames(count) %in% data$MouseName]



################# DE analysis

dds <- DESeqDataSetFromMatrix(countData = count_filtered, colData = data, design = ~StimType)
dds2 <- DESeq(dds)
res <- results(dds2)　
resLFC <- lfcShrink(dds = dds2, res = res, type = "normal", coef = 2)  



################# Draw Volcano plot 

png(paste(c("Volcano_", typ, ".png"), collapse=""), width = 1800, height = 2200, res = 250)  
EnhancedVolcano(toptable = res, 
                x = "log2FoldChange", y = "padj",
                lab = rownames(res), xlim = c(-4, 4), ylim = c(0, 30),
                pCutoff = 0.05, FCcutoff = log(100, 2), 
                pointSize = 0.5, 
                selectLab = c(""), 
                title = "EnhancedVolcano \n (FC cutoff=0, padj cutoff=0.05)")
dev.off()     



################# Draw Volcano plot 

diff_all = res %>% 
  as.data.frame() %>% 
  rownames_to_column("genes") %>% 
  filter(padj < 1) %>% 
  arrange(desc(log2FoldChange), desc(padj))  

write.csv(diff_all, paste(c(typ, "_AllGeneList.csv"), collapse=""))



###################### end of program ###################### 
