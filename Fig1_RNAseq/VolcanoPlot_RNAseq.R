
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

setwd("~/Desktop/x")



###################### read data

info <- read.delim("Mouse_Info.txt", header = T, stringsAsFactors = F, colClasses = rep("character",4))
colnames(info) <- c("MouseName", "StimType", "OF.15min", "Opt.Stim") 

count <- read.delim("FPKM_count.txt", header = T, stringsAsFactors = F)
gene <- count[,1] 
count <- count[,-1]
colnames(count) <- info$MouseName
row.names(count) <- gene  



################# Select data

typ <- "Stimx3.2wks"
typ1 <- info[7:12,]

#typ <- "Stimx10.2wks"
#typ1 <- info[13:18,]

#typ <- "Stimx3.24hrs"
#typ1 <- info[19:24,]

#typ <- "Stimx10.24hrs"
#typ1 <- info[25:30,]


data <-  rbind(info[1:6,], typ1)
count_filtered = count[, colnames(count) %in% data$MouseName]



################# DE analysis

dds <- DESeqDataSetFromMatrix(countData = count_filtered, colData = data, design = ~StimType)
dds2 <- DESeq(dds)
res <- results(dds2)
resLFC <- lfcShrink(dds = dds2, res = res, type = "normal", coef = 2) 



################# Draw Volcano plot 

png(paste(c("Volcano_", typ, ".png"), collapse=""), width = 1800, height = 1600, res = 250)  
EnhancedVolcano(toptable = resLFC,
                x = "log2FoldChange", y = "padj", 
                lab = rownames(resLFC), 
                xlim = c(-1.1, 1.1), ylim = c(0, 25),
                pCutoff = 0.05, FCcutoff = log(1.2, 2), 
                pointSize = 1.0, 
                labSize = 10.0,
                selectLab = c(""))
dev.off()     



################# Draw Volcano plot 

diff_all = res %>% 
  as.data.frame() %>% 
  rownames_to_column("genes") %>% 
  filter(padj < 1) %>% 
  arrange(desc(log2FoldChange), desc(padj))  

write.csv(diff_all, paste(c(typ, "_AllGeneList.csv"), collapse=""))

 

###################### end of program ###################### 
