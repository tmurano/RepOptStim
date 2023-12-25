
rm(list=ls(envir=globalenv()), envir=globalenv())

###################### install packages

if (!require(ggplot2)) install.packages('ggplot2')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(reshape2)) install.packages('reshape2')
if (!require(rstatix)) install.packages('rstatix')

library(ggplot2)
library(openxlsx) 
library(tidyverse)
library(reshape2)
library(rstatix)



###################### Set Working Directory 

setwd("~/Desktop")



###################### read OF data 

data <- read.xlsx('OF_Dist.xlsx', colNames=T)

NoStim_Ccnb  <- data[,2 :8 ]
NoStim_Scram <- data[,9 :15]
Stim10_Ccnb  <- data[,16:22]
Stim10_Scram <- data[,23:29]



###################### Make plot

NoStim_Ccnb_mean <- rowMeans(NoStim_Ccnb, na.rm = TRUE)
NoStim_Ccnb_sem  <- apply(NoStim_Ccnb, 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
NoStim_Scram_mean <- rowMeans(NoStim_Scram, na.rm = TRUE)
NoStim_Scram_sem  <- apply(NoStim_Scram, 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
Stim10_Ccnb_mean <- rowMeans(Stim10_Ccnb, na.rm = TRUE)
Stim10_Ccnb_sem  <- apply(Stim10_Ccnb, 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
Stim10_Scram_mean <- rowMeans(Stim10_Scram, na.rm = TRUE)
Stim10_Scram_sem  <- apply(Stim10_Scram, 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))


df <- data.frame(
  Day = rep(c(1:24), times = 2),
  StimType = c(rep("NoStim_Ccnb", 24),  rep("Stim10_Ccnb", 24)),
  Mean = c(NoStim_Ccnb_mean, Stim10_Ccnb_mean),
  SEM = c(NoStim_Ccnb_sem, Stim10_Ccnb_sem)
)
g <- ggplot(df, aes(x = Day, y = Mean, color = StimType)) + 
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin=Mean-SEM, ymax=Mean+SEM, fill=StimType, colour=StimType),
              linetype = "blank", alpha = 0.3) +
  geom_point(size = 2, fill = 'white', shape = 21, stroke = 1.5) +
  theme_bw(base_size = 18) +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red")) +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 18)) +
  theme(axis.text=element_text(colour="black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 24.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(3000, 8000), breaks = seq(3000, 8000, by = 1000))
plot(g)
ggsave("OF_Dist_Ccnb.png", g, width = 5, height = 4, dpi=300) 


df <- data.frame(
  Day = rep(c(1:24), times = 2),
  StimType = c(rep("NoStim_Scram", 24),  rep("Stim10_Scram", 24)),
  Mean = c(NoStim_Scram_mean, Stim10_Scram_mean),
  SEM = c(NoStim_Scram_sem, Stim10_Scram_sem)
)
g <- ggplot(df, aes(x = Day, y = Mean, color = StimType)) + 
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin=Mean-SEM, ymax=Mean+SEM, fill=StimType, colour=StimType),
              linetype = "blank", alpha = 0.3) +
  geom_point(size = 2, fill = 'white', shape = 16, stroke = 2) +
  theme_bw(base_size = 18) +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red")) +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 18)) +
  theme(axis.text=element_text(colour="black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 24.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(3000, 8000), breaks = seq(3000, 8000, by = 1000)) 
plot(g) 
ggsave("OF_Dist_Scram.png", g, width = 5, height = 4, dpi=300) 





######################  make dataframe

colnames(NoStim_Ccnb)  <- c("m1_1", "m1_2", "m1_3", "m1_4", "m1_5", "m1_6", "m1_7")
colnames(NoStim_Scram) <- c("m2_1", "m2_2", "m2_3", "m2_4", "m2_5", "m2_6", "m2_7")
colnames(Stim10_Ccnb)  <- c("m3_1", "m3_2", "m3_3", "m3_4", "m3_5", "m3_6", "m3_7")
colnames(Stim10_Scram) <- c("m4_1", "m4_2", "m4_3", "m4_4", "m4_5", "m4_6", "m4_7")


day <- c("Day1","Day2","Day3","Day4","Day5", "Day6","Day7","Day8","Day9","Day10", 
         "Day11","Day12","Day13","Day14","Day15", "Day16","Day17","Day18","Day19","Day20", 
         "Day21", "Day22","Day23","Day24") 

rownames(NoStim_Ccnb) <- day
rownames(NoStim_Scram) <- day
rownames(Stim10_Ccnb) <- day
rownames(Stim10_Scram) <- day


NoStim_Ccnb$rowname <- rownames(NoStim_Ccnb)
NoStim_Scram$rowname <- rownames(NoStim_Scram)
Stim10_Ccnb$rowname <- rownames(Stim10_Ccnb)
Stim10_Scram$rowname <- rownames(Stim10_Scram)


NoStim_Ccnb <- cbind(melt(NoStim_Ccnb), rep("NoStim_Ccnb", 168))
NoStim_Scram <- cbind(melt(NoStim_Scram), rep("NoStim_Scram", 168))
Stim10_Ccnb <- cbind(melt(Stim10_Ccnb), rep("Stim10_Ccnb", 168))
Stim10_Scram <- cbind(melt(Stim10_Scram), rep("Stim10_Scram", 168))


colnames(NoStim_Ccnb) <- c("Day", "mouseID", "Value", "StimType")
colnames(NoStim_Scram) <- c("Day", "mouseID", "Value", "StimType")
colnames(Stim10_Ccnb) <- c("Day", "mouseID", "Value", "StimType")
colnames(Stim10_Scram) <- c("Day", "mouseID", "Value", "StimType")


data <- rbind(NoStim_Ccnb, NoStim_Scram, Stim10_Ccnb, Stim10_Scram)



###################### repeated 2-way ANOVA test

res.aov <- anova_test(
data = data, dv = Value, wid = mouseID,
between = StimType, within = Day
)
get_anova_table(res.aov)

#ANOVA Table (type III tests)
#Effect DFn DFd     F     p p<.05   ges
#1     StimType  3.00  24.00 4.271 0.015     * 0.171
#2          Day  6.81 163.48 1.416 0.204       0.035
#3 StimType:Day 20.44 163.48 1.094 0.360       0.077



###################### post-hoc test

pwc <- data %>%
  group_by(Day) %>%
  pairwise_t_test(
    Value ~ StimType, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc
write.xlsx(pwc, "OF_stats.xlsx")

# A tibble: 144 × 10
#Day   .y.   group1       group2          n1    n2      p p.signif p.adj p.adj.signif
#* <chr> <chr> <chr>        <chr>        <int> <int>  <dbl> <chr>    <dbl> <chr>       
#1 Day1  Value NoStim_Ccnb  NoStim_Scram     7     7 0.85   ns       1     ns          
#2 Day1  Value NoStim_Ccnb  Stim10_Ccnb      7     7 0.721  ns       1     ns          
#3 Day1  Value NoStim_Scram Stim10_Ccnb      7     7 0.585  ns       1     ns          
#4 Day1  Value NoStim_Ccnb  Stim10_Scram     7     7 0.347  ns       1     ns          
#5 Day1  Value NoStim_Scram Stim10_Scram     7     7 0.451  ns       1     ns          
#6 Day1  Value Stim10_Ccnb  Stim10_Scram     7     7 0.199  ns       1     ns          
#7 Day10 Value NoStim_Ccnb  NoStim_Scram     7     7 0.871  ns       1     ns          
#8 Day10 Value NoStim_Ccnb  Stim10_Ccnb      7     7 0.218  ns       1     ns          
#9 Day10 Value NoStim_Scram Stim10_Ccnb      7     7 0.282  ns       1     ns          
#10 Day10 Value NoStim_Ccnb  Stim10_Scram     7     7 0.0195 *        0.117 ns          
# ℹ 134 more rows 






###################### end of program ###################### 
