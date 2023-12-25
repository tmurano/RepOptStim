
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

setwd("~/Desktop/Processed Data_position_Ca transient/Figures_OF")



###################### read OF data

data <- read.csv("OF_TotalDist.csv", header = T) 

x1 <- data[, 22:24]  
x2 <- data[, 28:30]  

df <- data.frame( 
  Days = rep(c(1:24), 3),
  StimType = c(rep("NoStim", 24), rep("Stimx3+2wks", 24), rep("Stimx10+2wks", 24)),
  Mean = melt(x1)[,2],
  SEM = melt(x2)[,2]
)


###################### Make plot

g <- ggplot(df, aes(x = Days, y = Mean, color = StimType)) + 
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) + 
  geom_ribbon(aes(ymin=Mean-SEM, ymax=Mean+SEM, fill=StimType, colour=StimType),
              linetype = "blank", alpha = 0.3) +
  theme_bw(base_size = 30) +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=30)) +
  theme(axis.text=element_text(colour="black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 24.2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(1800, 8200))
plot(g)

ggsave("OF_TotalDist.png", g, width = 9, height = 6.5, dpi=300) 

 



###################### Make dataframe

data <- data[, 4:21]

data[] <- lapply(data, as.numeric)

rownames(data) <- c("Day1","Day2","Day3","Day4","Day5","Day6","Day7","Day8","Day9","Day10", 
                     "Day11","Day12","Day13","Day14","Day15","Day16","Day17","Day18","Day19","Day20", 
                     "Day21", "Day22", "Day23", "Day24")
data$rowname <- rownames(data)
data <- cbind(melt(data), c(rep("NoStim", 24*6), rep("Stimx3", 24*6), rep("Stimx10", 24*6)))
colnames(data) <- c("Day", "mouseID", "Value", "StimType")



###################### repeated 2-way ANOVA test

res.aov <- anova_test(
  data = data, dv = Value, wid = mouseID,
  between = StimType, within = Day
)
get_anova_table(res.aov)

#ANOVA Table (type III tests)
#Effect DFn DFd     F     p p<.05   ges
# 1     StimType   2  14 9.960 2.00e-03     * 0.368
# 2          Day  23 322 5.077 4.63e-12     * 0.176
# 3 StimType:Day  46 322 2.575 7.81e-07     * 0.179



###################### post-hoc test

pwc <- data %>%
  group_by(Day) %>%
  pairwise_t_test(
    Value ~ StimType, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc
write.xlsx(pwc, "OF_stat.xlsx")

# A tibble: 72 × 10
#Day   .y.   group1  group2     n1    n2        p p.signif    p.adj p.adj.signif
#* <chr> <chr> <chr>   <chr>   <int> <int>    <dbl> <chr>       <dbl> <chr>       
#1 Day1  Value NoStim  Stimx10     6     6 0.0723   ns       0.217    ns          
#2 Day1  Value NoStim  Stimx3      6     6 0.0991   ns       0.297    ns          
#3 Day1  Value Stimx10 Stimx3      6     6 0.00217  **       0.00652  **          
#4 Day10 Value NoStim  Stimx10     6     6 0.00208  **       0.00624  **          
#5 Day10 Value NoStim  Stimx3      6     6 0.218    ns       0.653    ns          
#6 Day10 Value Stimx10 Stimx3      6     6 0.0283   *        0.0849   ns          
#7 Day11 Value NoStim  Stimx10     6     6 0.0106   *        0.0319   *           
#8 Day11 Value NoStim  Stimx3      6     6 0.164    ns       0.493    ns          
#9 Day11 Value Stimx10 Stimx3      6     6 0.166    ns       0.499    ns          
#10 Day12 Value NoStim  Stimx10     6     6 0.000298 ***      0.000893 ***         
# ℹ 62 more rows





###################### end of program ###################### 
