#R Script for plotting morphological measurements

#### install and load packages needed for graphing #############################

#install.packages("tidyverse")
#install.packages("cowplot")

library(tidyverse)
library(cowplot)

#### read in data. Download/ save your data as a csv file ####

data1 <- read_csv("di Stilio/trichome measurements.xlsx.csv") #this loads in your data and stores it as "data"

#### create objects that filter out specific measurements ######################

data1 %>% # this symbol is called pipes, it tells r to send it into the next function
  filter(Trichome == "circle") -> circles

data1 %>% 
  filter(Trichome == "branched") -> brancheds

#### plot data using ggplot ####################################################

plot1 <- ggplot(data = circles, aes(x = Stage, y = Perimeter, fill = Tissue)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge")+
  theme_cowplot() +
  geom_point(aes(x=Stage, y=Perimeter), position=position_jitterdodge()) 
plot1 



