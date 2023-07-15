library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")
library(ggtree)
library(tidytree)
library(ape)
library(treeio)
library(data.table)

##Clade_1###
tree<-read.newick('data/tree/clade1/Clade1-MS-REL-SKYLINE50ML-combined.nwk')

metadata_df <- fread('data/tree/clade1/Annotation1.txt', header = TRUE) ### 

# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                              breaks = "2 week",
                              start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))


#Simple-Tree
p<-ggtree(tree, mrsd="2022-12-04",color='grey',size=0.5) %<+% metadata_df + 
  theme_tree2()
p  

p2<-p +
  scale_fill_manual(values=c('antiquewhite4','bisque2','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4','darkgreen','cadetblue3','indianred3'), name='Sampling locations',na.value="grey90")+
  geom_tippoint(aes(fill=location),size=3, color='black',shape=21, stroke=0.1) +
  #scale_x_date(date_labels = "%B-%Y",date_breaks = "1 month") +
  theme(axis.text=element_text(size=5)) +
  ggplot2::ylim(0, 105)+
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p2


##Aggregated-tree
p3<-p+scale_colour_manual(values=c("#262460",'#227045','#FBC348','#B4DADF','#FFFFFF','#FDE8D9','#EE7167','#A01D2A','#9FA8DA', '#F9EDCC')) +
  scale_fill_manual(values=c("#262460",'#227045','#FBC348','#B4DADF','#FFFFFF','#FDE8D9','#EE7167','#A01D2A','#9FA8DA', '#F9EDCC'), name='Genomes',na.value="grey90")+
  #geom_tippoint(fill='black',size=2, align=F, color='black',shape=21, stroke=0.1) +
  geom_tippoint(aes(
    subset=(country=='MS'), fill=location),size=3, align=F, color='red',shape=21, stroke=0.1) +
  geom_tippoint(aes(
    subset=(country=='Other'), fill=location),size=3, align=F, color='darkgrey',shape=21, stroke=0.1) +
  #scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) +
  expand_limits(y = 105) +
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p3

##Clade_2###
tree<-read.newick('data/tree/clade2/Clade2-MS-RELSKYLINE50ML.nwk')

metadata_df <- fread('data/tree/clade2/Annotation2.txt', header = TRUE) ### 

# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))


#Simple-Tree
p<-ggtree(tree, mrsd="2022-12-04",color='grey',size=0.5) %<+% metadata_df + 
  theme_tree2()
p  

p2<-p +
  scale_fill_manual(values=c('antiquewhite4','bisque2','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4','darkgreen','cadetblue3','indianred3'), name='Sampling locations',na.value="grey90")+
  geom_tippoint(aes(fill=location),size=3, color='black',shape=21, stroke=0.1) +
  #scale_x_date(date_labels = "%B-%Y",date_breaks = "1 month") +
  theme(axis.text=element_text(size=5)) +
  ggplot2::ylim(0, 105)+
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p2


##Aggregated-tree
p3<-p+scale_colour_manual(values=c("#262460",'#227045','#FBC348','#B4DADF','#FFFFFF','#FDE8D9','#EE7167','#A01D2A','#9FA8DA', '#F9EDCC')) +
  scale_fill_manual(values=c("#262460",'#227045','#FBC348','#B4DADF','#FFFFFF','#FDE8D9','#EE7167','#A01D2A','#9FA8DA', '#F9EDCC'), name='Genomes',na.value="grey90")+
  #geom_tippoint(fill='black',size=2, align=F, color='black',shape=21, stroke=0.1) +
  geom_tippoint(aes(
    subset=(country=='MS'), fill=location),size=3, align=F, color='red',shape=21, stroke=0.1) +
  geom_tippoint(aes(
    subset=(country=='Other'), fill=location),size=3, align=F, color='darkgrey',shape=21, stroke=0.1) +
  #scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) +
  expand_limits(y = 105) +
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p3





