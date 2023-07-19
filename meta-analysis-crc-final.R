
library(esc)
library(tidyverse)
library(meta)
library(grid)
library(gridExtra)
library(ggrepel)
library(metafor)
library(ggplot2)
library(fpc)
library(mclust)

data <- read.csv("crc-meta-analysis-raw-data.csv")

#Run meta analysis with all included studies: 
m.bin <- metabin(event.e=data$event.e,
                 n.e=data$n.e,
                 event.c=data$event.c,
                 n.c=data$n.c,
                 studlab=data$author,
                 data=data,
                 sm="OR",
                 method="MH",
                 MH.exact=TRUE,
                 fixed=FALSE,
                 random=TRUE,
                 method.tau="PM",
                 hakn=TRUE,
                 title="Colorectal Cancer Screening and Rurality")

summary(m.bin)


#Methods to identify outliers due to high heterogeneity: 

find.outliers(m.bin)

#### Create a new dataframe removing outliers: 
data2 <- filter(data, author!="Schumacher 2008") #####
data2 <- filter(data2, author!="Anderson 2013")
data2 <- filter(data2, author!="Ko 2005")
data2 <- filter(data2, author!="Wang 2017")

#Meta analysis removing outliers: 

m.bin2 <- metabin(event.e=data2$event.e,
                 n.e=data2$n.e,
                 event.c=data2$event.c,
                 n.c=data2$n.c,
                 studlab=data2$author,
                 data=data2,
                 sm="OR",
                 method="MH",
                 MH.exact=TRUE,
                 fixed=FALSE,
                 random=TRUE,
                 method.tau="PM",
                 hakn=TRUE,
                 title="Colorectal Cancer Screening and Rurality")

summary(m.bin2)

#10 studies remaining from 14 originally. 

#Generate forest plot with outliers: 
meta::forest.meta(m.bin,
                  sortvar=TE,
                  prediction=TRUE,
                  print.tau2=FALSE,
                  leftlabs=c("Author","OR","SE"),
                  layout="RevMan5")



#Generate forest plot without the outliers: 
meta::forest.meta(m.bin2,
                  sortvar=TE,
                  prediction=TRUE,
                  print.tau2=FALSE,
                  leftlabs=c("Author","OR","SE"),
                  layout="RevMan5")

#Save forest plot as .tiff: 
png(file="foresplot-crc.png", width=2800, height=2400, res=600)


#Funnel Plot (for publication bias):

funnel.meta(m.bin2,
            xlim=c(0.5,1.5),
            studlab=TRUE)
title("Funnel Plot")
col.contour=c("green", "blue", "red")

funnel.meta(m.bin2, xlim=c(0.5,1.5),
            contour=c(0.9, 0.95, 0.99), #These represent p<0.1, p<0.05, and p<0.01 
            col.contour=col.contour)

legend(x = 1.2, y = 0.1, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

title("Contour-Enahnced Funnel Plot")



