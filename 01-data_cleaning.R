#### Preamble ####
# Purpose: Clean the survey data downloaded from: https://open.toronto.ca/dataset/transformto-net-zero-strategy-public-consultation-resident-survey/
# Author: Zixian He
# Data: 27 April, 2022
# Contact: zixian.he@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)	
library(opendatatoronto)
library(dplyr)

# get package
package <- show_package("77e593cf-877b-4bcc-bfdf-0b99e2935e5f")
package

# Read in the raw data. 
raw_data <- readr::read_csv("inputs/data/raw_data.csv")
data <- read.csv("/Users/eileen/Downloads/Raw data.csv",check.names = F)
# Just keep some variables that may be of interest (change 
# this depending on your interests)

         

# Create new data.
Buildings <- data.frame(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6])
colnames(Buildings)=c("Buildings 1","Buildings 2","Buildings 3","Buildings 4","Buildings 5","Buildings 6")

Energy <-data.frame(data[,9],data[,10],data[,11],data[,12],data[,13])
colnames(Energy)=c("Energy 1","Energy 2","Energy 3","Energy 4","Energy 5")

Transportation <- data.frame(data[,16],data[,17],data[,18],data[,19],data[,20],data[,21])
colnames(Transportation)=c("Transportation 1","Transportation 2","Transportation 3","Transportation 4","Transportation 5","Transportation 6")

SCW <- data.frame(data[,24],data[,25],data[,26])
colnames(SCW)=c("S-C&Waste 1","S-C&Waste 2","S-C&Waste 3")
summary(SCW)

GDE <- data.frame(data[,29],data[,30],data[,31],data[,32],data[,33],data[,34],data[,35])
colnames(GDE)=c("Greenspace&D-M&E-E 1","Greenspace&D-M&E-E 2","Greenspace&D-M&E-E 3","Greenspace&D-M&E-E 4","Greenspace&D-M&E-E 5","Greenspace&D-M&E-E 6","Greenspace&D-M&E-E 7")

Info <- data.frame(data[,39],data[,53],data[,55],data[,57])
colnames(Info)=c("Age","Gender","Housing","Income")

new_data <- cbind(Buildings, Energy, Transportation, SCW, GDE, Info)

# count buildings climate actions

as.data.frame(table(new_data$`Buildings 1`))
as.data.frame(table(new_data$`Buildings 2`))
as.data.frame(table(new_data$`Buildings 3`))
as.data.frame(table(new_data$`Buildings 4`))
as.data.frame(table(new_data$`Buildings 5`))
as.data.frame(table(new_data$`Buildings 6`))


# count energy climate actions
as.data.frame(table(new_data$`Energy 1`))
as.data.frame(table(new_data$`Energy 2`))
as.data.frame(table(new_data$`Energy 3`))
as.data.frame(table(new_data$`Energy 4`))
as.data.frame(table(new_data$`Energy 5`))


# count transportation climate actions
as.data.frame(table(new_data$`Transportation 1`))
as.data.frame(table(new_data$`Transportation 2`))
as.data.frame(table(new_data$`Transportation 3`))
as.data.frame(table(new_data$`Transportation 4`))
as.data.frame(table(new_data$`Transportation 5`))
as.data.frame(table(new_data$`Transportation 6`))


# count Sustainable consumption and waste climate actions
as.data.frame(table(new_data$`S-C&Waste 1`))
as.data.frame(table(new_data$`S-C&Waste 2`))
as.data.frame(table(new_data$`S-C&Waste 3`))


# count Greenspace, decision-making and equitable engagement climate actions
as.data.frame(table(new_data$`Greenspace&D-M&E-E 1`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 2`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 3`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 4`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 5`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 6`))
as.data.frame(table(new_data$`Greenspace&D-M&E-E 7`))


# count Demographics of the survey participants
as.data.frame(table(new_data$Age))
as.data.frame(table(new_data$Gender))
as.data.frame(table(new_data$Housing))
as.data.frame(table(new_data$Income))

# create figures
fig.cap="Demographics of the survey participants-Age"
ggplot(data = new_data, aes(x = Age)) +
  ggtitle("Demographics of the survey participants-Age") +
  geom_bar(alpha = 0.8, color="orange", fill="steelblue")


fig.cap="Demographics of the survey participants-Gender"
ggplot(data = new_data, aes(x = Gender)) +
  ggtitle("Demographics of the survey participants-Gender") +
  geom_bar(alpha = 0.8, color="black", fill="plum4")+
  coord_flip()


fig.cap="Demographics of the survey participants-Income (year)"
ggplot(data = new_data, aes(x = Income)) +
  ggtitle("Demographics of the survey participants-Income") +
  geom_bar(alpha = 0.8, color="lightsalmon2", fill="olivedrab4") +
  coord_flip()


fig.cap="Demographics of the survey participants-Housing Situation"
ggplot(data = new_data, aes(x = Housing)) +
  ggtitle("Demographics of the survey participants-Housing?") +
  geom_bar(alpha = 0.8, color="olivedrab3", fill="tan1")+
  coord_flip()


fig.cap="Highest priority climate actions-Buildings"
b = c(710,405,257,411,737,364)

barplot(b,
        main="Buildings priority climate actions",
        col=c("lightblue3"),
        names.arg=c("Bs 1","Bs 2","Bs 3","Bs 4","Bs 5","Bs 6"),
        ylab = "count",
        xlab = "climate actions - buidlings",
        ylim = c(0,810)
)
text(0.73,750,"710",col=("red"))
text(1.9,445,"405",col=("orange2"))
text(3.07,297,"257",col=("orange2"))
text(4.24,451,"411",col=("orange2"))
text(5.49,787,"737",col=("red"))
text(6.7,404,"364",col=("orange2"))



fig.cap="Highest priority climate actions-Energy"
e = c(404,684,512,436,565)

barplot(e,
        main="Energy priority climate actions",
        col=c("mistyrose2"),
        names.arg=c("Energy 1","Energy 2","Energy 3","Energy 4","Energy 5"),
        ylab = "count",
        xlab = "climate actions - energy",
        ylim = c(0,770)
)
text(0.73,444,"404",col=("black"))
text(1.9,724,"684",col=("red"))
text(3.07,552,"512",col=("black"))
text(4.24,476,"436",col=("black"))
text(5.49,605,"565",col=("red"))



fig.cap="Highest priority climate actions-Transportation"
t = c(303,380,367,629,642,423)

barplot(t,
        main="Transportation priority climate actions",
        col=c("navajowhite1"),
        names.arg=c("Tr 1",
                    "Tr 2",
                    "Tr 3",
                    "Tr 4",
                    "Tr 5",
                    "Tr 6"),
        ylab = "count",
        xlab = "climate actions - transportation",
        ylim = c(0,770)
)
text(0.73,343,"303",col=("steelblue4"))
text(1.9,420,"380",col=("steelblue4"))
text(3.07,407,"367",col=("steelblue4"))
text(4.24,679,"629",col=("red"))
text(5.49,682,"642",col=("red"))
text(6.7,463,"423",col=("steelblue4"))



fig.cap="Highest priority climate actions-Sustainable Consumption & Waste"
s = c(370,743,384)

barplot(s,
        main="Sustainable Consumption & Waste priority climate actions",
        col=c("lightsalmon2"),
        names.arg=c("S-C&Waste 1",
                    "S-C&Waste 2",
                    "S-C&Waste 3"),
        ylab = "count",
        xlab = "climate actions - sustainable consumption & waste",
        ylim = c(0,850)
)
text(0.73,410,"370",col=("blue"))
text(1.9,783,"743",col=("red"))
text(3.07,424,"384",col=("blue"))


fig.cap="Highest priority climate actions-Greenspace, Decision-Making and Equitable Engagement"
t = c(676,597,628,302,408,242,246)

barplot(t,
        main="Greenspace, Decision-Making and Equitable Engagement priority \n
    climate actions",
        col=c("lavender"),
        names.arg=c("GDE 1",
                    "GDE 2",
                    "GDE 3",
                    "GDE 4",
                    "GDE 5",
                    "GDE 6",
                    "GDE 7"),
        ylab = "count",
        xlab = "climate actions - greenspace, decision-making and equitable engagement",
        ylim = c(0,770)
)
text(0.73,716,"676",col=("red"))
text(1.9,637,"597",col=("red"))
text(3.07,668,"628",col=("red"))
text(4.24,342,"302",col=("steelblue4"))
text(5.49,448,"408",col=("steelblue4"))
text(6.7,282,"242",col=("steelblue4"))
text(7.87,286,"246",col=("steelblue4"))


# appendix information
fig.align="left", out.width = '100%'
knitr::include_graphics("01.jpg")
knitr::include_graphics("02.jpg")
knitr::include_graphics("03.jpg")
knitr::include_graphics("04.jpg")
knitr::include_graphics("05.jpg")


         