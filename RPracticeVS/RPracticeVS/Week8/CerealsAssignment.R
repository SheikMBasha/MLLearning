rm(list = ls(all = T))

getwd()
setwd('E://Insofe//Week8//Day1//Lab')

cerealsRaw <- read.csv('Cereals2.csv', header = T, sep = ",")